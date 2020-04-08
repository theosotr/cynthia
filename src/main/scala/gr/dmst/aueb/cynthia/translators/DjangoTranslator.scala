package gr.dmst.aueb.cynthia.translators

import gr.dmst.aueb.cynthia._


case class DjangoTranslator(t: Target) extends Translator(t) {
  private val hidden: scala.collection.mutable.Set[String] = scala.collection.mutable.Set()

  override val preamble =
   s"""import os, django
   |from django.db.models import *
   |os.environ.setdefault('DJANGO_SETTINGS_MODULE', 'djangoproject.settings')
   |django.setup()
   |
   |from ${target.orm.projectName}.models import *
   |import numbers, decimal
   |
   |def dump(x):
   |    if isinstance(x, numbers.Number):
   |        print(round(decimal.Decimal(x), 2))
   |    else:
   |        print(x)
   |""".stripMargin

  def getDjangoFieldName(field: String) =
    field.split('.').toList match {
      case Nil | _ :: Nil => field
      case _ :: t         => t mkString "__"
    }

  override def emitPrint(q: Query, dfields: Seq[String], ret: String) = {
    def _dumpField(v: String, fields: Iterable[String], ident: String = "") =
      fields map { as =>
        s"""
        |${ident}if(isinstance($v, dict)):
        |${ident}    dump($v['$as'])
        |${ident}else:
        |${ident}    dump($v.$as)""".stripMargin
      } mkString "\n"
    q match {
      case SetRes (_) | SubsetRes(_, _, _) =>
        s"for r in $ret:\n${_dumpField("r", dfields, ident = " " * 4)}"
      case FirstRes(_) => _dumpField(ret, dfields)
      case AggrRes (aggrs, _) => {
        val aggrF = TUtils.mapNonHiddenFields(
          aggrs, { case FieldDecl(_, as, _, _) => as })
        _dumpField(ret, aggrF)
      }
    }
  }

  def constructFilter(preds: Set[Predicate]) =
    preds map { x =>
      (Str("filter(") << translatePred(x) << ")").!
    } mkString(".")

  def constructOrderBy(spec: Seq[(String, Order)]) = spec match {
    case Seq() => ""
    case _     =>
      (
        Str("order_by(") << (
          spec map { x =>
            x match {
              case (k, Desc) => Utils.quoteStr("-" + getDjangoFieldName(k))
              case (k, Asc)  => Utils.quoteStr(getDjangoFieldName(k))
            }
          } mkString(",")
        ) << ")"
      ).!
  }

  def getType(ftype: FieldType) = ftype match {
    case StringF   => "TextField()"
    case IntF      => "IntegerField()"
    case DoubleF   => "FloatField()"
    case BooleanF  => "BooleanField()"
    case DateTimeF => "DateTimeField()"
  }

  def constructQueryPrefix(s: State) =  s.query match {
    case None =>
      s.sources.toList match {
        case Nil | _ :: _ :: _ => ???
        case h :: Nil =>
          val dbname = s.db match {
            case Postgres (_, _, _) => "postgres"
            case MySQL (_, _, _)    => "mysql"
            case SQLite (_)         => "default"
          }
          QueryStr(Some("ret" + s.numGen.next().toString),
                   Some(h + ".objects.using('" + dbname + "')"))
      }
    case Some(q) => q
  }

  def constructPrimAggr(fexpr: FieldExpr, fieldType: String) = {
    val (field, op) = fexpr match {
      case Count(None)        => ("'*'", "Count")
      case Count(Some(field)) => (constructFieldExpr(field), "Count")
      case Sum(field)         => (constructFieldExpr(field), "Sum")
      case Avg(field)         => (constructFieldExpr(field), "Avg")
      case Min(field)         => (constructFieldExpr(field), "Min")
      case Max(field)         => (constructFieldExpr(field), "Max")
      case _                  => ??? // Unreachable case
    }
    op + "(" + field + ", output_field=" + fieldType + ")"
  }

  def constructFieldExpr(fexpr: FieldExpr,
      fieldType: String = "FloatField()"): String = fexpr match {
    case F(f) =>
      if (this.hidden contains f)
        // Hidden fields are not annotated so just simply return `f`
        // corresponding to the variable holding the definition of hidden field.
        f
      else
        "F(" + Utils.quoteStr(getDjangoFieldName(f)) + ")"
    case Add(a1, a2) => "(" + constructFieldExpr(a1) + " + " + constructFieldExpr(a2) + ")"
    case Sub(a1, a2) => "(" + constructFieldExpr(a1) + " - " + constructFieldExpr(a2) + ")"
    case Mul(a1, a2) => "(" + constructFieldExpr(a1) + " * " + constructFieldExpr(a2) + ")"
    case Div(a1, a2) => "(" + constructFieldExpr(a1) + " / " + constructFieldExpr(a2) + ")"
    case Constant(v, UnQuoted) => "Value(" + v + ")"
    case Constant(v, Quoted)   => "Value(" + Utils.quoteStr(v) + ")"
    case _           => constructPrimAggr(fexpr, fieldType)
  }

  def constructAggrs(aggrs: Seq[FieldDecl]) = aggrs match {
    case Seq() => ""
    case _     => {
      val fields = TUtils.mapNonHiddenFields(aggrs, { case FieldDecl(f, as, t, _) =>
        (Str(as) << "=ExpressionWrapper(" << constructFieldExpr(f, fieldType = getType(t)) <<
          ", output_field=" << getType(t) << ")").!
      })
      "aggregate(" + (fields mkString ",") + ")"
    }
  }

  def constructFirst(first: Boolean) =
    if (first) "first()"
    else ""

  def constructOffsetLimit(offset: Int, limit: Option[Int]) = limit match {
    case None =>
      if (offset > 0) s"[$offset:]"
      else ""
    case Some(limit) =>
      if (offset > 0) s"[$offset:$offset + $limit]"
      else s"[:$limit]"
  }

  def constructAnnotate(fields: Iterable[FieldDecl]) =
    if (fields.isEmpty) ""
    else {
      val anFields = TUtils.mapNonHiddenFields(fields, { case FieldDecl(_, as, _, _) =>
        as + "=" + as
      })
      "annotate(" + (anFields mkString ", ") + ")"
    }

  def constructGroupBy(groupBy: Seq[String]) = groupBy match {
    case Seq() => ""
    case _     => "values(" + (
      groupBy map { x => Utils.quoteStr(getDjangoFieldName(x)) } mkString ", ") + ")"
  }

  def constructFieldDecls(fields: Iterable[FieldDecl]) =
    if (fields.isEmpty) QueryStr()
    else {
      fields.foldLeft(QueryStr()) { case (acc, FieldDecl(f, as, t, _)) => {
        val str = Str("ExpressionWrapper(") << constructFieldExpr(f) <<
          ", output_field=" << getType(t) << ")"
        acc >> QueryStr(Some(as), Some(str.!))
      }
    }
  }

  override def constructQuery(first: Boolean = false, offset: Int = 0,
      limit: Option[Int] = None)(s: State) = {
    this.hidden ++= TUtils.mapHiddenFields(
      s.fields.values, { case FieldDecl(_, as, _, _) => as })
    val qStr = constructFieldDecls(s.fields.values) >> constructQueryPrefix(s)
    val (aggrF, nonAggrF) = s.fields.values partition { case FieldDecl(f, _, _, _) => f.isAggregate }
    val (aggrP, nonAggrP) = s.preds partition { _.hasAggregate(s.fields) }
    qStr >> QueryStr(Some("ret" + s.numGen.next().toString),
      Some((Seq(
        qStr.ret.get,
        constructAnnotate(nonAggrF),
        constructFilter(nonAggrP),
        constructGroupBy(s.groupBy),
        constructAnnotate(aggrF),
        constructOrderBy(s.orders),
        constructFilter(aggrP),
        constructAggrs(s.aggrs),
        constructFirst(first)
      ) filter {
        case "" => false
        case _  => true
      }  mkString (".")) + constructOffsetLimit(offset, limit))
    )
  }

  override def unionQueries(s1: State, s2: State) = {
    val (q1, q2) = (constructQuery()(s1), constructQuery()(s2))
    s1 >> (q1 << q2 >> QueryStr(Some("ret" + s1.numGen.next().toString),
                                Some(q1.ret.get + ".union(" + q2.ret.get + ")")))
  }

  override def intersectQueries(s1: State, s2: State) = {
    val (q1, q2) = (constructQuery()(s1), constructQuery()(s2))
    s1 >> (q1 << q2 >> QueryStr(Some("ret" + s1.numGen.next().toString),
                                Some(q1.ret.get + ".intersect(" + q2.ret.get + ")")))
  }

  def translatePred(pred: Predicate): String = pred match {
    case Eq(k, e) =>
      (Str(getDjangoFieldName(k)) << "=" << constructFieldExpr(e)).!
    case Gt(k, e) =>
      (Str(getDjangoFieldName(k)) << "__gt=" << constructFieldExpr(e)).!
    case Gte(k, e) =>
      (Str(getDjangoFieldName(k)) << "__gte=" << constructFieldExpr(e)).!
    case Lt(k, e)  =>
      (Str(getDjangoFieldName(k)) << "__le=" << constructFieldExpr(e)).!
    case Lte(k, e) =>
      (Str(getDjangoFieldName(k)) << "__lte=" << constructFieldExpr(e)).!
    case Contains(k, e)             =>
      (Str(getDjangoFieldName(k)) << "__contains=" << constructFieldExpr(e)).!
    case Not(pred)                  =>
      (Str("~Q(") << translatePred(pred) << ")").!
    case Or(p1, p2)                 =>
      (Str("Q(") << translatePred(p1) << ") | Q(" << translatePred(p2) << ")").!
    case And(p1, p2)                =>
      (Str("Q(") << translatePred(p1) << "), Q(" << translatePred(p2) << ")").!
  }
}
