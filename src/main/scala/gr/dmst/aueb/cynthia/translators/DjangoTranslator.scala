package gr.dmst.aueb.cynthia.translators

import gr.dmst.aueb.cynthia._


case class DjangoTranslator(t: Target) extends Translator(t) {

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
      case _ :: t         => t.mkString("__")
    }

  override def emitPrint(q: Query, ret: String) = q match {
    case SetRes (_) | SubsetRes(_, _, _) => s"for r in $ret:\n  dump(r.id)"
    case FirstRes(_) => s"dump($ret.id)"
    case AggrRes (aggrs, _) => aggrs map { x => {
        val label = x.label match {
          case None => throw new Exception(
            "You must provide a label for root aggregates")
          case Some(l) => l
        }
        s"dump($ret['$label'])"
      }
    } mkString ("\n")
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

  def constructPrimAggr(aggr: Aggregate) = {
    val (field, op, label) = aggr match {
      case Count(l)      => ("*", "Count", l)
      case Sum(field, l) => (field, "Sum", l)
      case Avg(field, l) => (field, "Avg", l)
      case Min(field, l) => (field, "Min", l)
      case Max(field, l) => (field, "Max", l)
      case _             => ??? // Unreachable case
    }
    val k = Utils.quoteStr(getDjangoFieldName(field))
    label match {
      case None    => op + "(" + k + ", output_field=FloatField())"
      case Some(l) => l + "=" + op + "(" + k + ", output_field=TextField())"
    }
  }

  def constructAggr(aggr: Aggregate): String = aggr match {
    case Add(a1, a2, None)    => "(" + constructAggr(a1) + " + " + constructAggr(a2) + ")"
    case Add(a1, a2, Some(l)) => l + "=" + constructAggr(a1) + " + " + constructAggr(a2)
    case Sub(a1, a2, None)    => "(" + constructAggr(a1) + " - " + constructAggr(a2) + ")"
    case Sub(a1, a2, Some(l)) => l + "=" + constructAggr(a1) + " - " + constructAggr(a2)
    case Mul(a1, a2, None)    => "(" + constructAggr(a1) + " * " + constructAggr(a2) + ")"
    case Mul(a1, a2, Some(l)) => l + "=" + constructAggr(a1) + " * " + constructAggr(a2)
    case Div(a1, a2, None)    => "(" + constructAggr(a1) + " / " + constructAggr(a2) + ")"
    case Div(a1, a2, Some(l)) => l + "=" + constructAggr(a1) + " / " + constructAggr(a2)
    case _                    => constructPrimAggr(aggr)
  }

  def constructAggrs(aggrs: Seq[Aggregate]) = aggrs match {
    case Seq() => ""
    case _     => "aggregate(" + (aggrs map { constructAggr } mkString (",")) + ")"
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

  override def constructQuery(first: Boolean = false, offset: Int = 0,
      limit: Option[Int] = None)(s: State) = {
    val qStr = constructQueryPrefix(s)
    qStr >> QueryStr(Some("ret" + s.numGen.next().toString),
      Some((Seq(
        qStr.ret.get,
        constructFilter(s.preds),
        constructOrderBy(s.orders),
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
    case Eq(k, Value(v, Quoted))    =>
      (Str(getDjangoFieldName(k)) << "=" << Utils.quoteStr(v)).!
    case Eq(k, Value(v, UnQuoted))  =>
      (Str(getDjangoFieldName(k)) << "=" << v).!
    case Gt(k, Value(v, Quoted))    =>
      (Str(getDjangoFieldName(k)) << "__gt=" << Utils.quoteStr(v)).!
    case Gt(k, Value(v, UnQuoted))  =>
      (Str(getDjangoFieldName(k)) << "__gt=" << v).!
    case Gte(k, Value(v, Quoted))   =>
      (Str(getDjangoFieldName(k)) << "__gte=" << Utils.quoteStr(v)).!
    case Gte(k, Value(v, UnQuoted)) =>
      (Str(getDjangoFieldName(k)) << "__gte=" << v).!
    case Lt(k, Value(v, Quoted))    =>
      (Str(getDjangoFieldName(k)) << "__lt=" << Utils.quoteStr(v)).!
    case Lt(k, Value(v, UnQuoted))  =>
      (Str(getDjangoFieldName(k)) << "__le=" << v).!
    case Lte(k, Value(v, Quoted))   =>
      (Str(getDjangoFieldName(k)) << "__lte=" << Utils.quoteStr(v)).!
    case Lte(k, Value(v, UnQuoted)) =>
      (Str(getDjangoFieldName(k)) << "__lte=" << v).!
    case Contains(k, v)             =>
      (Str(getDjangoFieldName(k)) << "__contains=" << Utils.quoteStr(v)).!
    case Not(pred)                  =>
      (Str("~Q(") << translatePred(pred) << ")").!
    case Or(p1, p2)                 =>
      (Str("Q(") << translatePred(p1) << ") | Q(" << translatePred(p2) << ")").!
    case And(p1, p2)                =>
      (Str("Q(") << translatePred(p1) << "), Q(" << translatePred(p2) << ")").!
  }
}
