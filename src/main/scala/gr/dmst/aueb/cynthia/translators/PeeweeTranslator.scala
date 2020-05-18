package gr.dmst.aueb.cynthia.translators

import gr.dmst.aueb.cynthia._


case class PeeweeTranslator(t: Target) extends Translator(t) {

  override val preamble =
    s"""import numbers, decimal
    |from peewee import *
    |from models_${target.db.getName} import *
    |
    |def dump(x):
    |    if isinstance(x, numbers.Number):
    |        print(round(decimal.Decimal(x), 2))
    |    else:
    |        print(x)
    |""".stripMargin

  override def emitPrint(q: Query, dFields: Seq[String], ret: String) = {
    def _dumpField(v: String, fields: Iterable[String], ident: String = "") =
      fields map { as => s"${ident}dump($v.$as)" } mkString "\n"
    q match {
      case SetRes(_) | SubsetRes(_, _, _) =>
        s"for r in $ret:\n${_dumpField("r", dFields, ident = " " * 4)}"
      case FirstRes(_) => _dumpField(ret, dFields)
      case AggrRes(aggrs, _) => aggrs match {
        case Seq(FieldDecl(Count(_), _, _, _)) => s"dump($ret)"
        case _ => {
          val aggrF = TUtils.mapNonHiddenFields(aggrs, FieldDecl.as)
          _dumpField(ret, aggrF)
        }
      }
    }
  }

  def getPeeweeFieldName(field: String) = {
    def _getPeeweeFieldName(segs: List[String]): String = segs match {
      case Nil | _ :: Nil | _ :: (_ :: Nil) => segs mkString "."
      case _ :: (h :: t) => _getPeeweeFieldName(h.capitalize :: t)
    }
    _getPeeweeFieldName(field.split('.').toList)
  }

  def translatePred(pred: Predicate): String = pred match {
    case Eq(k, e) =>
      (Str(getPeeweeFieldName(k)) << "==" << constructFieldExpr(e)).!
    case Gt(k, e) =>
      (Str(getPeeweeFieldName(k)) << " > " << constructFieldExpr(e)).!
    case Gte(k, e) =>
      (Str(getPeeweeFieldName(k)) << " >= " << constructFieldExpr(e)).!
    case Lt(k, e) =>
      (Str(getPeeweeFieldName(k)) << " < " << constructFieldExpr(e)).!
    case Lte(k, e) =>
      (Str(getPeeweeFieldName(k)) << " <= " << constructFieldExpr(e)).!
    case Contains(k, e) =>
      (Str(getPeeweeFieldName(k)) << ".contains(" << constructFieldExpr(e) << ")").!
    case Not(pred)                  =>
      (Str("~(") << translatePred(pred) << ")").!
    case Or(p1, p2)                 =>
      (Str("(") << translatePred(p1) << ") | (" << translatePred(p2) << ")").!
    case And(p1, p2)                =>
      (Str("(") << translatePred(p1) << ") & (" << translatePred(p2) << ")").!
  }

  def constructFilter(preds: Set[Predicate], having: Boolean = false) =
    if (having) {
      preds map { x =>
        (Str("having(") << translatePred(x) << ")").!
      } mkString(".")
    } else {
      preds map { x =>
        (Str("where(") << translatePred(x) << ")").!
      } mkString(".")
    }

  def constructOrderBy(spec: Seq[(String, Order)]) = spec match {
    case Seq() => ""
    case _     =>
      (
        Str("order_by(") << (
          spec map { x =>
            x match {
              case (k, Desc) => getPeeweeFieldName(k) + ".desc()"
              case (k, Asc)  => getPeeweeFieldName(k) + ".asc()"
            }
          } mkString ","
        ) << ")"
      ).!
  }

  def constructPrimAggr(fexpr: FieldExpr) = {
    val (field, op) = fexpr match {
      case Count(None)        => ("", "fn.count")
      case Count(Some(field)) => (constructFieldExpr(field), "fn.count")
      case Sum(field)         => (constructFieldExpr(field), "fn.sum")
      case Avg(field)         => (constructFieldExpr(field), "fn.avg")
      case Min(field)         => (constructFieldExpr(field), "fn.min")
      case Max(field)         => (constructFieldExpr(field), "fn.max")
      case _                  => ??? // Unreachable case
    }
    op + "(" + field + ")"
  }

  def constructCompoundAggr(fexpr: FieldExpr) = {
    val (a1, a2, op) = fexpr match {
      case Add(a1, a2) => (a1, a2, " + ")
      case Sub(a1, a2) => (a1, a2, " - ")
      case Mul(a1, a2) => (a1, a2, " * ")
      case Div(a1, a2) => (a1, a2, " / ")
      case _           => ??? // Unreachable case
    }
    val str = Str("(") << constructFieldExpr(a1) << op << constructFieldExpr(a2) << ")"
    str.!
  }

  def constructFieldExpr(fexpr: FieldExpr): String = fexpr match {
    case F(f)                  => getPeeweeFieldName(f)
    case Constant(v, UnQuoted) => s"Value($v)"
    case Constant(v, Quoted)   => s"Value(${Utils.quoteStr(v)})"
    case _    =>
      if (!fexpr.compound) constructPrimAggr(fexpr)
      else constructCompoundAggr(fexpr)
  }

  def constructJoins(joins: Set[(String, String)]): String =
    joins map { case (_, y) =>
      "join(" + y + ")"
    } mkString (".")

  def constructQueryPrefix(s: State) =  s.query match {
    case None =>
      s.sources.toList match {
        case Nil => ??? // Unreachable case
        case h :: _   => s.aggrs match {
          case Seq() | Seq(FieldDecl(Count(_), _, _, _)) => {
            val dFields = TUtils.mapNonHiddenFields(
              s.fields.values, FieldDecl.as)
            val fieldStr = dFields mkString ","
            val q =
              if (fieldStr.equals(""))
                s"$h.select()"
              else
                s"$h.select($fieldStr)"
            QueryStr(
              Some("ret" + s.numGen.next().toString),
              Some(q)
            )
          }
          case _ => {
            val aggrs = TUtils.mapNonHiddenFields(s.aggrs, {
              case FieldDecl(f, l, t, _) =>
                s"(${constructFieldExpr(f)}).cast(${getType(t)}).alias('$l')"
            })
            val qstr = s"$h.select(${(aggrs mkString ", ")})"
            QueryStr(
              Some("ret" + s.numGen.next().toString),
              Some(qstr)
            )
          }
        }
      }
    case Some(q) => q
  }

  def constructFirst(first: Boolean) =
    if (first) "first()"
    else ""

  def constructOffsetLimit(offset: Int, limit: Option[Int]) = limit match {
    case None =>
      if (offset > 0) s"offset($offset)"
      else ""
    case Some(limit) =>
      if (offset > 0)
        RUtils.chooseFrom(Seq(
          s"offset($offset).limit($limit)",
          s"slice($offset, $offset + $limit)")
        )
      else s"limit($limit)"
  }

  def getType(ftype: FieldType) = ftype match {
    case StringF   => "'varchar'"
    case IntF      => "'signed'"
    case DoubleF   => "'float'"
    case BooleanF  => "'boolean'"
    case DateTimeF => "'datetime'"
  }

  def constructFieldDecls(fields: Iterable[FieldDecl]) =
    if (fields.isEmpty) QueryStr()
    else
      fields.foldLeft(QueryStr()) { case (acc, FieldDecl(f, as, t, _)) => {
        val str = Str("(") << constructFieldExpr(f) << ").cast(" <<
          getType(t) << ").alias(" << Utils.quoteStr(as) << ")"
        acc >> QueryStr(Some(as), Some(str.!))
      }
    }

  def constructGroupBy(groupBy: Seq[String]) = groupBy match {
    case Seq() => ""
    case _     =>
      "group_by(" + (
        groupBy map { getPeeweeFieldName } mkString ", ") + ")"
  }

  override def constructQuery(first: Boolean = false, offset: Int = 0,
      limit: Option[Int] = None)(s: State) = {
    val fieldVals = s.fields.values
    val nonAggrHidden = TUtils.filterNonAggrHidden(fieldVals).toSeq
    val qStr = constructFieldDecls(fieldVals) >> constructQueryPrefix(s)
    val (aggrP, nonAggrP) = s.preds partition { _.hasAggregate(s.fields) }
    qStr >> QueryStr(Some("ret" + s.numGen.next().toString),
      Some(Seq(
        qStr.ret.get,
        constructJoins(s.joins),
        constructFilter(nonAggrP),
        if (s.groupBy) constructGroupBy(nonAggrHidden) else "",
        constructFilter(aggrP, having = true),
        constructOrderBy(s.orders),
        s.aggrs match {
          case Seq() => ""
          case Seq(FieldDecl(Count(_), _, _, _)) => "count()"
          case _ => "first()"
        },
        constructFirst(first),
        constructOffsetLimit(offset, limit)
      ) filter {
        case "" => false
        case _  => true
      }  mkString("."))
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
}
