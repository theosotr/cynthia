package gr.dmst.aueb.cynthia.translators

import gr.dmst.aueb.cynthia._


case class SQLAlchemyTranslator(t: Target) extends Translator(t) {

  override val preamble =
    s"""from sqlalchemy import create_engine, or_, and_, not_, func
    |from sqlalchemy.orm import sessionmaker
    |from models import *
    |import numbers, decimal
    |
    |engine = create_engine('${target.db.getURI()}')
    |Session = sessionmaker(bind=engine)
    |session = Session()

    |def dump(x):
    |    if isinstance(x, numbers.Number):
    |        print(round(decimal.Decimal(x), 2))
    |    else:
    |        print(x)
    |""".stripMargin

  override def emitPrint(q: Query, ret: String) = q match {
    case SetRes(_) | SubsetRes(_, _, _) => s"for r in $ret:\n  dump(r.id)"
    case FirstRes(_) => s"dump($ret.id)"
    case AggrRes(aggrs, _) => aggrs match {
      case Seq(Count(_)) => s"dump($ret)"
      case _ => aggrs map { x => {
        val label = x.label match {
          case None => throw new Exception(
            "You must provide a label for root aggregates")
          case Some(l) => l
        }
        s"dump($ret.$label)"
        }} mkString ("\n")
      }
    }

  def getSQLAlchemyFieldName(field: String) = {
    def _getSQLAlchemyFieldName(segs: List[String]): String = segs match {
      case Nil | _ :: Nil | _ :: (_ :: Nil) => segs mkString "."
      case _ :: (h :: t) => _getSQLAlchemyFieldName(h.capitalize :: t)
    }
    _getSQLAlchemyFieldName(field.split('.').toList)
  }

  def translatePred(pred: Predicate): String = pred match {
    case Eq(k, Value(v, Quoted)) =>
      (Str(getSQLAlchemyFieldName(k)) << "==" << Utils.quoteStr(v)).!
    case Eq(k, Value(v, UnQuoted)) =>
      (Str(getSQLAlchemyFieldName(k)) << "==" << v).!
    case Gt(k, Value(v, Quoted)) =>
      (Str(getSQLAlchemyFieldName(k)) << " > " << Utils.quoteStr(v)).!
    case Gt(k, Value(v, UnQuoted)) =>
      (Str(getSQLAlchemyFieldName(k)) << " > " << v).!
    case Gte(k, Value(v, Quoted)) =>
      (Str(getSQLAlchemyFieldName(k)) << " >= " << Utils.quoteStr(v)).!
    case Gte(k, Value(v, UnQuoted)) =>
      (Str(getSQLAlchemyFieldName(k)) << " >= " << v).!
    case Lt(k, Value(v, Quoted)) =>
      (Str(getSQLAlchemyFieldName(k)) << " < " << Utils.quoteStr(v)).!
    case Lt(k, Value(v, UnQuoted)) =>
      (Str(getSQLAlchemyFieldName(k)) << " < " << v).!
    case Lte(k, Value(v, Quoted)) =>
      (Str(getSQLAlchemyFieldName(k)) << " <= " << Utils.quoteStr(v)).!
    case Lte(k, Value(v, UnQuoted)) =>
      (Str(getSQLAlchemyFieldName(k)) << " <= " << v).!
    case Contains(k, v) =>
      (Str(getSQLAlchemyFieldName(k)) << ".contains(" << Utils.quoteStr(v) << ")").!
    case Not(pred)                  =>
      (Str("not_(") << translatePred(pred) << ")").!
    case Or(p1, p2)                 =>
      (Str("or_(") << translatePred(p1) << ", " << translatePred(p2) << ")").!
    case And(p1, p2)                =>
      (Str("and_(") << translatePred(p1) << ", " << translatePred(p2) << ")").!
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
              case (k, Desc) => getSQLAlchemyFieldName(k) + ".desc()"
              case (k, Asc)  => getSQLAlchemyFieldName(k) + ".asc()"
            }
          } mkString ","
        ) << ")"
      ).!
  }

  def constructPrimAggr(aggr: Aggregate) = {
    val (field, op, label) = aggr match {
      case Count(l)      => ("", "func.count", l)
      case Sum(field, l) => (field, "func.sum", l)
      case Avg(field, l) => (field, "func.avg", l)
      case Min(field, l) => (field, "func.min", l)
      case Max(field, l) => (field, "func.max", l)
      case _             => ??? // Unreachable case
    }
    label match {
      case None    => op + "(" + field + ")"
      case Some(l) => op + "(" + field + ").label(" + Utils.quoteStr(l) + ")" 
    }
  }

  def constructCompoundAggr(aggr: Aggregate) = {
    val (a1, a2, op, l) = aggr match {
      case Add(a1, a2, l) => (a1, a2, " + ", l)
      case Sub(a1, a2, l) => (a1, a2, " - ", l)
      case Mul(a1, a2, l) => (a1, a2, " * ", l)
      case Div(a1, a2, l) => (a1, a2, " / ", l)
      case _ => ??? // Unreachable case
    }
    val str = Str("(") << constructAggr(a1) << op << constructAggr(a2) << ")"
    l match {
      case None    => str.!
      case Some(l) => (str << ".label(" << Utils.quoteStr(l) << ")").!
    }
  }

  def constructAggr(aggr: Aggregate): String =
    if (!aggr.compound) constructPrimAggr(aggr)
    else constructCompoundAggr(aggr)

  def toField(x: String, y: String) = {
    val char = Character.toLowerCase(y.charAt(0))
    val str = s"${char}${y.substring(1)}"
    x + "." + str
  }

  def constructJoins(joins: Set[(String, String)]): String =
    joins map { case (x, y) =>
      "join(" + toField(x, y) + ")"
    } mkString (".")

  def constructQueryPrefix(s: State) =  s.query match {
    case None =>
      s.sources.toList match {
        case Nil => ??? // Unreachable case
        case _   => s.aggrs match {
          case Seq() | Seq(Count(_)) => QueryStr(
            Some("ret" + s.numGen.next().toString),
            Some("session.query(" + s.sources.mkString(",") + ")"))
          case _ => QueryStr(
            Some("ret" + s.numGen.next().toString),
            Some("session.query(" + (s.aggrs map { constructAggr } mkString ",") + ")"))
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

  override def constructQuery(first: Boolean = false, offset: Int = 0,
      limit: Option[Int] = None)(s: State) = {
    val qStr = constructQueryPrefix(s)
    qStr >> QueryStr(Some("ret" + s.numGen.next().toString),
      Some(Seq(
        qStr.ret.get,
        constructJoins(s.joins),
        constructFilter(s.preds),
        constructOrderBy(s.orders),
        s.aggrs match {
          case Seq()         => ""
          case Seq(Count(_)) => "count()"
          case _             => "first()"
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
