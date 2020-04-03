package gr.dmst.aueb.cynthia.translators

import gr.dmst.aueb.cynthia._


case class SQLAlchemyTranslator(t: Target) extends Translator(t) {

  override val preamble =
    s"""from sqlalchemy import create_engine, or_, and_, not_, func, cast, types
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
      case Seq(FieldDecl(Count(_), _, _)) => s"dump($ret)"
      case _ => aggrs map { case FieldDecl(_, as, _) =>
        s"dump($ret.$as)"
      } mkString ("\n")
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

  def constructPrimAggr(fexpr: FieldExpr) = {
    val (field, op) = fexpr match {
      case Count(None)        => ("", "func.count")
      case Count(Some(field)) => (constructFieldExpr(field), "func.count")
      case Sum(field)         => (constructFieldExpr(field), "func.sum")
      case Avg(field)         => (constructFieldExpr(field), "func.avg")
      case Min(field)         => (constructFieldExpr(field), "func.min")
      case Max(field)         => (constructFieldExpr(field), "func.max")
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
    case F(f) => getSQLAlchemyFieldName(f)
    case _    =>
      if (!fexpr.compound) constructPrimAggr(fexpr)
      else constructCompoundAggr(fexpr)
  }

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
          case Seq() | Seq(FieldDecl(Count(_), _, _)) => QueryStr(
            Some("ret" + s.numGen.next().toString),
            Some("session.query(" + s.sources.mkString(",") + ")"))
          case _ => QueryStr(
            Some("ret" + s.numGen.next().toString),
            Some("session.query(" + (s.aggrs map { case FieldDecl(f, l, t) =>
              s"cast(${constructFieldExpr(f)}, ${getType(t)}).label('$l')"
            } mkString ",") + ")"))
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
    case StringF   => "types.String"
    case IntF      => "types.Integer"
    case DoubleF   => "types.Float"
    case BooleanF  => "types.Boolean"
    case DateTimeF => "types.DateTime"
  }

  def constructFieldDecls(fields: Set[FieldDecl]) =
    if (fields.isEmpty) QueryStr()
    else
      fields.foldLeft(QueryStr()) { case (acc, FieldDecl(f, as, t)) =>
        acc >> QueryStr(
          Some(as),
          Some("cast(" + constructFieldExpr(f) + ", " + getType(t) + ")")
        )
      }

  override def constructQuery(first: Boolean = false, offset: Int = 0,
      limit: Option[Int] = None)(s: State) = {
    val qStr = constructFieldDecls(s.fields) >> constructQueryPrefix(s)
    qStr >> QueryStr(Some("ret" + s.numGen.next().toString),
      Some(Seq(
        qStr.ret.get,
        constructJoins(s.joins),
        constructFilter(s.preds),
        constructOrderBy(s.orders),
        s.aggrs match {
          case Seq() => ""
          case Seq(FieldDecl(Count(_), _, _)) => "count()"
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
