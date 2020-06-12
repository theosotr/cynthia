package gr.dmst.aueb.cynthia.translators

import gr.dmst.aueb.cynthia._


case class SQLAlchemyTranslator(t: Target) extends Translator(t) {

  override val preamble =
    s"""from sqlalchemy import (create_engine, or_, and_, not_, func,
    |     type_coerce, types, literal, asc, desc)
    |from sqlalchemy.orm import sessionmaker
    |from sqlalchemy.exc import SAWarning
    |from models import *
    |import numbers, decimal, warnings
    |
    |# Ignore SQLAlchemy warnings
    |warnings.simplefilter("ignore", category=SAWarning)
    |
    |engine = create_engine('${target.db.getURI()}')
    |Session = sessionmaker(bind=engine)
    |session = Session()

    |def dump(x):
    |    if isinstance(x, numbers.Number):
    |        print(round(decimal.Decimal(x), 2))
    |    else:
    |        try:
    |            print(round(decimal.Decimal(float(x)), 2))
    |        except:
    |            if type(x) is bytes:
    |                print(str(x.decode('utf-8')))
    |            else:
    |                print(x)
    |""".stripMargin

  override def emitPrint(q: Query, dFields: Seq[String], ret: String) = {
    def _dumpField(v: String, fields: Iterable[String], ident: String = "") =
      fields map { as => s"${ident}dump($v.$as)" } mkString "\n"
    q match {
      case SetRes(_) | SubsetRes(_, _, _) =>
        s"for r in $ret:\n${_dumpField("r", dFields, ident = " " * 4)}"
      case FirstRes(_) => _dumpField(ret, dFields)
      case AggrRes(aggrs, _) => aggrs match {
        case Seq(FieldDecl(Count(None), _, _, _)) => s"dump($ret)"
        case _ => {
          val aggrF = TUtils.mapNonHiddenFields(aggrs, FieldDecl.as)
          _dumpField(ret, aggrF)
        }
      }
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
    case Eq(k, e) =>
      (Str(getSQLAlchemyFieldName(k)) << "==" << constructFieldExpr(e)).!
    case Gt(k, e) =>
      (Str(getSQLAlchemyFieldName(k)) << " > " << constructFieldExpr(e)).!
    case Gte(k, e) =>
      (Str(getSQLAlchemyFieldName(k)) << " >= " << constructFieldExpr(e)).!
    case Lt(k, e) =>
      (Str(getSQLAlchemyFieldName(k)) << " < " << constructFieldExpr(e)).!
    case Lte(k, e) =>
      (Str(getSQLAlchemyFieldName(k)) << " <= " << constructFieldExpr(e)).!
    case Contains(k, e) =>
      (Str(getSQLAlchemyFieldName(k)) << ".contains(" << constructFieldExpr(e) << ")").!
    case Not(pred)                  =>
      (Str("not_(") << translatePred(pred) << ")").!
    case Or(p1, p2)                 =>
      (Str("or_(") << translatePred(p1) << ", " << translatePred(p2) << ")").!
    case And(p1, p2)                =>
      (Str("and_(") << translatePred(p1) << ", " << translatePred(p2) << ")").!
  }

  def constructFilter(preds: Set[Predicate], having: Boolean = false) =
    if (having) {
      preds map { x =>
        (Str("having(") << translatePred(x) << ")").!
      } mkString(".")
    } else {
      preds map { x =>
        (Str("filter(") << translatePred(x) << ")").!
      } mkString(".")
    }

  def constructOrderBy(spec: Seq[(String, Order)]) = spec match {
    case Seq() => ""
    case _     =>
      (
        Str("order_by(") << (
          spec map { x =>
            x match {
              case (k, Desc) => "desc(" + getSQLAlchemyFieldName(k) + ")"
              case (k, Asc)  => "asc(" + getSQLAlchemyFieldName(k) + ")"
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
      // For addition, we explicitly use the '+' operator because when the
      // operands are strings, sqlalchemy generates the '||' db operator.
      case Add(a1, a2) => (a1, a2, ".op('+')(")
      case Sub(a1, a2) => (a1, a2, " - ")
      case Mul(a1, a2) => (a1, a2, " * ")
      case Div(a1, a2) => (a1, a2, " / ")
      case _           => ??? // Unreachable case
    }
    val str = Str("(") << constructFieldExpr(a1) << op << constructFieldExpr(a2) << ")"
    fexpr match {
      case Add(_, _) => (str << ")").!
      case _         => str.!
    }
  }

  def constructFieldExpr(fexpr: FieldExpr): String = fexpr match {
    case F(f)                  => getSQLAlchemyFieldName(f)
    case Constant(v, UnQuoted) => "literal(" + v + ")"
    case Constant(v, Quoted)   => "literal(" + Utils.quoteStr(v) + ")"
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
      s.aggrs match {
        case Seq() | Seq(FieldDecl(Count(None), _, _, _)) => {
          val dFields = TUtils.mapNonHiddenFields(
            s.fields.values, FieldDecl.as)
          val fieldStr = dFields mkString ","
          val q =
            if (fieldStr.equals(""))
              "session.query(" + s.source + ")"
            else
              "session.query(" + fieldStr + ").select_from(" + s.source + ")"
          QueryStr(
            Some("ret" + s.numGen.next().toString),
            Some(q)
          )
        }
        case _ => {
          val aggrs = TUtils.mapNonHiddenFields(s.aggrs, {
            case FieldDecl(f, l, t, _) =>
              s"type_coerce(${constructFieldExpr(f)}, ${getType(t)}).label('$l')"
          })
          val qstr = "session.query(" + (aggrs mkString ", ") + ").select_from(" + s.source + ")"
          QueryStr(
            Some("ret" + s.numGen.next().toString),
            Some(qstr)
          )
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

  def constructFieldDecls(fields: Iterable[FieldDecl]) =
    if (fields.isEmpty) QueryStr()
    else
      fields.foldLeft(QueryStr()) { case (acc, FieldDecl(f, as, t, _)) => {
        val str = Str("type_coerce(") << constructFieldExpr(f) << ", " << getType(StringF) <<
          ").label(" << Utils.quoteStr(as) << ")"
        acc >> QueryStr(Some(as), Some(str.!))
      }
    }

  def constructGroupBy(groupBy: Set[String]) = groupBy match {
    case Seq() => ""
    case _     =>
      "group_by(" + (
        groupBy map { getSQLAlchemyFieldName } mkString ", ") + ")"
  }

  override def constructCombinedQuery(s: State) = {
    val qstr = constructQueryPrefix(s)
    qstr >> QueryStr(Some("ret" + s.numGen.next().toString),
      Some(Seq(
        qstr.ret.get,
        constructOrderBy(s.orders),
        s.aggrs match {
          case Seq() => ""
          case Seq(FieldDecl(Count(None), _, _, _)) => "count()"
          case _ => "first()"
        }
      ) filter {
        case "" => false
        case _  => true
      }  mkString("."))
    )
  }

  override def constructNaiveQuery(s: State, first: Boolean, offset: Int,
      limit: Option[Int]) = {
    val fieldVals = s.fields.values
    val (aggrNHidden, nonAggrHidden) = TUtils.getAggrAndNonAggr(fieldVals)
    val qStr = constructFieldDecls(fieldVals) >> constructQueryPrefix(s)
    val (aggrP, nonAggrP) = s.preds partition { _.hasAggregate(s.fields) }
    qStr >> QueryStr(Some("ret" + s.numGen.next().toString),
      Some(Seq(
        qStr.ret.get,
        constructJoins(s.joins),
        constructFilter(nonAggrP),
        constructGroupBy(s.getNonConstantGroupingFields),
        constructFilter(aggrP, having = true),
        constructOrderBy(s.orders),
        s.aggrs match {
          case Seq() => ""
          case Seq(FieldDecl(Count(None), _, _, _)) => "count()"
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
    val (q1, q2) = (constructQuery(s1), constructQuery(s2))
    s1 >> (q1 << q2 >> QueryStr(Some("ret" + s1.numGen.next().toString),
                                Some(q1.ret.get + ".union(" + q2.ret.get + ")")))
  }

  override def intersectQueries(s1: State, s2: State) = {
    val (q1, q2) = (constructQuery(s1), constructQuery(s2))
    s1 >> (q1 << q2 >> QueryStr(Some("ret" + s1.numGen.next().toString),
                                Some(q1.ret.get + ".intersect(" + q2.ret.get + ")")))
  }
}
