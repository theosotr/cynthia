/*
 * Copyright (c) 2020-2021 Thodoris Sotiropoulos, Stefanos Chaliasos
 *
 * This program is free software: you can redistribute it and/or modify  
 * it under the terms of the GNU General Public License as published by  
 * the Free Software Foundation, version 3.
 *
 * This program is distributed in the hope that it will be useful, but 
 * WITHOUT ANY WARRANTY; without even the implied warranty of 
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU 
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License 
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */

package cynthia.translators

import cynthia.targets.Target
import cynthia.lang._
import cynthia.targets.{Cockroachdb, Postgres, MSSQL}
import cynthia.utils.{Utils, RUtils, Str}


case class SQLAlchemyTranslator(target: Target) extends Translator {

  override val preamble =
    s"""from sqlalchemy import (create_engine, or_, and_, not_, func,
    |     type_coerce, types, literal, asc, desc)
    |from sqlalchemy.orm import sessionmaker, aliased
    |from sqlalchemy.exc import SAWarning
    |from models import *
    |import numbers, decimal, warnings
    |
    |# Ignore SQLAlchemy warnings
    |warnings.simplefilter("ignore", category=SAWarning)
    |
    |engine = create_engine(
    |             '${target.db.getURI()}'
    |             ${if (target.db.getName().equals("cockroachdb")) ",connect_args={'sslmode': 'disable'}" else ""}
    |         )
    |Session = sessionmaker(bind=engine)
    |session = Session()

    |def dump(x, label):
    |    if isinstance(x, numbers.Number):
    |        print(label, round(decimal.Decimal(float(x) + 0.00), 2))
    |    else:
    |        try:
    |            print(label, round(decimal.Decimal(float(x) + 0.00), 2))
    |        except:
    |            if type(x) is bytes:
    |                print(label, str(x.decode('utf-8')))
    |            else:
    |                print(label, x if x is not None else '0.00')
    |""".stripMargin

  override def emitPrint(q: Query, dFields: Seq[String], ret: String) = {
    def _dumpField(v: String, fields: Iterable[String], ident: String = "") =
      fields map { as => s"${ident}dump(getattr($v,'$as', None), '$as')" } mkString "\n"
    q match {
      case SetRes(_) | SubsetRes(_, _, _) =>
        s"for r in $ret:\n${_dumpField("r", dFields, ident = " " * 4)}"
      case FirstRes(_) => _dumpField(ret, dFields)
      case AggrRes(aggrs, _) => aggrs match {
        case Seq(FieldDecl(Count(None), _, _, _)) => s"dump($ret, 'count')"
        case _ => {
          val aggrF = TUtils.mapNonHiddenFields(aggrs, FieldDecl.as)
          _dumpField(ret, aggrF)
        }
      }
    }
  }

  def getSQLAlchemyFieldName(field: String, withAlias: Boolean = false) = {
    def _getSQLAlchemyFieldName(acc: String, segs: List[String]): String = segs match {
      case Nil      => acc
      case h :: Nil => acc + "." + h
      case h :: t   =>
        if (acc.equals("")) _getSQLAlchemyFieldName(h.capitalize, t)
        else _getSQLAlchemyFieldName(acc + "_" + h.capitalize, t)
    }
    val segs = (field split '.').toList
    segs match {
      case f :: Nil => if (withAlias) Utils.quoteStr(f) else TUtils.toFieldVar(f)
      case _        => _getSQLAlchemyFieldName("", segs)
    }
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
    case Contains(k, v) =>
      (Str(getSQLAlchemyFieldName(k)) << ".contains(" << Utils.quoteStr(Utils.escapeStr(v)) << ", autoescape=True)").!
    case StartsWith(k, e) =>
      (Str(getSQLAlchemyFieldName(k)) << ".startswith(" << Utils.quoteStr(Utils.escapeStr(e)) << ", autoescape=True)").!
    case EndsWith(k, e) =>
      (Str(getSQLAlchemyFieldName(k)) << ".endswith(" << Utils.quoteStr(Utils.escapeStr(e)) << ", autoescape=True)").!
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

  def constructOrderBy(s: State, withAlias: Boolean) = s.orders match {
    case Seq() => ""
    case spec  =>
      (s.aggrs, target.db) match {
        case (Seq(_, _*), Postgres(_, _, _))
        | (Seq(_, _*), Cockroachdb(_, _, _))
        | (Seq(_, _*), MSSQL(_, _, _)) => ""
        case _ =>
          (
            Str("order_by(") << (
              Utils.removeDups(spec) map { x =>
                x match {
                  case (k, Desc) => "desc(" + getSQLAlchemyFieldName(k, withAlias) + ")"
                  case (k, Asc)  => "asc(" + getSQLAlchemyFieldName(k, withAlias) + ")"
                }
              } mkString ","
            ) << ")"
          ).!
      }
  }

  def constructPrimAggr(fexpr: FieldExpr, fprefix: String) = {
    val (field, op) = fexpr match {
      case Count(None)        => ("", "func.count")
      case Count(Some(field)) => (constructFieldExpr(field, fprefix), "func.count")
      case Sum(field)         => (constructFieldExpr(field, fprefix), "func.sum")
      case Avg(field)         => (constructFieldExpr(field, fprefix), "func.avg")
      case Min(field)         => (constructFieldExpr(field, fprefix), "func.min")
      case Max(field)         => (constructFieldExpr(field, fprefix), "func.max")
      case _                  => ??? // Unreachable case
    }
    op + "(" + field + ")"
  }

  def constructCompoundAggr(fexpr: FieldExpr, fprefix: String) = {
    val (a1, a2, op) = fexpr match {
      // For addition, we explicitly use the '+' operator because when the
      // operands are strings, sqlalchemy generates the '||' db operator.
      case Add(a1, a2) => (a1, a2, ".op('+')(")
      case Sub(a1, a2) => (a1, a2, " - ")
      case Mul(a1, a2) => (a1, a2, " * ")
      case Div(a1, a2) => (a1, a2, " / ")
      case _           => ??? // Unreachable case
    }
    val str = Str("(") << constructFieldExpr(a1, fprefix) << op <<
      constructFieldExpr(a2, fprefix) << ")"
    fexpr match {
      case Add(_, _) => (str << ")").!
      case _         => str.!
    }
  }

  def constructFieldExpr(fexpr: FieldExpr, fprefix: String = ""): String = fexpr match {
    case F(f)                  =>
      if (fprefix.equals("")) getSQLAlchemyFieldName(f)
      else fprefix + "." + TUtils.toLabel(getSQLAlchemyFieldName(f))
    case Constant(v, UnQuoted) => "literal(" + v + ")"
    case Constant(v, Quoted)   => "literal(" + Utils.quoteStr(Utils.escapeStr(v)) + ")"
    case _    =>
      if (!fexpr.compound) constructPrimAggr(fexpr, fprefix)
      else constructCompoundAggr(fexpr, fprefix)
  }

  def constructAliases(joins: Seq[Seq[String]]) = {
    joins.foldLeft(QueryStr()) { (acc, x) =>
      val (h :: t) = x.toList.reverse
      val alias = (t.reverse mkString "_") + "_" + h
      acc >> QueryStr(Some(alias), Some("aliased(" + h + ")"))
    }
  }

  def constructJoins(joins: Seq[Seq[String]]) =
    joins.foldLeft(Seq[String]()) { (acc, x) => {
      val (prefix, Seq(s, t)) = x splitAt (x.size - 2)
      val aliasSuffix =
        if (prefix.isEmpty) ""
        else (prefix mkString "_") + "_"
      acc :+ s"join(${aliasSuffix}${s}_${t}, ${aliasSuffix}${s}.${t.toLowerCase})"
    }} mkString "."

  def constructAggrPrefix(s: State, subquery: Boolean) = {
    val prefix = if (subquery) s.source + ".c" else ""
    val aggrs = TUtils.mapNonHiddenFields(s.aggrs, {
      case FieldDecl(f, l, t, _) =>
        s"type_coerce(${constructFieldExpr(f, prefix)}, ${getType(t)}).label('$l')"
    })
    val qstr = "session.query(" + (aggrs mkString ", ") + ").select_from(" + s.source + ")"
    QueryStr(
      Some("ret" + s.numGen.next().toString),
      Some(qstr)
    )
  }

  def constructQueryPrefix(s: State) =  s.query match {
    case None =>
      s.aggrs match {
        case Seq() | Seq(FieldDecl(Count(None), _, _, _)) => {
          val dFields = TUtils.mapNonHiddenFields(
            s.fields.values, { x => TUtils.toFieldVar(FieldDecl.as(x)) })
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
        case _ => constructAggrPrefix(s, false)
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
        acc >> QueryStr(Some(TUtils.toFieldVar(as)), Some(str.!))
      }
    }

  def constructGroupBy(groupBy: Set[String]) = groupBy match {
    case Seq() => ""
    case _     =>
      "group_by(" + (
        groupBy map { x => getSQLAlchemyFieldName(x) } mkString ", ") + ")"
  }

  def constructDistinct(distinct: Option[String], s: State) = distinct match {
    case Some("") => "distinct()"
    case Some(x)  => {
      target.db match {
        case Postgres(_, _, _) | Cockroachdb(_, _, _) => {
          if (x.split('.').size > 1)
            s"distinct(${getSQLAlchemyFieldName(x)})"
          else
            s"distinct(${TUtils.toFieldVar(x)})"
        }
        case _                 =>
          throw new UnsupportedException("Distinct on is supported only by Postgres")
      }
    }
    case _        => ""
  }

  override def constructCombinedQuery(s: State) = {
    val qstr = constructQueryPrefix(s)
    val qstr2 = qstr >> QueryStr(Some("ret" + s.numGen.next().toString),
      Some(Seq(
        qstr.ret.get,
        constructOrderBy(s, true),
        if (!s.aggrs.isEmpty) "subquery()" else ""
      ) filter {
        case "" => false
        case _  => true
      }  mkString("."))
    )
    if (!s.aggrs.isEmpty) {
      val str = qstr2 >> constructAggrPrefix(s source qstr2.ret.get, true)
      str >> QueryStr(Some("ret" + s.numGen.next().toString),
        Some(
          str.ret.get +
          (s.aggrs match {
            case Seq() => ""
            case Seq(FieldDecl(Count(None), _, _, _)) => ".count()"
            case _ => ".first()"
          })
        )
      )
    } else qstr2
  }

  override def constructNaiveQuery(s: State, first: Boolean, offset: Int,
      limit: Option[Int]) = {
    if (!s.aggrs.isEmpty && s.distinct.isDefined) {
      throw new UnsupportedException(
        "Distinct is not supported in aggregate queries.")
    }
    val fieldVals = s.fields.values
    val aliasStms = constructAliases(s.joins)
    val (aggrNHidden, nonAggrHidden) = TUtils.getAggrAndNonAggr(fieldVals)
    val qStr = aliasStms >> constructFieldDecls(fieldVals) >> constructQueryPrefix(s)
    val (aggrP, nonAggrP) = s.preds partition { _.hasAggregate(s.fields) }
    qStr >> QueryStr(Some("ret" + s.numGen.next().toString),
      Some(Seq(
        qStr.ret.get,
        constructJoins(s.joins.toSet.toSeq.sortWith { (x, y) => x.size < y.size }),
        constructFilter(nonAggrP),
        constructGroupBy(s.getNonConstantGroupingFields()),
        constructFilter(aggrP, having = true),
        constructOrderBy(s, false),
        constructDistinct(s.distinct, s),
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
