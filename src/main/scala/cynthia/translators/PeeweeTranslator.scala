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
import cynthia.targets.{MSSQL, Cockroachdb, Postgres}
import cynthia.utils.{Utils, Str}


case class PeeweeTranslator(target: Target) extends Translator {
  target.db match {
    case MSSQL(_, _, _) =>
      throw UnsupportedException("Peewee does not support MSSQL queries")
    case _ => ()
  }

  override val preamble =
    s"""import numbers, decimal
    |from peewee import *
    |from models_${target.db.getName()} import *
    |
    |# import logging
    |# logger = logging.getLogger('peewee')
    |# logger.addHandler(logging.StreamHandler())
    |# logger.setLevel(logging.DEBUG)
    |
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
      fields map { as => s"${ident}dump(getattr($v, '$as', None), '$as')" } mkString "\n"
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

  def getPeeweeFieldName(field: String, withAlias: Boolean = false) = {
    def _getPeeweeFieldName(acc: String, segs: List[String]): String = segs match {
      case Nil      => acc
      case h :: Nil => acc + "." + h
      case h :: t   =>
        if (acc.equals("")) _getPeeweeFieldName(h.capitalize, t)
        else _getPeeweeFieldName(acc + "_" + h.capitalize, t)
    }
    val segs = field.split('.').toList
    segs match {
      // Revert alias for declared fields.
      case _ :: Nil =>
        if (withAlias) TUtils.toFieldVar(field)
        else TUtils.toFieldVar(field) + ".alias()"
      case _        => _getPeeweeFieldName("", segs.toList)
    }
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
    case Contains(k, v) =>
      (Str(getPeeweeFieldName(k)) << ".contains(" << Utils.quoteStr(Utils.escapeStr(v)) << ")").!
    case StartsWith(k, v) =>
      (Str(getPeeweeFieldName(k)) << ".startswith(" << Utils.quoteStr(Utils.escapeStr(v)) << ")").!
    case EndsWith(k, v) =>
      (Str(getPeeweeFieldName(k)) << ".endswith(" << Utils.quoteStr(Utils.escapeStr(v)) << ")").!
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

  def constructOrderBy(s: State, withAlias: Boolean) = s.orders match {
    case Seq() => ""
    case spec  =>
      (s.aggrs, target.db) match {
        case (Seq(_, _*), Postgres(_, _, _)) | (Seq(_, _*), Cockroachdb(_, _, _)) => ""
        case _ =>
          (
            Str("order_by(") << (
              spec map { x =>
                x match {
                  case (k, Desc) => getPeeweeFieldName(k, withAlias = withAlias) + ".desc()"
                  case (k, Asc)  => getPeeweeFieldName(k, withAlias = withAlias) + ".asc()"
                }
              } mkString ","
            ) << ")"
          ).!
      }
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
    case Constant(v, UnQuoted) => s"Value($v, converter=False)"
    case Constant(v, Quoted)   => s"Value(${Utils.quoteStr(Utils.escapeStr(v))}, converter=False)"
    case _    =>
      if (!fexpr.compound) constructPrimAggr(fexpr)
      else constructCompoundAggr(fexpr)
  }

  def constructAliases(joins: Seq[Seq[String]]) = {
    joins.foldLeft(QueryStr()) { (acc, x) =>
      val (h :: t) = x.toList.reverse
      val alias = (t.reverse mkString "_") + "_" + h
      acc >> QueryStr(Some(alias), Some(h + ".alias()"))
    }
  }

  def constructJoins(joins: Seq[Seq[String]]) =
    joins.foldLeft(Seq[String]()) { (acc, x) => {
      val (prefix, Seq(s, t)) = x splitAt (x.size - 2)
      val aliasSuffix =
        if (prefix.isEmpty) ""
        else (prefix mkString "_") + "_"
      acc :+ s"switch(${aliasSuffix}${s}).join(${aliasSuffix}${s}_${t})"
    }} mkString "."

  def constructQueryPrefix(s: State) =  s.query match {
    case None =>
      s.aggrs match {
        case Seq() | Seq(FieldDecl(Count(None), _, _, _)) => {
          val dFields = TUtils.mapNonHiddenFields(
            s.fields.values, { x => TUtils.toFieldVar(FieldDecl.as(x)) })
          // When we have distinct, we implicitly add all fields referenced
          // in the 'order' operation to the list of fields mentioned in
          // select.
          val allFields = s.distinct match {
            case None => dFields
            case _    => dFields ++ (s.orders map {
              case (x, _) => getPeeweeFieldName(x, withAlias = false)
            })
          }
          val fieldStr = allFields mkString ","
          val q =
            if (fieldStr.equals(""))
              s"${s.source}.select()"
            else
              s"${s.source}.select($fieldStr)"
          QueryStr(
            Some("ret" + s.numGen.next().toString),
            Some(q)
          )
        }
        case _ => {
          val aggrs = TUtils.mapNonHiddenFields(s.aggrs, {
            case FieldDecl(f, l, t, _) =>
              s"(${constructFieldExpr(f)}).coerce(False).alias('$l')"
          })
          val qstr = s"${s.source}.select(${(aggrs mkString ", ")})"
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
      if (offset > 0) s"offset($offset).limit($limit)"
      else s"limit($limit)"
  }

  def getType(ftype: FieldType) = ftype match {
    case StringF   => "str"
    case IntF      => "int"
    case DoubleF   => "float"
    case BooleanF  => "bool"
    case DateTimeF => "str"
  }

  def constructFieldDecls(fields: Iterable[FieldDecl]) =
    if (fields.isEmpty) QueryStr()
    else
      fields.foldLeft(QueryStr()) { case (acc, FieldDecl(f, as, t, _)) => {
        val str = Str("(") << constructFieldExpr(f) << ").coerce(False)" <<
          ".alias(" << Utils.quoteStr(as) << ")"
        acc >> QueryStr(Some(TUtils.toFieldVar(as)), Some(str.!))
      }
    }

  def constructGroupBy(groupBy: Set[String]) = groupBy match {
    case Seq() => ""
    case _     =>
      "group_by(" + (
        groupBy map { x => getPeeweeFieldName(x) } mkString ", ") + ")"
  }

  def constructDistinct(distinct: Option[String]) = distinct match {
    case Some("") => "distinct()"
    case Some(x)  => s"distinct(${getPeeweeFieldName(x, withAlias = false)})"
    case _        => ""
  }

  override def constructCombinedQuery(s: State) = {
    if (!s.aggrs.isEmpty)
      throw UnsupportedException(
        "Aggregate functions are not supported in combined queries")
    val qstr = constructQueryPrefix(s)
    qstr >> QueryStr(Some("ret" + s.numGen.next().toString),
      Some(Seq(
        qstr.ret.get,
        constructOrderBy(s, true),
        "objects()",
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
    if (!s.aggrs.isEmpty && s.distinct.isDefined) {
      throw new UnsupportedException(
        "Distinct is not supported in aggregate queries.")
    }
    val fieldVals = s.fields.values
    val (aggrNHidden, nonAggrHidden) = TUtils.getAggrAndNonAggr(fieldVals)
    val aliasStms = constructAliases(s.joins)
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
        constructDistinct(s.distinct),
        "objects()",
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
