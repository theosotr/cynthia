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

import scala.collection.immutable.ListMap

import cynthia.Target
import cynthia.lang._
import cynthia.orms.MSSQL
import cynthia.utils.{Utils, Str}


case class PonyTranslator(target: Target) extends Translator {
  target.db match {
    case MSSQL(_, _, _) =>
      throw UnsupportedException("Pony does not support MSSQL queries")
    case _ => ()
  }
  var fieldsMap = ListMap[String, String]()
  var nonHiddenFieldsMap = Map[String, String]()

  override val preamble =
    s"""import numbers
    |from decimal import Decimal
    |from pony.orm import *
    |from models_${target.db.getName} import *
    |
    |# set_sql_debug(True)
    |db.generate_mapping()
    |
    |def dump(x, label):
    |    if isinstance(x, numbers.Number):
    |        print(label, round(Decimal(float(x) + 0.00), 2))
    |    else:
    |        try:
    |            print(label, round(Decimal(float(x) + 0.00), 2))
    |        except:
    |            if type(x) is bytes:
    |                print(label, str(x.decode('utf-8')))
    |            else:
    |                print(label, x if x is not None else '0.00')
    |
    |with db_session:
    |""".stripMargin


  def getPonyFieldName(field: String) =
    field.split('.').toList match {
      case Nil | _ :: Nil => field
      case t              =>
        t takeRight(2) map { x => x.toLowerCase } mkString "."
    }

  override def emitPrint(q: Query, dFields: Seq[String], ret: String) = {
    def _dumpField(v: String, fields: Iterable[String], ident: String = "",
      isAggr: Boolean = false) = {
        val counter: Iterator[Int] = LazyList.from(0).iterator
        fields map { as => {
          if (isAggr) {
            val index = counter.next().toString
            s"${ident}dump($v[$index], '$as')"
          } else if (nonHiddenFieldsMap.contains(as)) {
            if (nonHiddenFieldsMap.size >= 1) {
              val index = nonHiddenFieldsMap.keysIterator.toList.indexOf(as)
              s"${ident}dump($v[$index], '$as')"
            } else
              s"${ident}dump($v, '$as')"
          } else {
            s"${ident}dump(getattr($v, '$as', getattr($v[0], '$as', None) if isinstance($v, tuple) else None), '$as')"
          }
        }
        } mkString "\n"
      }

    q match {
      case SetRes(_)  => {
        val trimmedRet = ret.trim
        s"    for r in $trimmedRet:\n${_dumpField("r", dFields, ident = " " * 8)}"
      }
      case SubsetRes(offset, limit, _) => {
        val trimmedRet = ret.trim
        val resolvedLimit = limit match {
          case Some(x) => x + 1
          case _       => ""
        }
        val subset = s"[$offset:$resolvedLimit]"
        s"    for r in $trimmedRet$subset:\n${_dumpField("r", dFields, ident = " " * 8)}"
      }
      case FirstRes(_) => _dumpField(ret, dFields, ident = " " * 4)
      case AggrRes (aggrs, _) => {
        val trimmedRet = ret.trim
        val counter: Iterator[Int] = LazyList.from(-1).iterator
        aggrs match {
          case Seq(FieldDecl(Count(None), _, _, _)) => s"    dump($ret, 'count')"
          case _ => {
            val aggrF = TUtils.mapNonHiddenFields(aggrs, FieldDecl.as)
            _dumpField(ret.trim, aggrF, ident = " " * 4, true)
          }
        }
      }
    }
  }

  def isConstant(field: FieldExpr) = field match {
    case Constant(_, _) => true
    case _              => false
  }

  def constructPrimAggr(fexpr: FieldExpr) = {
    val (field, op, isConstantField) = fexpr match {
      case Count(None)        => ("", "count", false)
      case Count(Some(field)) => (constructFieldExpr(field), "count", isConstant(field))
      case Sum(field)         => (constructFieldExpr(field), "sum", isConstant(field))
      case Avg(field)         => (constructFieldExpr(field), "avg", isConstant(field))
      case Min(field)         => (constructFieldExpr(field), "min", isConstant(field))
      case Max(field)         => (constructFieldExpr(field), "max", isConstant(field))
      case _                  => ??? // Unreachable case
    }
    val fieldString = if (isConstantField) "[" + field + "]" else field
    op + "(" + fieldString + ")"
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
    case F(f)                  => getPonyFieldName(f)
    case Constant(v, UnQuoted) => v
    case Constant(v, Quoted)   => s""""${v}""""
    case _    =>
      if (!fexpr.compound) constructPrimAggr(fexpr)
      else constructCompoundAggr(fexpr)
  }

  def translatePred(pred: Predicate): String = pred match {
    case Eq(k, e) =>
      (Str(getPonyFieldName(k)) << "==" << constructFieldExpr(e)).!
    case Gt(k, e) =>
      (Str(getPonyFieldName(k)) << " > " << constructFieldExpr(e)).!
    case Gte(k, e) =>
      (Str(getPonyFieldName(k)) << " >= " << constructFieldExpr(e)).!
    case Lt(k, e) =>
      (Str(getPonyFieldName(k)) << " < " << constructFieldExpr(e)).!
    case Lte(k, e) =>
      (Str(getPonyFieldName(k)) << " <= " << constructFieldExpr(e)).!
    case Contains(k, e) =>
      (Str(constructFieldExpr(Constant(e, Quoted))) << " in " << getPonyFieldName(k)).!
    case StartsWith(k, v) =>
      (Str(getPonyFieldName(k)) << ".startswith(" << Utils.quoteStr(Utils.escapeStr(v), "\"") << ")").!
    case EndsWith(k, v) =>
      (Str(getPonyFieldName(k)) << ".endswith(" << Utils.quoteStr(Utils.escapeStr(v), "\"") << ")").!
    case Not(pred) =>
      (Str("not (") << translatePred(pred) << ")").!
    case Or(p1, p2) =>
      (Str("(") << translatePred(p1) << ") or (" << translatePred(p2) << ")").!
    case And(p1, p2) =>
      (Str("(") << translatePred(p1) << ") and (" << translatePred(p2) << ")").!
  }

  // The following functions until extractFields are used to create a map for
  // field declarations to their expressions.
  def constructPrimField(acc: ListMap[String, String], fexpr: FieldExpr) = {
    val (field, op, isConstantField) = fexpr match {
      case Count(None)        =>
        ("", "count", false)
      case Count(Some(field)) =>
        (constructField(acc, field), "count", isConstant(field))
      case Sum(field)         =>
        (constructField(acc, field), "sum", isConstant(field))
      case Avg(field)         =>
        (constructField(acc, field), "avg", isConstant(field))
      case Min(field)         =>
        (constructField(acc, field), "min", isConstant(field))
      case Max(field)         =>
        (constructField(acc, field), "max", isConstant(field))
      case _                  => ??? // Unreachable case
    }
    val fieldString = if (isConstantField) "[" + field + "]" else field
    op + "(" + fieldString + ")"
  }

  def constructField(acc: ListMap[String, String], fexpr: FieldExpr): String = fexpr match {
    case F(f) => acc.get(f) match {
      case None     => getPonyFieldName(f)
      case Some(s)  => s
    }
    case Constant(v, UnQuoted) => v
    case Constant(v, Quoted)   => Utils.quoteStr(v, "\"")
    case Add(e1, e2) => "(" + constructField(acc, e1) + "+" + constructField(acc, e2) + ")"
    case Sub(e1, e2) => "(" + constructField(acc, e1) + "-" + constructField(acc, e2) + ")"
    case Mul(e1, e2) => "(" + constructField(acc, e1) + "*" + constructField(acc, e2) + ")"
    case Div(e1, e2) => "(" + constructField(acc, e1) + "/" + constructField(acc, e2) + ")"
    case _    => constructPrimField(acc, fexpr)
  }

  def extractFields(fields: Map[String, FieldDecl]): ListMap[String, String] = {
    val counter: Iterator[Int] = LazyList.from(0).iterator
    fields.foldLeft(ListMap[String, String]()) ({ (acc, x) =>
        x._2 match {
          case FieldDecl(e, as, _, _) =>
            acc + (as -> (constructField(acc, e)))
        }
      })
  }

  def constructSelectedItems(source: String) = {
    if (!nonHiddenFieldsMap.isEmpty) {
      val items = nonHiddenFieldsMap.foldLeft(Seq[String]()) { (acc, x) => {
        acc :+ x._2
      }} mkString ","
      items + "," + source.toLowerCase
    } else
      source.toLowerCase
  }

  def constructFilter(preds: Set[Predicate]) =
    if (preds.isEmpty)
      ""
    else {
      val conditions = preds map { x =>
        (Str("(") << translatePred(x) << ")").!
      } mkString " and "
      "if " + conditions
    }

  def constructJoins(joins: Seq[Seq[String]]) = {
    joins.foldLeft(Seq[String]()) { (acc, x) => {
      val (_, Seq(s, t)) = x splitAt (x.size - 2)
      val tLow = t.toLowerCase
      val sLow = s.toLowerCase
      acc :+ s"for ${tLow} in ${sLow}.${tLow}_id"
    }} mkString " "
  }

  // select, join, filter
  def constructQueryPrefix(s: State, selectedItems: String, joins: String,
    filter: String) = {
      val (prefix, without) = s.distinct match {
        case Some("") => ("distinct", "")
        case Some(x)  => throw UnsupportedException("Distinct on is not supported by Pony")
        case _        => ("select", ".without_distinct()")
      }
      s.query match {
        case None    => {
          val qstr = s"${prefix}((${selectedItems}) for ${s.source.toLowerCase} in ${s.source} ${joins} ${filter})${without}"
          QueryStr(
            Some("    ret" + s.numGen.next().toString),
            Some(qstr)
          )
        }
        case Some(q) => q
      }
  }

  def constructOrderBy(s: State, selectedItems: String) = {
    def _getFieldExprOrFieldName(f: String) = fieldsMap.get(f) match {
      case None     => getPonyFieldName(f)
      case Some(s)  => s
    }
    s.orders match {
      case Seq() => ""
      case spec  => {
        (
          Str("order_by(lambda: (") << (
            spec map { x =>
              x match {
                case (k, Desc) => "desc(" + _getFieldExprOrFieldName(k) + ")"
                case (k, Asc)  => _getFieldExprOrFieldName(k)
              }
            } mkString ","
          ) << "))"
        ).!
      }
    }
  }

  def constructAggrs(s: State, bodyQstr: QueryStr) = {
    val ret = bodyQstr.ret.get.trim
    if (s.aggrs.nonEmpty) {
      val selectedItems =
        if (!nonHiddenFieldsMap.isEmpty) {
          val items = nonHiddenFieldsMap.foldLeft(Seq[String]()) { (acc, x) => {
            acc :+ x._1
          }} mkString ","
          items + "," + s.source.toLowerCase
        } else
          s.source.toLowerCase
      val aggrFields = s.aggrs.foldLeft(Map[String, FieldDecl]())  {
        (acc,f) => acc + (f.as -> f)
      }
      val aggrItems = extractFields(aggrFields) map (_._2) mkString ","
      QueryStr(Some("    ret" + s.numGen.next().toString),
        Some(s"select(($aggrItems) for $selectedItems in $ret)"))
    } else
      QueryStr(Some("    ret" + s.numGen.next().toString), Some(ret))
  }

  def constructFirst(first: Boolean) =
    if (first) "first()"
    else ""

  override def constructCombinedQuery(s: State) =
    throw new UnsupportedException("unions are not supported in Pony")

  override def constructNaiveQuery(s: State, first: Boolean, offset: Int,
      limit: Option[Int]) = {
    def _filterNonHiddenFieldDecls(i: String) =
      !FieldDecl.hidden(s.fields.get(i).get)
    fieldsMap = extractFields(s.fields)
    nonHiddenFieldsMap = (fieldsMap.view filterKeys _filterNonHiddenFieldDecls).toMap
    val orderedJoins = s.joins.toSet.toSeq.sortWith { (x, y) => x.size < y.size }
    val selectedItems = constructSelectedItems(s.source)
    val joins = constructJoins(orderedJoins)
    val (aggrP, nonAggrP) = s.preds partition { _.hasAggregate(s.fields) }
    val filter = constructFilter(nonAggrP)
    // qStr has four parts
    // mainQstr includes select, join, filter
    // bodyQstr includes order_by, without_distinct
    // distinctQstr distinct or without_distinct
    // lastQstr first
    val mainQstr = constructQueryPrefix(s, selectedItems, joins, filter)
    val bodyQstr = QueryStr(Some("    ret" + s.numGen.next().toString),
      Some(
        Seq(
          mainQstr.ret.get.trim,
          constructOrderBy(s, selectedItems),
        ) filter {
          case "" => false
          case _  => true
        }  mkString(".")
      )
    )
    val aggrsQstr = constructAggrs(s, bodyQstr)
    val lastQstr = QueryStr(Some("    ret" + s.numGen.next().toString),
      Some(
        Seq(
          aggrsQstr.ret.get.trim,
          s.aggrs match {
            case Seq() => ""
            case Seq(FieldDecl(Count(None), _, _, _)) => "count()"
            case _ => "first()"
          },
          constructFirst(first),
        ) filter {
          case "" => false
          case _  => true
        }  mkString(".")
      )
    )
    mainQstr >> bodyQstr >> aggrsQstr >> lastQstr
  }

  override def unionQueries(s1: State, s2: State) =
    throw new UnsupportedException("unions are not supported in Pony")

  override def intersectQueries(s1: State, s2: State) =
    throw new UnsupportedException("unions are not supported in Pony")
}
