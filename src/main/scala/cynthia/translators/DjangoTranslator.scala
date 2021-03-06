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
import cynthia.targets.{Postgres, MySQL, MSSQL, Cockroachdb, SQLite}
import cynthia.utils.{Utils, RUtils, Str}

case class DjangoTranslator(target: Target) extends Translator {
  private val hidden: scala.collection.mutable.Set[String] =
    scala.collection.mutable.Set()

  override val preamble =
    s"""import os, django
   |from django.db.models import *
   |# from django.db import connection
   |os.environ.setdefault('DJANGO_SETTINGS_MODULE', 'djangoproject.settings')
   |django.setup()
   |
   |from ${target.orm.projectName}.models import *
   |import numbers, decimal
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
        |${ident}    dump($v.get('$as', None), '$as')
        |${ident}else:
        |${ident}    dump(getattr($v, '$as', None), '$as')""".stripMargin
      } mkString "\n"
    q match {
      case SetRes(_) | SubsetRes(_, _, _) =>
        s"for r in $ret:\n${_dumpField("r", dfields, ident = " " * 4)}"
      case FirstRes(_) => _dumpField(ret, dfields)
      case AggrRes(aggrs, _) => {
        val aggrF = TUtils.mapNonHiddenFields(aggrs, FieldDecl.as)
        _dumpField(ret, aggrF)
      }
    }
  }

  def constructFilter(preds: Set[Predicate]) =
    preds map { x =>
      x match {
        case Not(p) =>
          RUtils.chooseFrom(
            Seq(
              (Str("filter(") << translatePred(x) << ")").!,
              (Str("exclude(") << translatePred(p) << ")").!
            )
          )
        case _ => (Str("filter(") << translatePred(x) << ")").!
      }
    } mkString (".")

  def constructOrderBy(spec: Seq[(String, Order)]) = spec match {
    case Seq() => ""
    case _ =>
      (
        Str("order_by(") << (
          spec map { x =>
            x match {
              case (k, Desc) => Utils.quoteStr("-" + getDjangoFieldName(k))
              case (k, Asc)  => Utils.quoteStr(getDjangoFieldName(k))
            }
          } mkString (",")
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

  def constructQueryPrefix(s: State) = s.query match {
    case None => {
      val dbname = target.db match {
        case Postgres(_, _, _)    => "postgres"
        case MySQL(_, _, _)       => "mysql"
        case MSSQL(_, _, _)       => "mssql"
        case Cockroachdb(_, _, _) => "cockroachdb"
        case SQLite(_)            => "default"
      }
      QueryStr(
        Some("ret" + s.numGen.next().toString),
        Some(s.source + ".objects.using('" + dbname + "')")
      )
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

  def constructFieldExpr(
      fexpr: FieldExpr,
      fieldType: String = "FloatField()"
  ): String = fexpr match {
    case F(f) =>
      if (this.hidden contains f)
        // Hidden fields are not annotated so just simply return `f`
        // corresponding to the variable holding the definition of hidden field.
        f
      else
        "F(" + Utils.quoteStr(getDjangoFieldName(f)) + ")"
    case Add(a1, a2) =>
      "(" + constructFieldExpr(a1) + " + " + constructFieldExpr(a2) + ")"
    case Sub(a1, a2) =>
      "(" + constructFieldExpr(a1) + " - " + constructFieldExpr(a2) + ")"
    case Mul(a1, a2) =>
      "(" + constructFieldExpr(a1) + " * " + constructFieldExpr(a2) + ")"
    case Div(a1, a2) =>
      "(" + constructFieldExpr(a1) + " / " + constructFieldExpr(a2) + ")"
    case Constant(v, UnQuoted) => "Value(" + v + ")"
    case Constant(v, Quoted) =>
      "Value(" + Utils.quoteStr(Utils.escapeStr(v)) + ")"
    case _ => constructPrimAggr(fexpr, fieldType)
  }

  def constructAggrs(aggrs: Seq[FieldDecl]) = aggrs match {
    case Seq() => ""
    case _ => {
      val fields = TUtils.mapNonHiddenFields(
        aggrs,
        { case FieldDecl(f, as, t, _) =>
          (Str(as) << "=ExpressionWrapper(" << constructFieldExpr(
            f,
            fieldType = getType(t)
          ) <<
            ", output_field=" << getType(t) << ")").!
        }
      )
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

  def constructAnnotate(fields: Seq[String]) =
    if (fields.isEmpty) ""
    else fields map { x => "annotate(" + x + "=" + x + ")" } mkString "."

  def constructValues(groupBy: Set[String]) = groupBy match {
    case Seq() => ""
    case _ =>
      "values(" + (groupBy map { x =>
        Utils.quoteStr(getDjangoFieldName(x))
      } mkString ", ") + ")"
  }

  def constructFieldDecls(fields: Iterable[FieldDecl]) =
    if (fields.isEmpty) QueryStr()
    else {
      fields.foldLeft(QueryStr()) {
        case (acc, FieldDecl(f, as, t, _)) => {
          val str = Str("ExpressionWrapper(") << constructFieldExpr(f) <<
            ", output_field=" + getType(t) + ")"
          acc >> QueryStr(Some(as), Some(str.!))
        }
      }
    }

  def computeFieldGraph(fields: Map[String, FieldDecl]) = {
    def _examineExpr(
        acc: Map[String, Set[String]],
        e: FieldExpr,
        as: String
    ): Map[String, Set[String]] = e match {
      case F(f) =>
        fields get f match {
          case None => acc // the field is native.
          // here, the expression references a field.
          case Some(_) =>
            acc + (as -> (acc get as match {
              case None    => Set(f)
              case Some(s) => s + f
            }))
        }
      case Constant(_, _) => acc
      case Count(None)    => acc
      case Count(Some(e)) => _examineExpr(acc, e, as)
      case Sum(e)         => _examineExpr(acc, e, as)
      case Avg(e)         => _examineExpr(acc, e, as)
      case Min(e)         => _examineExpr(acc, e, as)
      case Max(e)         => _examineExpr(acc, e, as)
      case Add(e1, e2) =>
        Utils.mergeMap(_examineExpr(acc, e1, as), _examineExpr(acc, e2, as))
      case Sub(e1, e2) =>
        Utils.mergeMap(_examineExpr(acc, e1, as), _examineExpr(acc, e2, as))
      case Mul(e1, e2) =>
        Utils.mergeMap(_examineExpr(acc, e1, as), _examineExpr(acc, e2, as))
      case Div(e1, e2) =>
        Utils.mergeMap(_examineExpr(acc, e1, as), _examineExpr(acc, e2, as))
    }
    val initMap = fields.keys.foldLeft(Map[String, Set[String]]()) { (acc, x) =>
      acc + (x -> Set())
    }
    fields.values.foldLeft(initMap) { (acc, x) =>
      x match { case FieldDecl(e, as, _, _) => _examineExpr(acc, e, as) }
    }
  }

  def constructDistinct(distinct: Option[String], hasAnnotate: Boolean) =
    distinct match {
      case Some("") => "distinct()"
      case Some(x) => {
        // Django does not support DISTINCT ON with annotate() calls.
        if (x.split('.').size > 1 && !hasAnnotate)
          s"distinct('${getDjangoFieldName(x)}')"
        else
          throw new UnsupportedException(
            "Distincts with declared fields are not supported"
          )
      }
      case _ => ""
    }

  override def constructCombinedQuery(s: State) = {
    val qstr = constructQueryPrefix(s)
    qstr >> QueryStr(
      Some("ret" + s.numGen.next().toString),
      Some(
        (Seq(
          qstr.ret.get,
          constructOrderBy(s.orders),
          constructAggrs(s.aggrs)
        ) filter {
          case "" => false
          case _  => true
        } mkString ("."))
      )
    )
  }

  override def constructNaiveQuery(
      s: State,
      first: Boolean,
      offset: Int,
      limit: Option[Int]
  ) = {
    val fieldVals = s.fields.values
    val hidden = TUtils.filterHidden(fieldVals)
    this.hidden ++= hidden
    val qStr = constructFieldDecls(fieldVals) >> constructQueryPrefix(s)
    val (aggrP, nonAggrP) = s.preds partition { _.hasAggregate(s.fields) }
    val groupBy: Set[String] = s.nonAggrF.toSeq match {
      case Seq(f) =>
        if (f.equals(s.source + ".id")) Set()
        else s.getNonConstantGroupingFields()
      case _ => s.getNonConstantGroupingFields()
    }
    val fieldsTopSort = Utils.topologicalSort(computeFieldGraph(s.fields))
    val (aggrF, nonAggrF) = fieldsTopSort partition { x => s.aggrF.contains(x) }
    val nonAggrFields = nonAggrF filter (x =>
      s.fields get x match {
        // filter out hidden fields
        case None    => false
        case Some(f) => !FieldDecl.hidden(f)
      }
    )
    // "buz"
    qStr >> QueryStr(
      Some("ret" + s.numGen.next().toString),
      Some(
        (Seq(
          qStr.ret.get,
          constructAnnotate(nonAggrFields),
          constructFilter(nonAggrP),
          if (!s.aggrF.isEmpty) constructValues(groupBy ++ s.constantF) else "",
          constructAnnotate(aggrF),
          constructOrderBy(s.orders),
          constructFilter(aggrP),
          constructDistinct(
            s.distinct,
            !(nonAggrFields.isEmpty && aggrF.isEmpty)
          ),
          constructValues((fieldVals filter {
            !FieldDecl.hidden(_)
          } map FieldDecl.as).toSet),
          constructAggrs(s.aggrs),
          constructFirst(first)
        ) filter {
          case "" => false
          case _  => true
        } mkString (".")) + constructOffsetLimit(offset, limit)
      )
    )
  }

  override def unionQueries(s1: State, s2: State) = {
    val (q1, q2) = (constructQuery(s1), constructQuery(s2))
    s1 >> (q1 << q2 >> QueryStr(
      Some("ret" + s1.numGen.next().toString),
      Some(q1.ret.get + ".union(" + q2.ret.get + ")")
    ))
  }

  override def intersectQueries(s1: State, s2: State) = {
    val (q1, q2) = (constructQuery(s1), constructQuery(s2))
    s1 >> (q1 << q2 >> QueryStr(
      Some("ret" + s1.numGen.next().toString),
      Some(q1.ret.get + ".intersection(" + q2.ret.get + ")")
    ))
  }

  def translatePred(pred: Predicate): String = pred match {
    case Eq(k, e) =>
      (Str(getDjangoFieldName(k)) << "=" << constructFieldExpr(e)).!
    case Gt(k, e) =>
      (Str(getDjangoFieldName(k)) << "__gt=" << constructFieldExpr(e)).!
    case Gte(k, e) =>
      (Str(getDjangoFieldName(k)) << "__gte=" << constructFieldExpr(e)).!
    case Lt(k, e) =>
      (Str(getDjangoFieldName(k)) << "__lt=" << constructFieldExpr(e)).!
    case Lte(k, e) =>
      (Str(getDjangoFieldName(k)) << "__lte=" << constructFieldExpr(e)).!
    case Contains(k, e) =>
      (Str(getDjangoFieldName(k)) << "__contains=" << Utils.quoteStr(
        Utils.escapeStr(e)
      )).!
    case StartsWith(k, e) =>
      (Str(getDjangoFieldName(k)) << "__startswith=" << Utils.quoteStr(
        Utils.escapeStr(e)
      )).!
    case EndsWith(k, e) =>
      (Str(getDjangoFieldName(k)) << "__endswith=" << Utils.quoteStr(
        Utils.escapeStr(e)
      )).!
    case Not(pred) =>
      (Str("~Q(") << translatePred(pred) << ")").!
    case Or(p1, p2) =>
      (Str("Q(") << translatePred(p1) << ") | Q(" << translatePred(p2) << ")").!
    case And(p1, p2) =>
      (Str("Q(") << translatePred(p1) << "), Q(" << translatePred(p2) << ")").!
  }
}
