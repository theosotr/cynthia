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

import cynthia.Target
import cynthia.lang._
import cynthia.orms.{Postgres, MySQL, MSSQL, Cockroachdb, SQLite}
import cynthia.utils.{Utils, Str}


case class ActiveRecordTranslator(target: Target) extends Translator {

  val dbsettings = target.db match {
    case Postgres(user, password, dbname) =>
      Str("\tadapter: 'postgresql',\n") <<
        "\thost: 'localhost',\n" <<
        "\tusername: " << Utils.quoteStr(user) << ",\n" <<
        "\tpassword: " << Utils.quoteStr(password) << ",\n" <<
        "\tdatabase: " << Utils.quoteStr(dbname)
    case MySQL(user, password, dbname) =>
      Str("\tadapter: 'mysql2',\n") <<
        "\thost: 'localhost',\n" <<
        "\tusername: " << Utils.quoteStr(user) << ",\n" <<
        "\tpassword: " << Utils.quoteStr(password) << ",\n" <<
        "\tdatabase: " << Utils.quoteStr(dbname)
    case MSSQL(user, password, dbname) =>
      Str("\tadapter: 'sqlserver',\n") <<
        "\thost: 'localhost',\n" <<
        "\tusername: " << Utils.quoteStr(user) << ",\n" <<
        "\tpassword: " << Utils.quoteStr(password) << ",\n" <<
        "\tdatabase: " << Utils.quoteStr(dbname)
    case Cockroachdb(user, password, dbname) =>
      Str("\tadapter: 'cockroachdb',\n") <<
        "\thost: 'localhost',\n" <<
        "\tport: '26257',\n" <<
        "\tusername: " << Utils.quoteStr(user) << ",\n" <<
        "\tdatabase: " << Utils.quoteStr(dbname)
    case SQLite(dbname) =>
      Str("\tadapter: 'sqlite3',\n") <<
        "\tdatabase: " << Utils.quoteStr(dbname)
  }
  var fieldsMap = Map[String, String]()
  var nonHiddenFieldsMap = Map[String, String]()

  override val preamble =
    s"""require 'active_record'
    |Dir[File.join(__dir__, 'models', '*.rb')].each {|file| require_relative file }
    |
    |ActiveRecord::Base.establish_connection(
    |${dbsettings.!}
    |)

    |# ActiveRecord::Base.logger = Logger.new(STDOUT)

    |def dump(var, label)
    |  if var.is_a? Integer
    |    puts "#{label} %0.2f" % [var]
    |  elsif var.is_a? Numeric
    |    puts "#{label} %0.2f" % [var.round(2)]
    |  elsif var == nil
    |    puts "#{label} 0.00"
    |  else
    |    puts "#{label} %0.2f" % Float(var)
    |  end
    |rescue
    |  puts "#{label} #{var}"
    |end
    |""".stripMargin

  override def emitPrint(q: Query, dFields: Seq[String], ret: String) = {
    def _dumpField(v: String, fields: Iterable[String], ident: String = "") =
      fields map { as => s"${ident}begin\n${ident*2}dump($v.$as, '$as')\n${ident}rescue\n${ident*2}dump($v, '$as')\n${ident}end" } mkString "\n"
    q match {
      case FirstRes(_) => _dumpField(ret, dFields)
      case SetRes(_) | SubsetRes(_, _, _) =>
        s"for i in $ret\n${_dumpField("i", dFields, ident = " " * 2)}\nend"
      case AggrRes (aggrs, _) => {
        aggrs map { x => x match {
          case x => {
            val (qfield, l) = x match { case FieldDecl(f, l, _, _) => (f, l) }
            s"dump($ret.${constructAggrExpr(qfield, nonHiddenFieldsMap)}, '$l')"
          }
        } } mkString("\n")
      }
    }
  }

  def escapeActiveRecordStr(s: String, like: Boolean = false) = {
    target.db match {
      case MySQL(_, _, _) =>
        if (like)
          s.replace("\\", "\\\\\\\\").replace("%", "\\%")
        else
          s.replace("\\", "\\\\\\\\").replace("'", "''")
      case SQLite(_) =>
        if (like)
          s.replace("\\", "\\\\").replace("%", "\\%")
        else
          s.replace("\\", "\\\\").replace("'", "''")
      case _              =>
        if (like)
          s.replace("\\", "\\\\")
        else
          s.replace("\\", "\\\\").replace("'", "''")
    }
  }

  def getActiveRecordFieldName(field: String) =
    if (field.contains(".")) {
      target.db match {
        case MySQL(_, _, _) => field.split('.').takeRight(2).map(
          s => s.substring(0, 1).toLowerCase() + s.substring(1)
        ).mkString(".")
        case _              => field.split('.').takeRight(2).mkString(".")
      }
    } else {
      field
    }

  def getSelectFieldName(field: String) = {
    val fieldElems = field.split('.')
    val numberOfDots = fieldElems.length
    if (numberOfDots > 2)
      fieldElems.takeRight(numberOfDots - 1).mkString(".")
    else
      field
  }

  def getSelectFields(s: State, nonAggrHidden: Map[String, String]) = {
    def _checkId(field: String) = {
      val fields = field.split('_')
      if (s.source.equals(fields(0).capitalize) && fields(1).equals("id"))
        "id"
      else
        field
    }
    s.distinct match {
      case Some(_) => nonHiddenFieldsMap ++ s.orders.filter(
        x => !nonHiddenFieldsMap.contains(x._1)).map{
          case (s, _) =>
            (_checkId(s.replace('.', '_')) -> getSelectFieldName(s.toLowerCase))
        }.toMap
      case _ => nonHiddenFieldsMap
    }
  }

  def constructJoins(joins: Set[(String, String)], source: String): String = {
    if (joins.isEmpty) ""
    else {
      val g = joins.groupBy(_._1).map { case (k,v) => (k,v.map(_._2))}
      // https://guides.rubyonrails.org/active_record_querying.html#joins
      // Revisit: not tail recursive, infinite loop
      def dfs(g: Map[String, Set[String]], current: String): String = {
           g.get(current) match {
             case None => ":" + current.toLowerCase
             case Some(e) => {
               val temp = e map {
                 (x) => {
                   dfs(g, x)
                 }
               }
               "{" + current.toLowerCase + ":[" + (temp mkString(",")) + "]}"
             }
           }
      }
      val sjoin = g(source) map {
        x => dfs(g, x)
      }
      "joins(" + (sjoin mkString(",")) + ")"
    }
  }

  def constructOrderBy(spec: Seq[(String, Order)], fields: Map[String, String]) = {
    def getOrderFieldName(k: String): String =
      k split '.' match {
        case Array(_) => fields(k)
        case x => {
          val Array(a, b) = x takeRight 2
          s"${a.toLowerCase}.${b.toLowerCase}"
        }
      }
    spec match {
      case Seq() => ""
      case _     =>
        (
          Str("order(") << (
            spec map { x =>
              x match {
                case (k, Desc) => "\"" + getOrderFieldName(k) + " DESC\""
                case (k, Asc)  => "\"" + getOrderFieldName(k) + " ASC\""
              }
            } mkString(",")
          ) << ")"
        ).!
    }
  }

  def translatePred(pred: Predicate, fields: Map[String, String]): (String, String) = {
    def handlePredicate(k: String, e: FieldExpr, symbol: String): (String, String) = {
      if (!e.isConstant) throw new UnsupportedException("complex where clauses are not supported")
      val key = fields.get(k) match {
          case None     => k.split('.').takeRight(2).mkString(".").toLowerCase()
          case Some(s)  => s
      }
      if (symbol.equals("LIKE"))
        (key + s" $symbol ?", s""", "%#{${constructFieldExpr(e, fields, true)}}%"""")
      else
        (key + s" $symbol ?", ", " + constructFieldExpr(e, fields))
    }
    def handleWithPredicate(k: String, e: String, symbol: String): (String, String) = {
      val key = fields.get(k) match {
          case None     => k.split('.').takeRight(2).mkString(".").toLowerCase()
          case Some(s)  => s
      }
      if (symbol.equals("StartsWith"))
        (key + s" LIKE ?", s""", "#{${Utils.quoteStr(escapeActiveRecordStr(e, true), "\"")}}%"""")
      else
        (key + s" LIKE ?", s""", "%#{${Utils.quoteStr(escapeActiveRecordStr(e, true), "\"")}}"""")
    }
    pred match {
      case Eq(k, e)         => handlePredicate(k, e, "=")
      case Gt(k, e)         => handlePredicate(k, e, ">")
      case Gte(k, e)        => handlePredicate(k, e, ">=")
      case Lt(k, e)         => handlePredicate(k, e, "<")
      case Lte(k, e)        => handlePredicate(k, e, "<=")
      case Contains(k, e)   => handlePredicate(k, Constant(e, Quoted), "LIKE")
      case StartsWith(k, e) => handleWithPredicate(k, e, "StartsWith")
      case EndsWith(k, e)   => handleWithPredicate(k, e, "EndsWith")
      case Or(p1, p2)       => {
        val res1 = translatePred(p1, fields)
        val res2 = translatePred(p2, fields)
        ("((" + res1._1 + ") OR (" + res2._1 + "))", res1._2 + res2._2)
      }
      case And(p1, p2)      => {
        val res1 = translatePred(p1, fields)
        val res2 = translatePred(p2, fields)
        ("((" + res1._1 + ") AND (" + res2._1 + "))", res1._2 + res2._2)
      }
      case Not(p)           => {
        val res = translatePred(p, fields)
        ("NOT " + res._1, res._2)
      }
    }
  }

  def constructFilter(preds: Set[Predicate], fields: Map[String, String], having: Boolean = false): String = {
    def _handleAndOrWithNot(p1: Predicate, p2: Predicate, x: Predicate) =
      p1 match {
        case Not(p1) => {
          val res1 = translatePred(p1, fields)
          val res2 = constructFilter(Set(p2), fields, having)
          (Str("where.not([\"") << res1._1 << "\"" << res1._2 << "])."
            << res2
          ).!
        }
        case _ => {
          val res = translatePred(x, fields)
          (Str("where([\"") << res._1 << "\"" << res._2 << "])").!
        }
      }
    preds map { x => x match {
      case Not(x) => {
        val res = translatePred(x, fields)
        (Str("where.not([\"") << res._1 << "\"" << res._2 << "])").!
      }
      case And(p1, p2) => _handleAndOrWithNot(p1, p2, x)
      case Or(p1, p2) => _handleAndOrWithNot(p1, p2, x)
      case _ => {
        val res = translatePred(x, fields)
        (Str("where([\"") << res._1 << "\"" << res._2 << "])").!
      }
     }
    } mkString(".")
  }

  def constructOffsetLimit(offset: Int, limit: Option[Int]) = limit match {
    case None =>
      if (offset > 0) s"offset($offset)"
      else ""
    case Some(limit) =>
      if (offset > 0)
        s"offset($offset).limit($limit)"
      else s"limit($limit)"
  }

  def constructFieldExpr(fexpr: FieldExpr, fields: Map[String, String], like: Boolean = false): String = fexpr match {
    case F(f) => fields.get(f) match {
      case None     => f.split('.').takeRight(2).mkString(".").toLowerCase()
      case Some(s)  => s
    }
    case Constant(v, UnQuoted) => v
    case Constant(v, Quoted)   => Utils.quoteStr(escapeActiveRecordStr(v, like))
    case Add(e1, e2) => "(" + constructFieldExpr(e1, fields) + "+" + constructFieldExpr(e2, fields) + ")"
    case Sub(e1, e2) => "(" + constructFieldExpr(e1, fields) + "-" + constructFieldExpr(e2, fields) + ")"
    case Mul(e1, e2) => "(" + constructFieldExpr(e1, fields) + "*" + constructFieldExpr(e2, fields) + ")"
    case Div(e1, e2) => "(" + constructFieldExpr(e1, fields) + "/" + constructFieldExpr(e2, fields) + ")"
    case _ => ???
  }

def constructAggrExpr(fexpr: FieldExpr, fields: Map[String, String]) = {
    fexpr match {
      case Count(None)        => "count"
      case Count(Some(field)) => "count(\"" + constructFieldExpr(field, fields) + "\")"
      case Sum(field)         => "sum(\"" + constructFieldExpr(field, fields) + "\")"
      case Avg(field)         => "average(\"" + constructFieldExpr(field, fields) + "\")"
      case Min(field)         => "minimum(\"" + constructFieldExpr(field, fields) + "\")"
      case Max(field)         => "maximum(\"" + constructFieldExpr(field, fields) + "\")"
      case _                  =>  throw new UnsupportedException("compound aggrates not supported") // Revisit
    }
  }

  def constructPrimField(acc: Map[String, String], fexpr: FieldExpr) = {
    val (field, op) = fexpr match {
      case Count(None)        => ("", "count")
      case Count(Some(field)) => (constructField(acc, field), "count")
      case Sum(field)         => (constructField(acc, field), "sum")
      case Avg(field)         => (constructField(acc, field), "avg")
      case Min(field)         => (constructField(acc, field), "min")
      case Max(field)         => (constructField(acc, field), "max")
      case _                  => ??? // Unreachable case
    }
    op + "(" + field + ")"
  }

  def constructField(acc: Map[String, String], fexpr: FieldExpr): String = fexpr match {
    case F(f) => acc.get(f) match {
      case None     => f.split('.').takeRight(2).mkString(".").toLowerCase()
      case Some(s)  => s
    }
    case Constant(v, UnQuoted) => v
    case Constant(v, Quoted)   => Utils.quoteStr(escapeActiveRecordStr(v))
    case Add(e1, e2) => "(" + constructField(acc, e1) + "+" + constructField(acc, e2) + ")"
    case Sub(e1, e2) => "(" + constructField(acc, e1) + "-" + constructField(acc, e2) + ")"
    case Mul(e1, e2) => "(" + constructField(acc, e1) + "*" + constructField(acc, e2) + ")"
    case Div(e1, e2) => "(" + constructField(acc, e1) + "/" + constructField(acc, e2) + ")"
    case _    => constructPrimField(acc, fexpr)
  }

  def extractFields(fields: Map[String, FieldDecl]): Map[String, String] = {
    fields.foldLeft(Map[String, String]()) ({ (acc, x) =>
        x._2 match {
          case FieldDecl(e, as, _, _) => acc + (as -> constructField(acc, e))
        }
      })
  }

  def getDBAliasName(name: String) = target.db match {
    case MySQL(_, _, _) => Utils.quoteStr(name, quotes ="`")
    case _              => Utils.quoteStr(name, quotes = "\\\"")
  }

  def constructSelects(fields: Map[String, String]) =
    fields.foldLeft(List[String]()) { (acc, x) => {
      val (name, expr) = x match { case (k, v) => (k, v) }
      acc :+ "select(" + Utils.quoteStr(s"${expr} as ${getDBAliasName(name)}", "\"") + ")"
    }} mkString(".")

  def constructGroupBy(groupBy: Set[String]) =
    if (groupBy.isEmpty)
      ""
    else {
      "group(" + (
        groupBy map { case x => {
          fieldsMap.get(x) match {
            case None     => Utils.quoteStr(getActiveRecordFieldName(x), "\"")
            case Some(s)  => Utils.quoteStr(s, "\"")
          }
        }} mkString ", ") + ")"
    }

  def constructDistinct(distinct: Option[String]) = distinct match {
    case Some("") => "distinct"
    case Some(x)  => throw new UnsupportedException("Distincts with fields are not supported")
    case _        => ""
  }

  override def constructCombinedQuery(s: State) =
    throw new UnsupportedException("combined queries not supported")

  override def constructNaiveQuery(s: State, first: Boolean, offset: Int,
      limit: Option[Int]) = {
    def filterNonHiddenFieldDecls(i: String) = !FieldDecl.hidden(s.fields.get(i).get)
    val model = s.source
    fieldsMap = extractFields(s.fields)
    nonHiddenFieldsMap = (fieldsMap.view filterKeys filterNonHiddenFieldDecls).toMap
    // satisfy postgres need that ORDER BY columns be part of the SELECT list
    // when DISTINCT is used
    val selectFields = getSelectFields(s, nonHiddenFieldsMap)
    val (aggrP, nonAggrP) = s.preds partition { _.hasAggregate(s.fields) }
    QueryStr(Some("ret" + s.numGen.next().toString),
      Some(Seq(
        model,
        constructJoins(s.getJoinPairs, s.source),
        constructOrderBy(s.orders, fieldsMap),
        constructFilter(nonAggrP, fieldsMap),
        constructFilter(aggrP, fieldsMap, having=true),
        constructOffsetLimit(offset, limit),
        constructSelects(selectFields),
        constructGroupBy(s.getNonConstantGroupingFields),
        constructDistinct(s.distinct),
        if (first) "first" else "all",
      ) filter {
        case "" => false
        case _ => true
      } mkString "."))
  }

  override def unionQueries(s1: State, s2: State) =
    throw new UnsupportedException("unions are not supported")

  override def intersectQueries(s1: State, s2: State) =
    throw new UnsupportedException("unions are not supported")
}
