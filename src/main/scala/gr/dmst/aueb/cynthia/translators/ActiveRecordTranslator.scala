package gr.dmst.aueb.cynthia.translators

import gr.dmst.aueb.cynthia._


case class ActiveRecordTranslator(t: Target) extends Translator(t) {

  val dbsettings = target.db match {
    case Postgres(user, password, dbname) =>
      Str("\tadapter: 'postgres',\n") <<
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
    case SQLite(dbname) =>
      Str("\tadapter: 'sqlite3',\n") <<
        "\tdatabase: " << Utils.quoteStr(dbname)
  }

  override val preamble =
    s"""require 'active_record'
    |Dir[File.join(__dir__, 'models', '*.rb')].each {|file| require_relative file }
    |
    |ActiveRecord::Base.establish_connection(
    |${dbsettings.!}
    |)

    |def dump(var)
    |  if var.is_a? Integer
    |    puts "%0.2f" % [var]
    |  elsif var.is_a? Numeric
    |    puts "%0.2f" % [var]
    |  elsif var.is_a? String
    |    puts var.strip
    |  elsif var == nil
    |    puts "None"
    |  else
    |    puts var
    |  end
    |end
    |""".stripMargin

  override def emitPrint(q: Query, dFields: Seq[String], ret: String) = {
    def _dumpField(v: String, fields: Iterable[String], ident: String = "") =
      fields map { as => s"${ident}dump($v.$as)" } mkString "\n"
    q match {
      case FirstRes(_) => _dumpField(ret, dFields)
      case SetRes(_) | SubsetRes(_, _, _) =>
        s"for i in $ret\n${_dumpField("i", dFields, ident = " " * 2)}\nend"
      case AggrRes (aggrs, _) => {
        aggrs map { x => x match {
          case x => {
            val (qfield, _, _) = x match { case FieldDecl(f, l, t, _) => (f, l, t) }
            s"dump($ret." + constructAggrExpr(qfield) + ")"
          }
        } } mkString("\n")
      }
    }
  }

  override def constructCombinedQuery(s: State) = QueryStr(Some("var"))

  override def constructNaiveQuery(s: State, first: Boolean, offset: Int,
      limit: Option[Int]) = QueryStr(Some("var"))

  def getActiveRecordFieldName(field: String) =
    if (field.contains(".")) field.split('.').array(field.count(_ == '.')) else
      field

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
      val mpamies = g(source) map {
        x => dfs(g, x)
      }
      "joins(" + (mpamies mkString(",")) + ")"
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

  def translatePred(pred: Predicate): (String, String) = pred match {
    case Eq(k, e) =>
      ((Str(getActiveRecordFieldName(k)) << " = ?").!, ", " + constructFieldExpr(e))
    case Gt(k, e) =>
      ((Str(getActiveRecordFieldName(k)) << " > ?").!, ", " + constructFieldExpr(e))
    case Gte(k, e) =>
      ((Str(getActiveRecordFieldName(k)) << " >= ?").!, ", " + constructFieldExpr(e))
    case Lt(k, e) =>
      ((Str(getActiveRecordFieldName(k)) << " < ?").!, ", " + constructFieldExpr(e))
    case Lte(k, e) =>
      ((Str(getActiveRecordFieldName(k)) << " <= ?").!, ", " + constructFieldExpr(e))
    case Contains(k, e) =>
      ((Str(getActiveRecordFieldName(k)) << " LIKE ?").!, ", '%" + constructFieldExpr(e, true) + "%'")
   case Or(p1, p2) =>
      val res1 = translatePred(p1)
      val res2 = translatePred(p2)
      (res1._1 + " AND " + res2._1, res1._2 + res2._2)
    case And(p1, p2) => {
      val res1 = translatePred(p1)
      val res2 = translatePred(p2)
      (res1._1 + " AND " + res2._1, res1._2 + res2._2)
    }
    case _ => ("", "")
  }

  def constructFilter(preds: Set[Predicate]): String = {
    preds map { x => x match {
      case Not(x) => {
        val res = translatePred(x)
        (Str("where.not([\"") << res._1 << "\"" << res._2 << "])").!
      }
      case And(p1, p2) => {
        p1 match {
          case Not(p1) => {
            val res1 = translatePred(p1)
            val res2 = constructFilter(Set(p2))
            (Str("where.not([\"") << res1._1 << "\"" << res1._2 << "])."
              << res2
            ).!
          }
          case _ => {
            val res = translatePred(x)
            (Str("where([\"") << res._1 << "\"" << res._2 << "])").!
          }
        }
      }
      case _ => {
        val res = translatePred(x)
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

  def constructFieldExpr(fexpr: FieldExpr, unquoted: Boolean = false): String = fexpr match {
    case F(f)                  => getActiveRecordFieldName(f)
    case Constant(v, UnQuoted) => if (unquoted) v else Utils.quoteStr(v)
    case Constant(v, Quoted)   => if (unquoted) v else Utils.quoteStr(v)
    // case _    =>
      // if (!fexpr.compound) constructPrimAggr(fexpr)
      /* else constructCompoundAggr(fexpr) */
    case _ => ""
  }

def constructAggrExpr(fexpr: FieldExpr) = {
    fexpr match {
      case Count(None)        => "count"
      case Count(Some(field)) => "count(\"" + constructFieldExpr(field) + "\")"
      case Sum(field)         => "sum(\"" + constructFieldExpr(field) + "\")"
      case Avg(field)         => "average(\"" + constructFieldExpr(field) + "\")"
      case Min(field)         => "minimum(\"" + constructFieldExpr(field) + "\")"
      case Max(field)         => "maximum(\"" + constructFieldExpr(field) + "\")"
      case _                  =>  throw new UnsupportedException("unions are not supported") // Revisit
    }
  }

  def constructAggr(aggrs: Seq[FieldDecl]) = aggrs match {
    case Seq()  => ""
    case Seq(x) => {
      val (qfield, as, t) = x match { case FieldDecl(f, l, t, _) => (f, l, t) }
      constructAggrExpr(qfield)
    }
    case _      => ""
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
    case Constant(v, Quoted)   => Utils.quoteStr(v)
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

  def constructSelects(fields: Map[String, String]) =
    fields.foldLeft(List[String]()) { (acc, x) => {
      val (name, expr) = x match { case (k, v) => (k, v) }
      acc :+ "select(" + Utils.quoteStr(s"$expr as $name", "\"") + ")"
    }} mkString(".")

  def constructGroupBy(groupBy: Set[String]) =
    if (groupBy.isEmpty)
      ""
    else
      "group(" + (
        groupBy map { case x => {
          Utils.quoteStr(getActiveRecordFieldName(x), "\"")
        }} mkString ", ") + ")"

  override def constructQuery(first: Boolean = false, offset: Int = 0,
      limit: Option[Int] = None)(s: State) = {
        println(s)
        val model = s.source
        val fields = extractFields(s.fields)
        println(fields)
        println(s.getNonConstantGroupingFields)
        val (aggrP, nonAggrP) = s.preds partition { _.hasAggregate(s.fields) }
        QueryStr(Some("ret" + s.numGen.next().toString),
          Some(Seq(
            model,
            constructJoins(s.joins, s.source),
            constructOrderBy(s.orders, fields),
            constructFilter(nonAggrP),
            if (first) "first" else "all",
            constructOffsetLimit(offset, limit),
            constructSelects(fields),
            constructGroupBy(s.getNonConstantGroupingFields),
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
