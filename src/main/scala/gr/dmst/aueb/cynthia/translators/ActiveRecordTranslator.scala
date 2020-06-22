package gr.dmst.aueb.cynthia.translators

import gr.dmst.aueb.cynthia._


case class ActiveRecordTranslator(t: Target) extends Translator(t) {

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
      fields map { as => s"${ident}dump($v.$as, '$as')" } mkString "\n"
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

  def translatePred(pred: Predicate, fields: Map[String, String]): (String, String) = {
    def handlePredicate(k: String, e: FieldExpr, symbol: String, unquoted: Boolean = false): (String, String) = {
      if (!e.isConstant) throw new UnsupportedException("complex where clauses are not supported")
      val key = fields.get(k) match {
          case None     => k.split('.').takeRight(2).mkString(".").toLowerCase()
          case Some(s)  => s
      }
      if (symbol == "LIKE")
        (key + s" $symbol '%${constructFieldExpr(e, fields, unquoted)}%'", "")
      else
        (key + s" $symbol ?", ", " + constructFieldExpr(e, fields, unquoted))
    }
    pred match {
      case Eq(k, e)       => handlePredicate(k, e, "=")
      case Gt(k, e)       => handlePredicate(k, e, ">")
      case Gte(k, e)      => handlePredicate(k, e, ">=")
      case Lt(k, e)       => handlePredicate(k, e, "<")
      case Lte(k, e)      => handlePredicate(k, e, "<=")
      case Contains(k, e) => handlePredicate(k, e, "LIKE", true)
      case Or(p1, p2)     => {
        val res1 = translatePred(p1, fields)
        val res2 = translatePred(p2, fields)
        (res1._1 + " AND " + res2._1, res1._2 + res2._2)
      }
      case And(p1, p2)    => {
        val res1 = translatePred(p1, fields)
        val res2 = translatePred(p2, fields)
        (res1._1 + " AND " + res2._1, res1._2 + res2._2)
      }
      case _ => ("", "")
    }
  }

  def constructFilter(preds: Set[Predicate], fields: Map[String, String], having: Boolean = false): String = {
    preds map { x => x match {
      case Not(x) => {
        val res = translatePred(x, fields)
        (Str("where.not([\"") << res._1 << "\"" << res._2 << "])").!
      }
      case And(p1, p2) => {
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
      }
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

  def constructFieldExpr(fexpr: FieldExpr, fields: Map[String, String], unquoted: Boolean = false): String = fexpr match {
    case F(f) => fields.get(f) match {
      case None     => f.split('.').takeRight(2).mkString(".").toLowerCase()
      case Some(s)  => s
    }
    case Constant(v, UnQuoted) => v
    case Constant(v, Quoted)   => if (unquoted) v else Utils.quoteStr(v) // LIKE case
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

  def getDBAliasName(name: String) = target.db match {
    case MySQL(_, _, _) => Utils.quoteStr(name, quotes ="`")
    case _              => Utils.quoteStr(name, quotes = "\\\"")
  }

  def constructSelects(fields: Map[String, String]) =
    fields.foldLeft(List[String]()) { (acc, x) => {
      val (name, expr) = x match { case (k, v) => (k, v) }
      acc :+ "select(" + Utils.quoteStr(s"$expr as ${getDBAliasName(name)}", "\"") + ")"
    }} mkString(".")

  def constructGroupBy(groupBy: Set[String]) =
    if (groupBy.isEmpty)
      ""
    else
      "group(" + (
        groupBy map { case x => {
          fieldsMap.get(x) match {
            case None     => Utils.quoteStr(getActiveRecordFieldName(x), "\"")
            case Some(s)  => Utils.quoteStr(s, "\"")
          }
        }} mkString ", ") + ")"

  override def constructCombinedQuery(s: State) =
    throw new UnsupportedException("combined queries not supported")

  override def constructNaiveQuery(s: State, first: Boolean, offset: Int,
      limit: Option[Int]) = {
    def filterNonHiddenFieldDecls(i: String) = !FieldDecl.hidden(s.fields.get(i).get)
    val model = s.source
    fieldsMap = extractFields(s.fields)
    nonHiddenFieldsMap = (fieldsMap.view filterKeys filterNonHiddenFieldDecls).toMap
    val (aggrP, nonAggrP) = s.preds partition { _.hasAggregate(s.fields) }
    QueryStr(Some("ret" + s.numGen.next().toString),
      Some(Seq(
        model,
        constructJoins(s.getJoinPairs, s.source),
        constructOrderBy(s.orders, fieldsMap),
        constructFilter(nonAggrP, fieldsMap),
        constructFilter(aggrP, fieldsMap, having=true),
        if (first) "first" else "all",
        constructOffsetLimit(offset, limit),
        constructSelects(nonHiddenFieldsMap),
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
