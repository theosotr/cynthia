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
    |  elsif var.is_a? String
    |    puts var.strip
    |  else
    |    puts var
    |  end
    |end
    |""".stripMargin

  override def emitPrint(q: Query, dFields: Seq[String], ret: String) = {
    q match {
      case FirstRes(_) => s"dump($ret.id)"
      case SetRes(_) | SubsetRes(_, _, _) => s"for i in $ret\n\tdump(i.id)\nend"
      case _ => ""
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

  def constructOrderBy(spec: Seq[(String, Order)]) = spec match {
    case Seq() => ""
    case _     =>
      (
        Str("order(") << (
          spec map { x =>
            x match {
              case (k, Desc) => getActiveRecordFieldName(k) + ": :desc"
              case (k, Asc)  => getActiveRecordFieldName(k) + ": :asc"
            }
          } mkString(",")
        ) << ")"
      ).!
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
    // case Not(pred)                  =>
      // (Str("not_(") << translatePred(pred) << ")").!
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

  override def constructQuery(first: Boolean = false, offset: Int = 0,
      limit: Option[Int] = None)(s: State) = {
        println(s)
        val model = s.source
        val (aggrP, nonAggrP) = s.preds partition { _.hasAggregate(s.fields) }
        println(nonAggrP)
        QueryStr(Some("ret" + s.numGen.next().toString),
          Some(Seq(
            model,
            constructJoins(s.joins, s.source),
            constructOrderBy(s.orders),
            constructFilter(nonAggrP),
            if (first) "first" else "all",
            constructOffsetLimit(offset, limit)
          ) filter {
            case "" => false
            case _ => true
          } mkString "."))
  }

  override def unionQueries(s1: State, s2: State) = s1

  override def intersectQueries(s1: State, s2: State) = s1
}
