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
      case SetRes(_) => s"for i in $ret\n\tdump(i.id)\nend"
      case _ => ""
    }
  }

  override def constructCombinedQuery(s: State) = QueryStr(Some("var"))

  override def constructNaiveQuery(s: State, first: Boolean, offset: Int,
      limit: Option[Int]) = QueryStr(Some("var"))

  def getActiveRecordFieldName(field: String, el: Int = 1) =
    if(field.contains(".")) field.split('.').array(el) else field;

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

  def constructFieldExpr(fexpr: FieldExpr): String = fexpr match {
    case Constant(v, UnQuoted) => v
    case Constant(v, Quoted)   => Utils.quoteStr(v)
    case _ => ""
  }

  def translatePredArgs(pred: Predicate): String = pred match {
    case Eq(k, _) =>
      (Str(getActiveRecordFieldName(k, 2)) << " = ?").!
    case _ => ""
  }

  def translatePredVals(pred: Predicate): String = pred match {
    case Eq(_, e) => ", " + constructFieldExpr(e)
    case _ => ""
  }

  def constructFilter(preds: Set[Predicate]) =
    preds map { x =>
      (Str("where([\"") << translatePredArgs(x) << "\"" << translatePredVals(x) << "])").!
    } mkString(".")


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
            if (first) "first" else "all",
            constructFilter(nonAggrP)
          ) filter {
            case "" => false
            case _ => true
          } mkString "."))
  }

  override def unionQueries(s1: State, s2: State) = s1

  override def intersectQueries(s1: State, s2: State) = s1
}
