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
    }
  }

  override def constructCombinedQuery(s: State) = QueryStr(Some("var"))

  override def constructNaiveQuery(s: State, first: Boolean, offset: Int,
      limit: Option[Int]) = QueryStr(Some("var"))

  def getActiveRecordFieldName(field: String) =
    field.split('.').array(1);

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

  override def constructQuery(first: Boolean = false, offset: Int = 0,
      limit: Option[Int] = None)(s: State) = {
        val model = s.source
        QueryStr(Some("ret" + s.numGen.next().toString),
          Some(Seq(
            model,
            if (first) "first" else "all",
            constructOrderBy(s.orders)
          ) filter {
            case "" => false
            case _ => true
          } mkString "."))
  }

  override def unionQueries(s1: State, s2: State) = s1

  override def intersectQueries(s1: State, s2: State) = s1
}
