package gr.dmst.aueb.cynthia

import scala.util.{Success, Failure}
import scopt.OParser
import java.nio.file.{Paths, Files}


case class Options (
  mode: Option[String] = None,
  schemas: Int = 1,
  nuqueries: Int = 200,
  orms: Seq[String] = Seq(),
  dbs: Seq[String] = Seq("sqlite"),
  sql: String = "",
  aql: String = "",
  dotCynthia: String = "",
  storeMatches: Boolean = false,
  mismatches: Seq[Int] = Seq()
)


object Cynthia {

  def main(args: Array[String]): Unit = {
    val builder = OParser.builder[Options]
    val cliParser = new scopt.OptionParser[Options]("cynthia") {
      import builder._
      head("cynthia", "0.1")

      // General options
      opt[Seq[String]]('o', "orms")
        .action((x, o) => o.copy(orms = x))
        .text("ORMs to differentially test")
        .validate(_.foldLeft (success) { (acc, x) => x match {
            case "django"  | "sqlalchemy" | "sequelize"
            | "peewee" | "activerecord" => acc
            case _  => failure("ORM '" + x + "' is not supported")
          }
        })

      opt[Seq[String]]('d', "backends")
        .action((x, o) => o.copy(dbs = o.dbs ++ x))
        .text("Database backends to store data (Default Value: sqlite)")
        .validate(_.foldLeft (success) { (acc, x) => x match {
            case "mysql" | "postgres" => acc
            case "sqlite"             => failure ("SQLite is used by default")
            case _                    => failure ("Database backend '" + x + "' is not supported")
          }
        })

      opt[Unit]('S', "store-matches")
        .action((x, o) => o.copy(storeMatches = true))
        .text("Save matches")

      note("\n")
      help("help") text("prints this usage text")

      // Sub-commands
      cmd("auto") action { (_, c) => c.copy(mode = Some("auto")) } children(
        opt[Int]('n', "queries")
          .action((x, o) => o.copy(nuqueries = x))
          .text("Number of queries to generate for each schema (Default value: 200)")
          .validate(x => {
            if (x < 1) failure("You must generate at least one query")
            else success
          }),
        opt[Int]('s', "schemas")
          .action((x, o) => o.copy(schemas = x))
          .text("Number of schemas to generate (Default value: 1)")
          .validate(x => {
            if (x < 1) failure("You must generate at least one schema")
            else success
          }),
      )
      cmd("generate") action { (_, c) => c.copy(mode = Some("generate")) } children(
        opt[Int]('n', "queries")
          .action((x, o) => o.copy(nuqueries = x))
          .text("Number of queries to generate for each schema (Default value: 200)")
          .validate(x => {
            if (x < 1) failure("You must generate at least one query")
            else success
          }),
        opt[Int]('s', "schemas")
          .action((x, o) => o.copy(schemas = x))
          .text("Number of schemas to generate (Default value: 1)")
          .validate(x => {
            if (x < 1) failure("You must generate at least one schema")
            else success
          }),
      )
      cmd("replay") action { (_, c) => c.copy(mode = Some("replay")) } children(
        opt[String]('c', "cynthia")
          .required()
          .action((x, o) => o.copy(dotCynthia = x))
          .text(".cynthia directory for replaying missmatches")
          .validate(x => {
            if (!Files.exists(Paths.get(x)))
              failure("Directory " + x + " does not exist")
            else success
          }),
        opt[Seq[Int]]('m', "mismatches")
          .action((x, o) => o.copy(mismatches = x))
          .text("Mismatches to replay")
          .validate(_.foldLeft (success) { (acc, x) => x match {
              case _ => acc
            }
          })
      )
      cmd("select") action { (_, c) => c.copy(mode = Some("select")) } children(
        opt[String]('s', "sql")
          .required()
          .action((x, o) => o.copy(sql = x))
          .text("File with the sql script to generate and feed the Database")
          .validate(x => {
            if (!Files.exists(Paths.get(x)))
              failure("File " + x + " does not exist")
            else success
          }),
        opt[String]('a', "aql")
          .required()
          .action((x, o) => o.copy(aql = x))
          .text("A file with an AQL query or a directory with many AQL queries")
          .validate(x => {
            if (!Files.exists(Paths.get(x)))
              failure("File or directory " + x + " does not exist")
            else success
          }),
      )
      cmd("clean") action { (_, c) => c.copy(mode = Some("clean")) }
      checkConfig(x =>
        x.mode match {
          case Some("auto") =>
            if (x.orms.isEmpty)
              failure("You must give at least one orm with --orms option")
            if (x.dbs.length + x.orms.length < 3)
              failure(
                "Number of database backends + number of ORMs must be greather than 2.")
            else
              success
          case Some("generate") =>
            failure("Sub-command generate is not implemented")
          case Some("select") =>
            if (x.orms.isEmpty)
              failure("You must give at least one orm with --orms option")
            success
          case Some("replay") =>
            if (x.orms.isEmpty)
              failure("You must give at least one orm with --orms option")
            failure("Sub-command replay is not implemented")
          case Some("clean") =>
            success
          case _ =>
            failure("A sub-command is required.")
        }
      )
    }

    cliParser.parse(args, Options()) map { options =>
      Controller(options)
    } getOrElse {
      println("Wrong arguments")
    }

  }
}
