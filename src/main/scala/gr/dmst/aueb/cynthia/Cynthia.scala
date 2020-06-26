package gr.dmst.aueb.cynthia

import java.nio.file.{Paths, Files}
import scala.util.{Success, Failure}
import scopt.OParser


case class Options (
  mode: Option[String] = None,
  schemas: Int = 1,
  nuqueries: Int = 200,
  records: Int = 20,
  orms: Seq[String] = Seq(),
  noCombined: Boolean = false,
  dbs: Seq[String] = Seq("sqlite"),
  sql: String = "",
  aql: String = "",
  dotCynthia: String = ".cynthia",
  schema: String = "",
  storeMatches: Boolean = false,
  all: Boolean = false,
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

      opt[Unit]("no-combined")
        .action((_, c) => c.copy(noCombined = true))
        .text("Don't generate combined queries")

      opt[Int]('r', "records")
        .action((x, o) => o.copy(records = x))
        .text("Number of records to generate for each table")
        .validate(x => {
          if (x < 1) failure("You must generate at least one record")
          else success
        })

      note("\n")
      help("help") text("prints this usage text")

      // Sub-commands
      cmd("test") action { (_, c) => c.copy(mode = Some("test")) } children(
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
        opt[Unit]("no-combined")
          .action((_, c) => c.copy(noCombined = true))
          .text("Don't generate combined queries"),
        opt[Int]('r', "records")
          .action((x, o) => o.copy(records = x))
          .text("Number of records to generate for each table")
          .validate(x => {
            if (x < 1) failure("You must generate at least one record")
            else success
          })
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
        opt[Unit]("no-combined")
          .action((_, c) => c.copy(noCombined = true))
          .text("Don't generate combined queries"),
        opt[Int]('r', "records")
          .action((x, o) => o.copy(records = x))
          .text("Number of records to generate for each table")
          .validate(x => {
            if (x < 1) failure("You must generate at least one record")
            else success
          })
      )
      cmd("replay") action { (_, c) => c.copy(mode = Some("replay")) } children(
        opt[String]('c', "cynthia")
          .action((x, o) => o.copy(dotCynthia = x))
          .text("cynthia directory for replaying missmatches (Default .cynthia)"),
        opt[String]('s', "schema")
          .action((x, o) => o.copy(schema = x))
          .text("Schema to replay"),
        opt[Unit]('a', "all")
          .action((x, o) => o.copy(all = true))
          .text("Replay all queries. Always use it with --store-matches to not remove matches queries"),
        opt[Seq[Int]]('m', "mismatches")
          .action((x, o) => o.copy(mismatches = x))
          .text("Mismatches to replay")
          .validate(_.foldLeft (success) { (acc, x) => x match {
              case _ => acc
            }
          })
      )
      cmd("run") action { (_, c) => c.copy(mode = Some("run")) } children(
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
          case Some("test") =>
            if (x.orms.isEmpty)
              failure("You must give at least one orm with --orms option")
            else if (x.dbs.length + x.orms.length < 3)
              failure(
                "Number of database backends + number of ORMs must be greather than 2.")
            else
              success
          case Some("generate") =>
            success
          case Some("run") =>
            if (x.orms.isEmpty)
              failure("You must give at least one orm with --orms option")
            else
              success
          case Some("replay") =>
            if (!Files.exists(Paths.get(x.dotCynthia)))
              failure("Directory " + x.dotCynthia + " does not exist")
            else if (!x.schema.isEmpty && !Files.exists(Paths.get(x.dotCynthia + "/schemas/" + x.schema)))
              failure("Schema " + x.schema + " does not exist")
            else if (x.schema.isEmpty && !x.mismatches.isEmpty)
              failure("You cannot use --mismatches option without --schema option")
            else if (x.orms.isEmpty)
              failure("You must give at least one orm with --orms option")
            else if (x.dbs.length + x.orms.length < 3)
              failure(
                "Number of database backends + number of ORMs must be greather than 2.")
            else
              success
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
