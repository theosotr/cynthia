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
  sql: Option[String] = None,
  aql: Option[String] = None,
  dotCynthia: String = ".cynthia",
  schema: Option[String] = None,
  storeMatches: Boolean = false,
  all: Boolean = false,
  mismatches: Seq[Int] = Seq(),
  minDepth: Int = 5,
  maxDepth: Int = 25,
  dbUser: String = "orm_testing",
  dbPass: String = "orm_testing",
  timeout: Option[Int] = None,
  onlyWorkDir: Boolean = false
)


object Cynthia {

  def main(args: Array[String]): Unit = {
    val builder = OParser.builder[Options]
    val cliParser = new scopt.OptionParser[Options]("cynthia") {
      import builder._
      head("cynthia", "0.1")

      // General options
      def ormsOption() =
        opt[Seq[String]]('o', "orms")
          .required
          .action((x, o) => o.copy(orms = x))
          .text("ORMs to differentially test")
          .validate(_.foldLeft (success) { (acc, x) => x match {
              case "django"  | "sqlalchemy" | "sequelize"
              | "peewee" | "activerecord" => acc
              case _  => failure("ORM '" + x + "' is not supported")
            }
          })

      def backendsOption() =
        opt[Seq[String]]('d', "backends")
          .action((x, o) => o.copy(dbs = o.dbs ++ x))
          .text("Database backends to store data (Default Value: sqlite)")
          .validate(_.foldLeft (success) { (acc, x) => x match {
              case "mysql" | "postgres" | "cockroachdb" | "mssql" | "oracle" => acc
              case "sqlite"             => failure ("SQLite is used by default")
              case _                    => failure ("Database backend '" + x + "' is not supported")
            }
          })

      def storeMatchesOption() =
        opt[Unit]('S', "store-matches")
          .action((x, o) => o.copy(storeMatches = true))
          .text("Save matches")

      def noCombineOption() =
        opt[Unit]("no-combined")
          .action((_, c) => c.copy(noCombined = true))
          .text("Don't generate combined queries")

      def recordsOption() =
        opt[Int]('r', "records")
          .action((x, o) => o.copy(records = x))
          .text("Number of records to generate for each table")
          .validate(x => {
            if (x < 1) failure("You must generate at least one record")
            else success
          })

      def minDepthOption() =
        opt[Int]("min-depth")
          .action((x, o) => o.copy(minDepth = x))
          .text("Minimum depth of generated AQL queries")
          .validate(x =>
              if (x < 1) failure("Minimum depth must be greater than 1")
              else success)

      def maxDepthOption() =
        opt[Int]("max-depth")
          .action((x, o) => o.copy(maxDepth = x))
          .text("Maximum depth of generated AQL queries")
          .validate(x =>
              if (x < 1) failure("Maximum depth must be greater than 1")
              else success)

      note("\n")

      // Sub-commands
      cmd("test") action { (_, c) => c.copy(mode = Some("test")) } children(
        opt[Int]('n', "queries")
          .action((x, o) => o.copy(nuqueries = x))
          .text("Number of queries to generate for each schema (Default value: 200)")
          .validate(x =>
            if (x < 1) failure("You must generate at least one query")
            else success
          ),
        opt[Int]('s', "schemas")
          .action((x, o) => o.copy(schemas = x))
          .text("Number of schemas to generate (Default value: 1)")
          .validate(x =>
            if (x < 1) failure("You must generate at least one schema")
            else success
          ),
        opt[Int]("timeout")
          .action((x, o) => o.copy(timeout = Some(x)))
          .text("Timeout for testing in seconds")
          .validate( x =>
            if (x < 1) failure("Timeout must be greater than 1")
            else success
          ),
        ormsOption,
        backendsOption,
        storeMatchesOption,
        noCombineOption,
        recordsOption,
        minDepthOption,
        maxDepthOption
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
        noCombineOption,
        recordsOption,
        minDepthOption,
        maxDepthOption
      )
      cmd("replay") action { (_, c) => c.copy(mode = Some("replay")) } children(
        opt[String]('c', "cynthia")
          .action((x, o) => o.copy(dotCynthia = x))
          .text("cynthia directory for replaying missmatches (default .cynthia)"),
        opt[String]('s', "schema")
          .action((x, o) => o.copy(schema = Some(x)))
          .text("schema to replay"),
        opt[Unit]('a', "all")
          .action((x, o) => o.copy(all = true))
          .text("Replay all queries. Always use it with --store-matches to not remove matches queries"),
        opt[Seq[Int]]('m', "mismatches")
          .action((x, o) => o.copy(mismatches = x))
          .text("Mismatches to replay")
          .validate(_.foldLeft (success) { (acc, x) => x match {
              case _ => acc
            }
          }),
        ormsOption,
        backendsOption,
        storeMatchesOption
      )
      cmd("run") action { (_, c) => c.copy(mode = Some("run")) } children(
        opt[String]('s', "sql")
          .required()
          .action((x, o) => o.copy(sql = Some(x)))
          .text("File with the sql script to generate and feed the Database")
          .validate(x => {
            if (!Files.exists(Paths.get(x)))
              failure("File " + x + " does not exist")
            else success
          }),
        opt[String]('a', "aql")
          .required()
          .action((x, o) => o.copy(aql = Some(x)))
          .text("A file with an AQL query or a directory with many AQL queries")
          .validate(x => {
            if (!Files.exists(Paths.get(x)))
              failure("File or directory " + x + " does not exist")
            else success
          }),
          ormsOption,
          backendsOption,
          storeMatchesOption
        )
      cmd("inspect") action { (_, c) => c.copy(mode = Some("inspect")) } children(
        opt[String]('c', "cynthia")
          .action((x, o) => o.copy(dotCynthia = x))
          .text("cynthia directory for inspecting missmatches (default .cynthia)"),
        opt[String]('s', "schema")
          .action((x, o) => o.copy(schema = Some(x)))
          .text("schema to inspect"),
        opt[Seq[Int]]('m', "mismatches")
          .action((x, o) => o.copy(mismatches = x))
          .text("mismatches to inspect")
        )
      cmd("clean") action { (_, c) => c.copy(mode = Some("clean")) } children(
        opt[Unit]("only-workdir")
          .action((x, o) => o.copy(onlyWorkDir = true))
          .text("Clean only the working directory .cynthia")
        )
      checkConfig(x =>
        x.mode match {
          case Some("test") =>
            if (x.dbs.length + x.orms.length < 3)
              failure(
                "Number of database backends + number of ORMs must be greather than 2.")
            else success
          case Some("generate") => success
          case Some("run")      => success
          case Some("replay")   =>
            if (!Files.exists(Paths.get(x.dotCynthia)))
              failure("Directory " + x.dotCynthia + " does not exist")
            else if (!x.schema.isEmpty && !Files.exists(Paths.get(Utils.joinPaths(List(x.dotCynthia, "schemas", x.schema.get)))))
              failure("Schema " + x.schema.get + " does not exist")
            else if (x.schema.isEmpty && !x.mismatches.isEmpty)
              failure("You cannot use --mismatches option without --schema option")
            else if (x.dbs.length + x.orms.length < 3)
              failure(
                "Number of database backends + number of ORMs must be greather than 2.")
            else success
          case Some("inspect") => success
          case Some("clean") => success
          case _ =>
            failure("A sub-command is required.")
        }
      )
    }

    cliParser.parse(args, Options()) map { options =>
      Controller(options)
    } getOrElse {
      // Wrong arguments
    }
  }
}
