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

package cynthia

import java.nio.file.{Paths, Files}
import scala.util.{Success, Failure}

import scopt.OParser

import cynthia.utils.Utils


case class Options (
  mode: Option[String] = None,
  schemas: Int = 1,
  nuqueries: Int = 200,
  records: Int = 20,
  orms: Seq[String] = Seq(),
  noCombined: Boolean = true,
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
  wellTyped: Boolean = true,
  dbUser: String = "orm_testing",
  dbPass: String = "0rm_test1ng",
  timeout: Option[Int] = None,
  onlyWorkDir: Boolean = false,
  solverGen: Boolean = false,
  solverTimeout: Int = 5000,
  regenerateData: Boolean = false,
  onlyConstrainedQ: Boolean = false,
  randomSeed: Long = System.currentTimeMillis()
) {

  def generateData =
    if (regenerateData) true
    else
      mode match {
      case Some("test") | Some("generate") => true
      case _ => false
    }

  def failure(msg: String) =
    Failure(new Exception(msg))

  def success =
    Success(())

  def validateSolverOption() =
    if (solverGen && !wellTyped)
      failure(
        "The option --solver cannot be used with the option --no-well-typed"
      )
    else success

  def validateOptions() =
    mode match {
      case Some("test") =>
        if (dbs.length + orms.length < 3)
          failure(
            "Number of database backends + number of ORMs must be greather than 2.")
        else success
      case Some("generate") => success
      case Some("run")      => success
      case Some("replay")   =>
        if (!Files.exists(Paths.get(dotCynthia)))
          failure("Directory " + dotCynthia + " does not exist")
        else if (!schema.isEmpty &&
            !Files.exists(
              Paths.get(
                Utils.joinPaths(List(dotCynthia, "schemas", schema.get))
              )
            ))
          failure("Schema " + schema.get + " does not exist")
        else if (schema.isEmpty && !mismatches.isEmpty)
          failure("You cannot use --mismatches option without --schema option")
        else if (dbs.length + orms.length < 3)
          failure(
            "Number of database backends + number of ORMs must be greather than 2.")
        else success
      case Some("inspect") => success
      case Some("clean") => success
      case _ =>
        failure("A sub-command is required.")
    }
}


object OptionParser {
  val builder = OParser.builder[Options]
  val cliParser = new scopt.OptionParser[Options]("cynthia") {
    import builder._
    head("cynthia", "0.1")

    // General options
    def ormsOption() =
      opt[Seq[String]]('o', "orms")
        .required()
        .action((x, o) => o.copy(orms = x))
        .text("ORMs to differentially test")
        .validate(_.foldLeft (success) { (acc, x) => x match {
            case "django"  | "sqlalchemy" | "sequelize"
            | "peewee" | "activerecord" | "pony" => acc
            case _  => failure("ORM '" + x + "' is not supported")
          }
        })

    def backendsOption() =
      opt[Seq[String]]('d', "backends")
        .action((x, o) => o.copy(dbs = o.dbs ++ x))
        .text("Database backends to store data (default value: sqlite)")
        .validate(_.foldLeft (success) { (acc, x) => x match {
            case "mysql" | "postgres" | "cockroachdb" | "mssql" => acc
            case "sqlite" => failure ("SQLite is used by default")
            case _ => failure ("Database backend '" + x + "' is not supported")
          }
        })

    def dbUserOption() =
      opt[String]('u', "db-user")
        .action((x, o) => o.copy(dbUser = x))
        .text("The username to log in the database")

    def dbPassOption() =
      opt[String]('p', "db-pass")
        .action((x, o) => o.copy(dbPass = x))
        .text("The password used to log in the database")

    def storeMatchesOption() =
      opt[Unit]('S', "store-matches")
        .action((x, o) => o.copy(storeMatches = true))
        .text("Save matches into the 'sessions' directory")

    def combinedOption() =
      opt[Unit]("combined")
        .action((_, c) => c.copy(noCombined = false))
        .text("Generate AQL queries consting of other simpler queries")

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
        .text("Minimum depth of the generated AQL queries")
        .validate(x =>
            if (x < 1) failure("Minimum depth must be greater than 1")
            else success)

    def maxDepthOption() =
      opt[Int]("max-depth")
        .action((x, o) => o.copy(maxDepth = x))
        .text("Maximum depth of the generated AQL queries")
        .validate(x =>
            if (x < 1) failure("Maximum depth must be greater than 1")
            else success)

    def wellTypedOption() =
      opt[Unit]("no-well-typed")
        .action((_, c) => c.copy(wellTyped = false))
        .text("Generate AQL queries that are type incorrect")

    def solverOption() =
      opt[Unit]("solver")
        .action((_, c) => c.copy(solverGen = true))
        .text("Generate database records through a solver-based approach")

    def solverTimeoutOption() =
      opt[Int]("solver-timeout")
        .action((x, o) => o.copy(solverTimeout = x))
        .text("Solver timeout for each query")
        .validate(x =>
            if (x <= 0) failure("Solver timeout must be greater than zero")
            else success)

    def randomSeed() =
      opt[Int]("random-seed")
        .action((x, o) => o.copy(randomSeed = x))
        .text("Make the testing procedure deterministic by giving a random seed")

    def onlyConstrainedQ() =
      opt[Unit]("only-constrained-queries")
        .action((x, o) => o.copy(onlyConstrainedQ = true))
        .text("Generate only constrained queries")

    note("Cynthia: Data-Oriented Differential Testing of Object-Relational Mapping Systems\n")
    help("help").text("Prints this usage text")
    version("version").text("Prints the current tool's version")


    // Sub-commands
    cmd("test") action { (_, c) => c.copy(mode = Some("test")) } children(
      opt[Int]('n', "queries")
        .action((x, o) => o.copy(nuqueries = x))
        .text("Number of queries to generate for each schema (default value: 200)")
        .validate(x =>
          if (x < 1) failure("You must generate at least one query")
          else success
        ),
      opt[Int]('s', "schemas")
        .action((x, o) => o.copy(schemas = x))
        .text("Number of schemas to generate (default value: 1)")
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
      ormsOption(),
      backendsOption(),
      dbUserOption(),
      dbPassOption(),
      storeMatchesOption(),
      combinedOption(),
      recordsOption(),
      minDepthOption(),
      maxDepthOption(),
      wellTypedOption(),
      solverOption(),
      solverTimeoutOption(),
      randomSeed(),
      onlyConstrainedQ()
    )
    cmd("generate") action { (_, c) => c.copy(mode = Some("generate")) } children(
      opt[Int]('n', "queries")
        .action((x, o) => o.copy(nuqueries = x))
        .text("Number of queries to generate for each schema (default value: 200)")
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
      combinedOption(),
      recordsOption(),
      minDepthOption(),
      maxDepthOption(),
      wellTypedOption(),
      solverOption(),
      solverTimeoutOption(),
      randomSeed(),
      onlyConstrainedQ()
    )
    cmd("replay") action { (_, c) => c.copy(mode = Some("replay")) } children(
      opt[String]('c', "cynthia")
        .action((x, o) => o.copy(dotCynthia = x))
        .text("The cynthia directory for replaying missmatches (default value: .cynthia)"),
      opt[String]('s', "schema")
        .action((x, o) => o.copy(schema = Some(x)))
        .text("schema to replay"),
      opt[Unit]('a', "all")
        .action((x, o) => o.copy(all = true))
        .text("Replay all queries."),
      opt[Seq[Int]]('m', "mismatches")
        .action((x, o) => o.copy(mismatches = x))
        .text("Replay queries for which ORM previously produced different results")
        .validate(_.foldLeft (success) { (acc, x) => x match {
            case _ => acc
          }
        }),
      opt[Unit]("generate-data")
        .action((x, o) => o.copy(regenerateData = true))
        .text("Re-generate data while replaying testing sessions"),
      ormsOption(),
      backendsOption(),
      dbUserOption(),
      dbPassOption(),
      recordsOption(),
      solverOption(),
      solverTimeoutOption(),
      randomSeed()
    )
    cmd("run") action { (_, c) => c.copy(mode = Some("run")) } children(
      opt[String]('s', "sql")
        .required()
        .action((x, o) => o.copy(sql = Some(x)))
        .text("File with the sql script to generate and feed the database")
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
      ormsOption(),
      backendsOption(),
      dbUserOption(),
      dbPassOption(),
      storeMatchesOption()
    )
    cmd("inspect") action { (_, c) => c.copy(mode = Some("inspect")) } children(
      opt[String]('c', "cynthia")
        .action((x, o) => o.copy(dotCynthia = x))
        .text("The cynthia directory for inspecting missmatches (default value: .cynthia)"),
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
        .text("Clean only the working directory '.cynthia'"),
      dbUserOption(),
      dbPassOption()
    )

    checkConfig(x => x.validateOptions() match {
      case Failure(e) => failure(e.getMessage)
      case Success(_) => success
    })
  }
}
