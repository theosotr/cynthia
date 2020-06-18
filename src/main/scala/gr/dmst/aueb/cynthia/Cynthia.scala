package gr.dmst.aueb.cynthia

import scala.util.{Success, Failure}
import scopt.OParser
import java.nio.file.{Paths, Files}


case class Options (
  schemas: Int = 1,
  nuqueries: Int = 200,
  orms: Seq[String] = Seq(),
  dbs: Seq[String] = Seq("sqlite"),
  predefined: (String, String) = ("", "")
)


object Cynthia {

  def main(args: Array[String]): Unit = {
    val builder = OParser.builder[Options]
    val cliParser = {
      import builder._
      OParser.sequence(
        programName("cynthia"),
        head("cynthia", "0.1"),
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
        opt[Seq[String]]('o', "orms")
          .required()
          .action((x, o) => o.copy(orms = x))
          .validate(_.foldLeft (success) { (acc, x) => x match {
              case "django"  | "sqlalchemy" | "sequelize"
              | "peewee" | "activerecord" => acc
              case _  => failure("ORM '" + x + "' is not supported")
            }
          })
          .text("ORMs to differentially test"),
        opt[Seq[String]]('d', "backends")
          .action((x, o) => o.copy(dbs = o.dbs ++ x))
          .validate(_.foldLeft (success) { (acc, x) => x match {
              case "mysql" | "postgres" => acc
              case "sqlite"             => failure ("SQLite is used by default")
              case _                    => failure ("Database backend '" + x + "' is not supported")
            }
          })
          .text("Database backends to store data (Default Value: sqlite)"),
        opt[(String, String)]('p', "predefined")
          .action((x, o) => o.copy(predefined = x))
          .validate(x => {
            if (!Files.exists(Paths.get(x._1)))
              failure("File " + x._1 + " does not exist")
            else if (!Files.exists(Paths.get(x._2)))
              failure("File " + x._2 + " does not exist")
            else
              success
          })
          .text("Set schema (key) and a file with a Seq of AQL queries (value)"),
        checkConfig(x =>
          if (x.dbs.length + x.orms.length < 2)
            failure(
              "Number of database backends + number of ORMs must be greather than 1")
          else success
        )
      )
    }

    OParser.parse(cliParser, args, Options()) match {
      case Some(options) => Controller(options)
      case _ => println("Wrong arguments")
    }
  }
}
