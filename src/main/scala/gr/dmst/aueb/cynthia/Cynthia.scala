package gr.dmst.aueb.cynthia

import scala.util.{Success, Failure}
import scopt.OParser
import gr.dmst.aueb.cynthia.{DB, Postgres, MySQL, SQLite, DBSetup}
import gr.dmst.aueb.cynthia.Utils


case class Options (
  schemas: String = "",
  orms: Seq[String] = Seq(),
  dbs: Seq[String] = Seq("sqlite")
)


object Cynthia {

  def main(args: Array[String]): Unit = {
    val builder = OParser.builder[Options]
    val cliParser = {
      import builder._
      OParser.sequence(
        programName("cynthia"),
        head("cynthia", "0.1"),
        opt[String]('s', "schemas")
          .required()
          .action((x, o) => o.copy(schemas = x))
          .text("Path to database schemas"),
        opt[Seq[String]]('o', "orms")
          .required()
          .action((x, o) => o.copy(orms = x))
          .validate(_.foldLeft (success) { (acc, x) => x match {
              case "django"  | "sqlalchemy" | "sequelize" | "activerecord" => acc
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
          .text("Database backends to store data"),
        checkConfig(x =>
          if (x.dbs.length + x.orms.length < 2)
            failure(
              "Number of database backends + number of ORMs must be greather than 1")
          else success
        )
      )
    }

    OParser.parse(cliParser, args, Options()) match {
      case Some(options) => {
        Utils.listFiles(options.schemas) match {
          case None        => println("No .sql file found in " + options.schemas)
          case Some (list) =>
            Utils.setWorkDir()
            list.foreach { schema => {
              TestRunnerCreator(options, schema) match {
                case Success(testRunner) => testRunner.start()
                case Failure(e)          => println(e.getMessage)
              }
            }
          }
        }
      }
      case _ => println("Wrong arguments")
    }
  }
}
