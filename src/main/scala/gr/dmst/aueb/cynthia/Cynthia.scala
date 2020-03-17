package gr.dmst.aueb.cynthia

import scopt.OParser
import gr.dmst.aueb.cynthia.{DB, Postgres, MySQL, SQLite, DBSetup}
import gr.dmst.aueb.cynthia.Utils


case class Options (
  schemas: String = "",
  orms: Seq[String] = Seq(),
  dbs: Seq[String] = Seq()
)


object Cynthia {
  val dbUser = "orm_testing"
  val dbPass = "orm_testing"

  def genDBList(dbname: Option[String], workdir: String) = {
    val dbnames = dbname match {
      case None         => ("postgres", "sys", workdir)
      case Some(dbname) => (dbname, dbname, workdir)
    }
    List(
      //Postgres(dbUser, dbPass, dbnames._1),
      //MySQL(dbUser, dbPass, dbnames._2),
      SQLite(dbnames._3)
    )
  }

  def genORMList(dbname: String, workdir: String) =
    List(
      Django(dbname,
             Utils.joinPaths(List(workdir, "django")),
             "djangoproject"),
      SQLAlchemy(dbname,
                 Utils.joinPaths(List(workdir, "sqlalchemy")))
    )

  def genAQLQuery() =
    SetRes(Union(New ("Listing", None), New ("Listing", None)))

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
              case "django"     => acc
              case "sqlalchemy" => acc
              case _            => failure("ORM '" + x + "' is not supported")
            }
          })
          .text("ORMs to differentially test"),
        opt[Seq[String]]('d', "backends")
          .action((x, o) => o.copy(dbs = x))
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
              val dbname = schema.replace(".sql", "")
              val schemaPath = Utils.joinPaths(List(options.schemas, schema))
              val dbDir = Utils.joinPaths(List(
                Utils.getDBDir(), dbname))
              genDBList(None, dbDir).foreach { db =>
                DBSetup.createdb(db, dbname) }
              val dbs = genDBList(Some(dbname), dbDir)
              dbs.foreach { db =>
                DBSetup.setupSchema(db, schemaPath) }
              val projectDir = Utils.joinPaths(
                List(Utils.getProjectDir(), dbname))
              Utils.createDir(projectDir)
              val orms = genORMList(dbname, projectDir)
              orms.foreach { orm =>
                ProjectCreator.createProject(orm, dbs)
                ORMTranslator.translate(genAQLQuery(), Target(orm, dbs.head))
              }
            }
          }
        }
      }
      case _ => println("Wrong arguments")
    }
  }
}
