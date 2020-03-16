package gr.dmst.aueb.cynthia

import scopt.OParser
import gr.dmst.aueb.cynthia.{DB, Postgres, MySQL, SQLite, DBSetup}
import gr.dmst.aueb.cynthia.Utils


case class Options (
  schemas: String = ""
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
    SetRes(Union(New ("Listings", None), New ("Listings", None)))

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
          .text("Path to database schemas")
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
              orms.foreach { orm => ProjectCreator.createProject(orm, dbs) }
              ORMTranslator.translate (genAQLQuery(), Target(orms.head, dbs.head))
            }
          }
        }
      }
      case _ => println("Wrong arguments")
    }
  }
}
