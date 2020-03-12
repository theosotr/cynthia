package gr.dmst.aueb.cynthia

import scopt.OParser
import gr.dmst.aueb.cynthia.{BaseDatabase, Postgres, MySQL, SQLite}
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
      Postgres(dbUser, dbPass, dbnames._1),
      MySQL(dbUser, dbPass, dbnames._2),
      SQLite(dbnames._3)
    )

  }


  def applyDBs(dbs: List[BaseDatabase], apply: BaseDatabase => Unit) =
    dbs.foreach { db =>
      try {
        db.connect()
        apply(db)
      } finally {
        db.disconnect()
      }
    }

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
              val workDirDb = Utils.joinPaths(List(
                Utils.getWorkdir(), dbname))
              applyDBs(genDBList(None, workDirDb), (db) => db.createdb(dbname))
              applyDBs(genDBList(Some(dbname), workDirDb),
                      (db) => db.setupSchema(schemaPath))
            }
          }
        }
      }
      case _ => println("Wrong arguments")
    }
  }
}
