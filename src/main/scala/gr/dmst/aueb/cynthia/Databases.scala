package gr.dmst.aueb.cynthia

import java.sql.{Connection, DriverManager, ResultSet}
import scala.io.Source


sealed trait DB {
  def getURI(): String
  def getName(): String
}

case class Postgres (user: String, password: String, dbname: String) extends DB {
  override def getURI() =
    "postgresql://" + user + ":" + password + "@localhost/" + dbname

  override def getName() =
    "postgres"
}

case class MySQL (user: String, password: String, dbname: String) extends DB {
  override def getURI() =
    "mysql://" + user + ":" + password + "@localhost/" + dbname

  override def getName() =
    "mysql"
}

case class SQLite (dbname: String) extends DB {
  override def getURI() =
    "sqlite:///" + dbname

  override def getName() =
    "sqlite"
}


object DBSetup {

  def getHost(db: DB) = db match {
    case Postgres(user, password, dbname) =>
      "jdbc:postgresql://localhost:5432/" + dbname +
          "?user=" + user + "&password=" + password
    case MySQL(user, password, dbname) =>
      "jdbc:mysql://localhost:3306/" + dbname +
          "?user=" + user + "&password=" + password
    case SQLite(dbname) => "jdbc:sqlite:" + dbname
  }

  def process(f: (DB, String, Connection) => Unit, db: DB, element: String) = {
    val host = getHost(db)
    val dbcon = DriverManager.getConnection(host)
    try {
      f(db, element, dbcon)
    } finally {
      dbcon.close()
    }
  }

  def setupSchema(db: DB, schema: String) =
    process(setupSchemaFun, db, schema)

  def createdb(db: DB, dbname: String) =
    process(createdbFun, db, dbname)

  def setupSchemaFun(db: DB, schema: String, dbcon: Connection) =
    if (dbcon != null) {
      val stmt = dbcon.createStatement()
      dbcon.setAutoCommit(false)
      Source.fromFile(schema)
        .mkString
        .replace("\n", "")
        .split(";")
        .foreach { stmt.addBatch }
      stmt.executeBatch()
      dbcon.commit()
    }

  def createdbFun(db: DB, dbname: String, dbcon: Connection) =
    db match {
      case Postgres(_, _, _) =>
        if (dbcon != null) {
          val preStmt = dbcon.prepareStatement(
            "SELECT 1 FROM pg_catalog.pg_database WHERE datname = ?")
          preStmt.setString(1, dbname)
          if (!preStmt.executeQuery().next()) {
            val stmt = dbcon.createStatement()
            stmt.executeUpdate("CREATE DATABASE " + dbname)
          }
        }
      case MySQL(_, _, _) =>
        if (dbcon != null) {
          val stmt = dbcon.createStatement()
          stmt.executeUpdate("CREATE DATABASE IF NOT EXISTS " + dbname)
        }
      case SQLite(_) => ()
    }
}
