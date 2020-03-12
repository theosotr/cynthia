package gr.dmst.aueb.cynthia

import java.sql.{Connection, DriverManager, ResultSet}
import scala.io.Source


sealed abstract class BaseDatabase {

  var dbcon: Connection = null

  def getHost(): String

  def connect() =
    this.dbcon = DriverManager.getConnection(getHost())

  def disconnect() =
    if (this.dbcon != null) {
      this.dbcon.close()
    } else {
      ()
    }

  def createdb(dbname: String): Unit

  def setupSchema(schema: String) =
    if (this.dbcon == null) {
      ()
    } else {
      val stmt = this.dbcon.createStatement()
      this.dbcon.setAutoCommit(false)
      Source.fromFile(schema)
        .mkString
        .replace("\n", "")
        .split(";")
        .foreach { stmt.addBatch }
      stmt.executeBatch()
      this.dbcon.commit()
    }
}


case class Postgres (user: String, password: String, dbname: String)
    extends BaseDatabase {

  override def getHost() =
    "jdbc:postgresql://localhost:5432/" + dbname +
        "?user=" + user + "&password=" + password

  override def createdb(dbname: String): Unit = {
    if (this.dbcon != null) {
      val preStmt = this.dbcon.prepareStatement(
        "SELECT 1 FROM pg_catalog.pg_database WHERE datname = ?")
      preStmt.setString(1, dbname)
      if (!preStmt.executeQuery().next()) {
        val stmt = this.dbcon.createStatement()
        stmt.executeUpdate("CREATE DATABASE " + dbname)
      }
    }
  }
}


case class MySQL (user: String, password: String, dbname: String)
    extends BaseDatabase {

  override def getHost() =
    "jdbc:mysql://localhost:3306/" + dbname +
        "?user=" + user + "&password=" + password

  override def createdb(dbname: String): Unit = {
    if (this.dbcon != null) {
      val stmt = this.dbcon.createStatement()
      stmt.executeUpdate("CREATE DATABASE IF NOT EXISTS " + dbname)
    }
  }
}


case class SQLite (dbname: String)
    extends BaseDatabase {

  override def getHost() =
    "jdbc:sqlite:" + dbname + ".sqlite3"

  override def createdb(dbname: String): Unit = { }
}
