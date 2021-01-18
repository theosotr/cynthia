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

package cynthia.targets

import java.sql.{Connection, DriverManager}
import scala.io.Source

import cynthia.utils.Utils


sealed trait DB {
  val defaultDbs: Set[String]
  def getURI(): String
  def getName(): String
}

case class Postgres (user: String, password: String, dbname: String) extends DB {
  val defaultDbs = Set(
    "postgres",
    "template0",
    "template1"
  )

  // We use this function in SQLAlchemy translator
  override def getURI() =
    "postgresql://" + user + ":" + password + "@localhost/" + dbname

  override def getName() =
    "postgres"
}

case class MySQL (user: String, password: String, dbname: String) extends DB {
  val defaultDbs = Set(
    "mysql",
    "information_schema",
    "performance_schema",
    "sys"
  )

  // We use this function in SQLAlchemy translator
  override def getURI() =
    "mysql://" + user + ":" + password + "@localhost/" + dbname

  override def getName() =
    "mysql"
}

case class SQLite (dbname: String) extends DB {
  val defaultDbs = Set()

  // We use this function in SQLAlchemy translator
  override def getURI() =
    "sqlite:///" + dbname

  override def getName() =
    "sqlite"
}

case class Cockroachdb (user: String, password: String, dbname: String) extends DB {
  val defaultDbs = Set(
    "defaultdb",
    "system",
    "postgres"
  )

  // We use this function in SQLAlchemy translator
  override def getURI() =
    "cockroachdb://" + user + "@localhost:26257/" + dbname

  override def getName() =
    "cockroachdb"
}

case class MSSQL (user: String, password: String, dbname: String) extends DB {
  val defaultDbs = Set(
    "master",
    "tempdb",
    "model",
    "msdb"
  )

  // We use this function in SQLAlchemy translator, 1433 is the ODBC port
  override def getURI() =
    "mssql+pyodbc://" + user + ":" + password + "@localhost:1433/" +
      dbname + "?driver=ODBC+Driver+17+for+SQL+Server"

  override def getName() =
    "mssql"
}

object DBSetup {

  private val tableNameRegex = "INSERT INTO \\[([a-zA-Z]+)\\]\\(".r

  def getHost(db: DB) = db match {
    case Postgres(user, password, dbname) =>
      "jdbc:postgresql://localhost:5432/" + dbname +
          "?user=" + user + "&password=" + password
    case MySQL(user, password, dbname) =>
      "jdbc:mysql://localhost:3306/" + dbname +
          "?user=" + user + "&password=" + password
    case Cockroachdb(user, _, dbname) =>
      "jdbc:postgresql://localhost:26257/" + dbname +
          "?user=" + user
    case MSSQL (user, password, dbname) =>
      "jdbc:sqlserver://localhost:1434;database=" + dbname +
          ";user=" + user + ";password=" + password + ";"
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
    // All the databases created by cynthia have the prefix 'cynthia_'.
    process(createdbFun, db, "cynthia_" + dbname)

  def clean(db: DB) =
    process(dropDatabases, db, "")

  def getDatabases(db: DB, dbcon: Connection) = {
    val query = db match {
      case Postgres(_, _, _) =>
        """
        SELECT datname FROM pg_database
        WHERE datistemplate = false;
        """
      case MySQL(_, _, _) | Cockroachdb(_, _, _) => "show databases;"
      case MSSQL(_, _, _) => "select name from sys.databases"
      case SQLite(_) => ??? // Unreachable case
    }
    val stmt = dbcon.createStatement
    val rs = stmt.executeQuery(query)
    Iterator.from(0).takeWhile(_ => rs.next()).map(_ => rs.getString(1)).toSet
  }

  def dropDatabases(db: DB, element: String, dbcon: Connection) = db match {
    case SQLite(_) =>
      Utils.listFiles(Utils.getDBDir(), ext = "") match {
        case Some(files) => files foreach Utils.emptyFile
        case None => ()
      }
    case _ => {
      val stmt = dbcon.createStatement
      dbcon.setAutoCommit(true)
      getDatabases(db, dbcon) filter (x =>
          !db.defaultDbs.contains(x) && x.startsWith("cynthia_")) foreach { x =>
        stmt.addBatch("DROP DATABASE IF EXISTS " + x)
      }
      stmt.executeBatch
    }
  }

  def setupSchemaFun(db: DB, schema: String, dbcon: Connection) =
    if (dbcon != null) {
      val stmt = dbcon.createStatement()
      val pattern = "\"([^\"\n]+)\""
      dbcon.setAutoCommit(false)
      val sql = db match {
        case MySQL(_, _, _) =>
          // Add this statement to disable foreign key constraints.
          "SET FOREIGN_KEY_CHECKS=0;\n" +
            Source.fromFile(schema).mkString.replaceAll(pattern, "`$1`")
        case MSSQL(_, _, _) => {
          // First we need to extract the tables related to the INSERT statements
          // Then, we construct ALTER TABLE XX NOCHECK CONSTRAINT ALL;
          // These statements are used to disable foreign key constraints.
          // We apply these statements before data insertion.
          val source = Source.fromFile(schema).mkString.replaceAll(
            pattern, "[$1]")
          val stmts = (tableNameRegex.findAllIn(
              source).matchData map { _.group(1) }).toSet map { x: String =>
            "ALTER TABLE [" + x + "] NOCHECK CONSTRAINT ALL;"
          }
          (stmts mkString "\n") + "\n" + source
        }
        case Postgres(_, _, _) =>
          // Add this statement at the end of file in order
          // to disable all foreign key constraints.
          "SET session_replication_role = 'replica';\n" +
            Source.fromFile(schema).mkString
        case _ => Source.fromFile(schema).mkString
      }
      sql.replace("\n", "")
        .split(";")
        .foreach { stmt.addBatch }
      stmt.executeBatch
      dbcon.commit
    }

  def createdbFun(db: DB, dbname: String, dbcon: Connection) =
    db match {
      case Postgres(_, _, _) =>
        if (dbcon != null) {
          val preStmt = dbcon.prepareStatement(
            "SELECT datname FROM pg_catalog.pg_database WHERE datname = ?")
          preStmt.setString(1, dbname.toLowerCase)
          if (!preStmt.executeQuery().next()) {
            val stmt = dbcon.createStatement()
            stmt.executeUpdate("CREATE DATABASE " + dbname.toLowerCase)
          }
        }
      case MSSQL(_, _, _) =>
        if (dbcon != null) {
          val preStmt = dbcon.prepareStatement(
            "SELECT name FROM sys.databases WHERE name = ?")
          preStmt.setString(1, dbname.toLowerCase)
          if (!preStmt.executeQuery().next()) {
            val stmt = dbcon.createStatement()
            stmt.executeUpdate("CREATE DATABASE " + dbname.toLowerCase)
          }
        }
      case MySQL(_, _, _) | Cockroachdb(_, _, _) =>
        if (dbcon != null) {
          val stmt = dbcon.createStatement()
          stmt.executeUpdate("CREATE DATABASE IF NOT EXISTS " + dbname)
        }
      case SQLite(_) => ()
    }
}
