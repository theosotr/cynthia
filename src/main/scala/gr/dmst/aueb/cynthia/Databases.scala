package gr.dmst.aueb.cynthia

import java.io.File
import java.sql.{Connection, DriverManager, ResultSet}
import scala.io.Source


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

  override def getURI() =
    "mysql://" + user + ":" + password + "@localhost/" + dbname

  override def getName() =
    "mysql"
}

case class SQLite (dbname: String) extends DB {
  val defaultDbs = Set()

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


object DBSetup {

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
      case SQLite(_) => ??? // Unreachable case
    }
    val stmt = dbcon.createStatement
    val rs = stmt.executeQuery(query)
    Iterator.from(0).takeWhile(_ => rs.next()).map(_ => rs.getString(1)).toSet
  }

  def dropDatabases(db: DB, element: String, dbcon: Connection) = db match {
    case SQLite(_) =>
      Utils.listFiles(Utils.getDBDir, ext = "") match {
        case Some(files) => files foreach Utils.emptyFile
        case None => ()
      }
    case _ => {
      val stmt = dbcon.createStatement
      dbcon.setAutoCommit(true)
      getDatabases(db, dbcon) filter { !db.defaultDbs.contains(_) } foreach { x =>
        stmt.addBatch("DROP DATABASE IF EXISTS " + x)
      }
      stmt.executeBatch
    }
  }

  def setupSchemaFun(db: DB, schema: String, dbcon: Connection) =
    if (dbcon != null) {
      val stmt = dbcon.createStatement()
      dbcon.setAutoCommit(false)
      Source.fromFile(schema)
        .mkString
        .replace("\n", "")
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
      case MySQL(_, _, _) | Cockroachdb(_, _, _) =>
        if (dbcon != null) {
          val stmt = dbcon.createStatement()
          stmt.executeUpdate("CREATE DATABASE IF NOT EXISTS " + dbname)
        }
      case SQLite(_) => ()
    }
}
