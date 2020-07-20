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
    "mssql+pyodbc://" + user + ":" + password + "@localhost:1433/" + dbname + "?driver=ODBC+Driver+17+for+SQL+Server"

  override def getName() =
    "mssql"
}

case class Oracle (user: String, password: String, hostname: String, sid: String) extends DB {
  val defaultDbs = Set(
    "SYSTEM",
    "SYSAUX",
    "UNDOTBS1",
    "TEMP",
    "USERS"
  )

  // We use this function in SQLAlchemy translator, 1433 is the ODBC port
  override def getURI() = ???
    // "mssql+pyodbc://" + user + ":" + password + "@localhost:1433/" + dbname + "?driver=ODBC+Driver+17+for+SQL+Server"

  override def getName() =
    "oracle"
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
    case MSSQL (user, password, dbname) =>
      "jdbc:sqlserver://localhost:1434;database=" + dbname +
          ";user=" + user + ";password=" + password + ";"
    case Oracle(user, password, hostname, sid) => {
      val url = "jdbc:oracle:thin:@" + hostname + ":1521:" + sid +
          "?user=" + user + "&password=" + password
      println(url)
      url
    }
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
      case MSSQL(_, _, _) => "select name from sys.databases"
      case Oracle(_, _, _, _) => "SELECT TABLESPACE_NAME FROM USER_TABLESPACES;"
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
      case Oracle(_, _, _, _) =>
        if (dbcon != null) {
          val stmt = dbcon.createStatement()
          // https://stackoverflow.com/questions/33330968/error-ora-65096-invalid-common-user-or-role-name-in-oracle
          val changeSessionSettings = "alter session set \"_ORACLE_SCRIPT\"=true"
          // stmt.executeUpdate("CREATE NAMESPACE IF NOT EXISTS " + dbname.toLowerCase + ";")
          // println("namespace " + dbname.toLowerCase + " created" + dbname.toLowerCase)
          val tbs_perm = "tbs_" + dbname.toLowerCase + "_tmp"
          val perm_file = "perm_" + dbname.toLowerCase + ".dat"
          val tbs_temp = "tbs_" + dbname.toLowerCase + "_perm"
          val tmp_file = "tmp_" + dbname.toLowerCase + ".dbf"
          val cmd = "CREATE USER " + dbname.toLowerCase +
            " IDENTIFIED BY Password123 " +
            " DEFAULT TABLESPACE " + tbs_perm + " " +
            " TEMPORARY TABLESPACE " + tbs_temp + " " +
            " QUOTA 20M on " + tbs_perm
          stmt.executeUpdate(changeSessionSettings)
          stmt.executeUpdate("alter session set container=ORCLPDB1")
          stmt.executeUpdate("create tablespace " + tbs_perm + " datafile '" + perm_file + "' size 20M autoextend on")
          stmt.executeUpdate("create temporary tablespace " + tbs_temp + " tempfile '" + tmp_file + "' size 20M autoextend on")
          stmt.executeUpdate(cmd)
          stmt.executeUpdate("GRANT CONNECT TO " + dbname.toLowerCase)
          stmt.executeUpdate("GRANT CONNECT, RESOURCE, DBA TO " + dbname.toLowerCase)
          stmt.executeUpdate("GRANT create session TO " + dbname.toLowerCase)
          stmt.executeUpdate("GRANT create table TO " + dbname.toLowerCase)
          stmt.executeUpdate("GRANT create view TO " + dbname.toLowerCase)
          stmt.executeUpdate("GRANT create any trigger TO " + dbname.toLowerCase)
          stmt.executeUpdate("GRANT create any procedure TO " + dbname.toLowerCase)
          stmt.executeUpdate("GRANT create sequence TO " + dbname.toLowerCase)
          stmt.executeUpdate("GRANT create synonym TO " + dbname.toLowerCase)
          println("user " + dbname.toLowerCase + " created")
        }
      case SQLite(_) => ()
    }
}
