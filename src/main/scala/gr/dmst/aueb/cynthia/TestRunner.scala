package gr.dmst.aueb.cynthia

import java.io.File
import java.nio.file.{Paths, Files}
import scala.concurrent.{Await, Future, TimeoutException}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.{Try, Success, Failure}
import scala.language.postfixOps
import scala.io.Source
import scala.sys.process._

import pprint.PPrinter.BlackWhite
import spray.json._
import DefaultJsonProtocol._

import gr.dmst.aueb.cynthia.gen.{SolverDataGenerator, NaiveDataGenerator}
import gr.dmst.aueb.cynthia.serializers.AQLJsonProtocol._
import gr.dmst.aueb.cynthia.translators.{QueryInterpreter, SchemaTranslator, State}


case class Target(orm: ORM, db: DB) {
  def getTargetCommand() = orm match {
    case Django (_, _, _) | SQLAlchemy (_, _)
    | Peewee(_, _) | Pony(_, _) => "python3 " + orm.getDriverPath(db)
    case Sequelize(_, _)    => "node " + orm.getDriverPath(db)
    case ActiveRecord(_, _) => "ruby " + orm.getDriverPath(db)
  }

  override def toString() =
    orm.ormName + "[" + db.getName() + "]"
}

object TestRunnerCreator {
  def genBackends(backends: Seq[String], workdir: String,
    dbname: Option[String], dbUser: String, dbPass: String) = {
    // Get default databases per backend
    val (pdb, mdb, cdb, msdb, sdb) = dbname match {
      case None         => ("postgres", "sys", "defaultdb", "master", workdir)
      case Some(dbname) => (dbname, dbname, dbname, dbname, workdir)
    }
    backends.map { x => x match {
        // In postgress, the database should be in lowercase.
        case "postgres"       => Postgres(dbUser, dbPass, pdb.toLowerCase)
        case "mysql"          => MySQL(dbUser, dbPass, mdb)
        case "mssql"          => MSSQL(dbUser, "orm_testing1", msdb)
        case "cockroachdb"    => Cockroachdb("root", "", cdb.toLowerCase)
        case "sqlite"         => SQLite(sdb)
        case _                => ???
      }
    }
  }

  def genORMs(orms: Seq[String], dbname: String, workdir: String) =
    orms.map { x => x match {
        case "django"        => Django(dbname,
                                   Utils.joinPaths(List(workdir, "django")),
                                   "djangoproject")
        case "sqlalchemy"    => SQLAlchemy(dbname,
                                        Utils.joinPaths(List(workdir, "sqlalchemy")))
        case "sequelize"     => Sequelize(dbname,
                                       Utils.joinPaths(List(workdir, "sequelize")))
        case "peewee"        => Peewee(dbname,
                                       Utils.joinPaths(List(workdir, "peewee")))
        case "pony"          => Pony(dbname,
                                       Utils.joinPaths(List(workdir, "pony")))
        case "activerecord"  => ActiveRecord(dbname,
                                       Utils.joinPaths(List(workdir, "activerecord")))
        case _               => ???
      }
    }

  def genTargets(orms: Seq[ORM], backends: Seq[DB]) =
    orms.flatMap(x => backends.map(y => Target(x, y)))

  def apply(options: Options, schema: Schema) = {
    val schemaPath = Utils.joinPaths(List(Utils.getSchemaDir(), schema.name))
    val dbDir = Utils.joinPaths(List(Utils.getDBDir(), schema.name))
    val createdb = Try(genBackends(
      options.dbs, dbDir, None,
      options.dbUser, options.dbPass
    ) foreach { db =>
      DBSetup.createdb(db, schema.name)
    })
    val dbs = genBackends(
      options.dbs, dbDir, Some(schema.name), options.dbUser, options.dbPass)
    val schemadb = createdb.flatMap { _ =>
      Try(dbs.foreach { db =>
        DBSetup.setupSchema(db, schemaPath)
      })
    }
    val projectDir = Utils.joinPaths(List(Utils.getProjectDir(), schema.name))
    val orms = genORMs(options.orms, schema.name, projectDir)
    schemadb.flatMap { _ =>
      Try(Utils.createDir(projectDir))
        .flatMap { _ => Try (
          orms.foreach { orm => ProjectCreator.createProject(orm, dbs) })
        }
    } match {
      case Success(_) => Success(new TestRunner(
        schema, genTargets(orms, dbs), options))
      case Failure(e) => Failure(e)
    }
  }
}

case class Stats(
  totalQ: Int = 0,
  mismatches: Int = 0,
  invalid: Int = 0,
  solverTimeouts: Int = 0
  ) {

  def ++(mism: Boolean = false, invd: Boolean = false, timedout: Boolean = false) =
    if (mism)
      Stats(totalQ + 1, mismatches + 1, invalid, solverTimeouts)
    else
      if (invd) Stats(totalQ + 1, mismatches, invalid + 1, solverTimeouts)
      else
        if (timedout) Stats(totalQ, mismatches, invalid, solverTimeouts + 1)
        else Stats(totalQ + 1, mismatches, invalid, solverTimeouts)
}

sealed trait DataGenRes
case object DataExists extends DataGenRes
case object DataGenFailed extends DataGenRes
case class  DataGenSucc(data: String) extends DataGenRes


class TestRunner(schema: Schema, targets: Seq[Target], options: Options) {

  private val genEnumerator = LazyList.from(1).iterator

  private val dbs = targets.map(x => x.db).toSet

  private val mismatchTxt = "MISMATCH"

  private val matchTxt = "MATCH"

  private val invalidTxt = "INVALID"

  val qGen = QueryGenerator(
    options.minDepth, options.maxDepth, options.noCombined, options.wellTyped)

  // test and generate modes
  def genQuery(schema: Schema, limit: Int = 10) = {
    def _genQuery(i: Int): LazyList[(Int, Query)] = {
      val q = qGen(schema)
      if (i >= limit) (i, q) #:: LazyList.empty
      else (i, q) #:: _genQuery(i + 1)
    }
    _genQuery(1)
  }

  // run mode
  def getQueriesFromDisk(path: String) = {
    if (Files.isDirectory(Paths.get(path)))
      // Revisit We want to return a LazyList
      (Utils.getListOfFiles(path).foldLeft((1, List[(Int, Query)]())) {
        case ((c, l), p) => (c + 1, (c, Utils.loadQuery(p)) :: l)
      })._2
    else
      LazyList((1, Utils.loadQuery(path)))
  }

  // replay mode
  def getQueriesFromCynthia() = {
    // Revisit We want to return a LazyList
    val dirs =
      if (options.all) Utils.getListOfDirs(getQueriesDir)
      // We cannot have options.all and options.mismatches
      else
        Utils.getListOfDirs(getQueriesDir) filter(p => {
          val diffOut = Utils.joinPaths(List(p, "diff_test.out"))
          val isMismatch =
            Utils.exists(diffOut) &&
              Source.fromFile(diffOut).mkString.equals(mismatchTxt)
          if (options.mismatches.isEmpty) isMismatch
          else options.mismatches.contains(p.split('/').last.toInt) && isMismatch
        })
    // Get queries from mismatches and probably matches
    val queries = dirs map (x => {
      val queryJsonFile = Utils.joinPaths(List(x, "query.aql.json"))
      val query = Utils.loadQuery(queryJsonFile)
      // Remove old report
      //Utils.deleteRecursively(new File(x))
      (x.split('/').last.toInt, query)
    })
    queries
  }

  def getQueries(): Seq[(Int, Query)] = {
    options.mode match {
      case Some("test")  =>
        genQuery(schema, limit = options.nuqueries)
      case Some("generate")  =>
        genQuery(schema, limit = options.nuqueries)
      case Some("run") =>
        options.aql match {
          case Some(x) => getQueriesFromDisk(x)
          case None    => ??? // unreachable
        }
      case Some("replay") => getQueriesFromCynthia
      case _ => ??? // unreachable
    }
  }

  def getQueriesDir() =
    Utils.joinPaths(List(
      Utils.getReportDir,
      schema.name)
    )

  def getQueryOutputDir(qid: Int) =
    Utils.joinPaths(List(
      Utils.getReportDir,
      schema.name,
      qid.toString)
    )

  def getInitialDataFile() =
    Utils.joinPaths(List(Utils.getSchemaDir, s"data_${schema.name}.sql"))

  def getSQLQueryData(qid: Int) =
    Utils.joinPaths(
      List(Utils.getReportDir, schema.name, qid.toString, "data.sql"))

  def storeResults(q: Query, results: Map[(String, String), Seq[Target]],
                   reportDir: String, diffOut: String) = {
    storeQueries(q, reportDir)
    Utils.writeToFile(Utils.joinPaths(List(reportDir, "diff_test.out")), diffOut)
    if (!diffOut.equals(invalidTxt))
      results.foreach { case ((_, k), v) => v.foreach { x =>
          // FIXME: For debugging purposes only.
          s"cp -r ${x.orm.projectDir} $reportDir".!!
          val filename = s"${x.orm.ormName}_${x.db.getName}.out"
          Utils.writeToFile(Utils.joinPaths(List(reportDir, filename)), k)
        }
      }
  }

  def storeQueries(q: Query, queriesDir: String) = {
    new File(queriesDir).mkdirs
    val queryFile = Utils.joinPaths(List(queriesDir, "query.aql"))
    val queryJsonFile = Utils.joinPaths(List(queriesDir, "query.aql.json"))
    Utils.writeToFile(queryFile, BlackWhite.tokenize(q).mkString)
    Utils.writeToFile(queryJsonFile, q.toJson.prettyPrint)
  }

  def generate() = {
    println(s"Generating queries for ${schema.name}...")
    getQueries().foreach { case (qid, q) =>
      val queriesDir = getQueryOutputDir(qid)
      storeQueries(q, queriesDir)
    }
  }

  def loadInitialData() =
    Source.fromFile(getInitialDataFile).mkString

  def generateSolverData(q: Query, s: State) =
      SolverDataGenerator(schema, q, s, options.records,
                          options.solverTimeout)()

  def generateNaiveData() = {
    val modelMap = schema.models.foldLeft(Map[String, Set[String]]()) {
      case (acc, (k, v)) => {
        val acc2 = if (acc.contains(k)) acc else acc + (k -> Set[String]())
        (v.fields filter Field.isForeign).foldLeft(acc2) { case (acc, Field(_, Foreign(n))) => {
          acc get k match {
            case None    => acc + (k -> Set(n))
            case Some(e) => acc + (k -> (e + n))
          }
        }}
    }}
    val topSort = Utils.topologicalSort(modelMap)
    (topSort map (m => {
      val model = schema.models(m)
      (model, NaiveDataGenerator(model, options.records, limit = options.records))
    })).toMap
  }

  def populateSchema(dataFile: String, getData: () => Option[Map[Model, Seq[Seq[Constant]]]]) =
    if (Utils.exists(dataFile)) {
      dbs.foreach { db => DBSetup.setupSchema(db, dataFile) }
      DataExists
    } else
      if (options.mode.get.equals("test")) {
        getData() match {
          case None       => DataGenFailed
          case Some(data) => {
            val insStms =
              data.foldLeft(Str("")) { case (acc, (model, data)) =>
                acc << SchemaTranslator.dataToInsertStmts(model, data)
              }.toString
            dbs foreach { db =>
              val dataPath = Utils.joinPaths(
                List(Utils.getProjectDir, schema.name, s"data_${db.getName}.sql"))
              Utils.writeToFile(dataPath, insStms)
              DBSetup.setupSchema(db, dataPath)
            }
            DataGenSucc(insStms)
          }
        }
      } else DataExists

  def storeDataSQL(reportDir: String, data: Option[String]) = data match {
    case None       => () // Empty insert statements.
    case Some(data) =>
      Utils.writeToFile(Utils.joinPaths(List(reportDir, "data.sql")), data)
  }

  def prepareFuture(stats: Stats, qid: Int, q: Query, s: State,
                    data: Option[String]): Future[Stats] = {
    val futures = targets map { t =>
      Future {
        (t, QueryExecutor(q, s, t))
      }
    }
    Future.sequence(futures) map { res =>
      val results = (
        Map[(String, String), Seq[Target]](),
        Map[(String, String), Seq[Target]]()
      )
      val (oks, failed) =
        res.foldLeft(results) { case ((oks, failed), x) => {
          x match {
            case (target, Unsupported(_)) => (oks, failed)
            case (_, Invalid(_)) => (oks, failed)
            case (target, Ok(res)) => {
              val k = (target.db.getName(), res)
              val targets = oks getOrElse(k, Seq())
              (oks + (k -> (targets :+ target)), failed)
            }
            case (target, Fail(err)) => {
              val k = (target.db.getName(), err)
              val targets = failed getOrElse(k, Seq())
              (oks, failed + (k -> (targets :+ target)))
            }
          }
        }}
      val okDbs = oks.keys.map { case (k, _) => k }.toSet
      val reportDir = getQueryOutputDir(qid)
      if ((failed.keys.exists { case (k, _) => okDbs.contains(k) }) ||
          oks.size > dbs.size) {
        val msg =
          s"""${Console.GREEN}[INFO]: Mismatch found in schema '${schema.name}':${Console.RESET}
          |  - Query ID: $qid
          |  - Report Directory: $reportDir
          |  - OK Target Groups:\n${oks.map { case (_, v) =>
              "    * " + (v.map {_.toString} mkString ", ")
            } mkString "\n"}
          |  - Failed Target Group: ${failed.values.flatten.map { _.toString } mkString ", " }
          """.stripMargin
        storeResults(q, oks ++ failed, reportDir, mismatchTxt)
        storeDataSQL(reportDir, data)
        println(msg)
        stats ++ (mism = true)
      } else if (failed.size == 0 && oks.size == 0) {
        storeResults(q, oks ++ failed, reportDir, invalidTxt)
        stats ++ (invd = true)
      } else {
        if (options.storeMatches) {
          storeDataSQL(reportDir, data)
          storeResults(q, oks ++ failed, reportDir, matchTxt)
        }
        stats.++()
      }
    }
  }

  def start() = {
    val initialInsertStmts = new StringBuilder
    val extraInsertStmts = new StringBuilder
    if (!options.solverGen) {
      populateSchema(
        getInitialDataFile,
        { () =>
          if (options.mode.get.equals("test")) Some(generateNaiveData())
          else None
        }) match {
        case DataGenSucc(stmts) => {
          initialInsertStmts.append(stmts)
          Utils.writeToFile(getInitialDataFile, stmts)
        }
        case _ => ()
      }
    }
    val stats = getQueries()
      .foldLeft(Stats()) { case (stats, (qid, q)) => {
        try {
          val s = QueryInterpreter(q)
          val (newStats, data) =
            if (!options.solverGen && options.mode.get.equals("test"))
              (stats, None)
            else
              populateSchema(
                getSQLQueryData(qid),
                { () =>
                    SolverDataGenerator(
                      schema, q, s, options.records, options.solverTimeout)()
                }) match {
                  case DataGenFailed      => (stats ++ (timedout = true), None)
                  case DataExists         => (stats, None)
                  case DataGenSucc(stmts) => (stats, Some(stmts))
                }
          Await.result(prepareFuture(newStats, qid, q, s, data), 10 seconds)
        } catch {
          case e: TimeoutException => {
            BlackWhite.tokenize(q).mkString
            stats
          }
          case e: Exception => {
            println(BlackWhite.tokenize(q).mkString)
            throw e
          }
        }
      }
    }
    val msg =
      s"""Testing session for ${schema.name} ends...
      |Statistics
      |----------
      |Total Queries: ${stats.totalQ}
      |Queries Passed: ${stats.totalQ - stats.mismatches - stats.invalid - stats.solverTimeouts}
      |Mismatches: ${stats.mismatches}
      |Invalid Queries: ${stats.invalid}
      |Solver Timeouts: ${stats.solverTimeouts}\n""".stripMargin
    println(msg)
  }
}
