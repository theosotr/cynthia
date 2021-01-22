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

import java.io.File
import java.nio.file.{Paths, Files}
import scala.concurrent.{Await, Future, TimeoutException}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.{Try, Success, Failure}
import scala.io.Source
import scala.sys.process._

import com.typesafe.scalalogging.Logger
import pprint.PPrinter.BlackWhite
import spray.json._
import me.tongfei.progressbar.ProgressBar;

import cynthia.Options
import cynthia.lang.{Schema, Query, QueryInterpreter, State}
import cynthia.lang.AQLJsonProtocol._
import cynthia.gen.{
  SolverDataGenerator,
  NaiveDataGenerator,
  DataGeneratorController,
  DataGenSucc,
  DataExists,
  DataGenFailed,
  QueryGenerator
}
import cynthia.utils.{Utils, Str}

case class Target(orm: ORM, db: DB) {
  def getTargetCommand() = orm match {
    case Django(_, _, _) | SQLAlchemy(_, _) | Peewee(_, _) | Pony(_, _) =>
      "python3 " + orm.getDriverPath(db)
    case Sequelize(_, _)    => "node " + orm.getDriverPath(db)
    case ActiveRecord(_, _) => "ruby " + orm.getDriverPath(db)
  }

  override def toString() =
    orm.ormName + "[" + db.getName() + "]"
}

case class TestRunnerCreator(logger: Logger) {
  def genBackends(
      backends: Seq[String],
      workdir: String,
      dbname: Option[String],
      dbUser: String,
      dbPass: String
  ) = {
    // All databases created by cynthia have the prefix 'cynthia_'.
    val p = "cynthia_"
    // Get default databases per backend
    val (pdb, mdb, cdb, msdb, sdb) = dbname match {
      case None => ("postgres", "sys", "defaultdb", "master", workdir)
      case Some(dbname) =>
        (p + dbname, p + dbname, p + dbname, p + dbname, workdir)
    }
    backends.map { x =>
      x match {
        // In postgress, the database should be in lowercase.
        case "postgres"    => Postgres(dbUser, dbPass, pdb.toLowerCase)
        case "mysql"       => MySQL(dbUser, dbPass, mdb)
        case "mssql"       => MSSQL(dbUser, dbPass, msdb)
        case "cockroachdb" => Cockroachdb("root", "", cdb.toLowerCase)
        case "sqlite"      => SQLite(sdb)
        case _             => ???
      }
    }
  }

  def genORMs(orms: Seq[String], dbname: String, workdir: String) =
    orms.map { x =>
      x match {
        case "django" =>
          Django(
            dbname,
            Utils.joinPaths(List(workdir, "django")),
            "djangoproject"
          )
        case "sqlalchemy" =>
          SQLAlchemy(dbname, Utils.joinPaths(List(workdir, "sqlalchemy")))
        case "sequelize" =>
          Sequelize(dbname, Utils.joinPaths(List(workdir, "sequelize")))
        case "peewee" =>
          Peewee(dbname, Utils.joinPaths(List(workdir, "peewee")))
        case "pony" => Pony(dbname, Utils.joinPaths(List(workdir, "pony")))
        case "activerecord" =>
          ActiveRecord(dbname, Utils.joinPaths(List(workdir, "activerecord")))
        case _ => ???
      }
    }

  def genTargets(orms: Seq[ORM], backends: Seq[DB]) =
    orms.flatMap(x => backends.map(y => Target(x, y)))

  def setupTargets(options: Options, schema: Schema) = {
    val schemaPath = Utils.joinPaths(List(Utils.getSchemaDir(), schema.name))
    val dbDir = Utils.joinPaths(List(Utils.getDBDir(), schema.name))
    val createdb = Try(
      genBackends(
        options.dbs,
        dbDir,
        None,
        options.dbUser,
        options.dbPass
      ) foreach { db =>
        DBSetup.createdb(db, schema.name)
      }
    )

  }

  def setupDBs(
      options: Options,
      filteredDBs: Option[Seq[String]],
      conDB: Option[String],
      schema: Schema,
      f: (DB) => Unit
  ) = {
    val dbDir = Utils.joinPaths(List(Utils.getDBDir(), schema.name))
    genBackends(
      options.dbs,
      dbDir,
      conDB,
      options.dbUser,
      options.dbPass
    ) filter (db =>
      filteredDBs match {
        case None              => true
        case Some(filteredDBs) => filteredDBs.contains(db.getName())
      }
    ) filter { db =>
      Try(f(db)) match {
        case Success(_) => true
        case Failure(e) => {
          logger.error(s"Unable to setup ${db.getName()}: ${e.getMessage}")
          false
        }
      }
    }
  }

  def apply(options: Options, schema: Schema, pBar: ProgressBar) = {
    // Setup databases: First we need to create a database whose name is the
    // same with the name of the provided schema. To do so, we log in in the
    // system database of each backend.
    //
    // After that, we log in the newly created database and create the tables
    // included in the schema.
    val schemaPath = Utils.joinPaths(List(Utils.getSchemaDir(), schema.name))
    val succDBs = setupDBs(
      options,
      Some(
        // At this point we create the databases.
        setupDBs(
          options,
          None,
          None,
          schema,
          { db => DBSetup.createdb(db, schema.name) }
        ) map { _.getName() }
      ),
      Some(schema.name),
      schema,
      { db => DBSetup.setupSchema(db, schemaPath) }
    )

    // If we were unable to setup any dabase, we should abort.
    val res =
      if (succDBs.isEmpty)
        Failure(
          new Exception(
            "Unable to setup any database."
              + " See the .cynthia/cynthia.log for more details."
          )
        )
      else Success(())

    val sessionDir = Utils.joinPaths(List(Utils.getProjectDir(), schema.name))
    val orms = genORMs(options.orms, schema.name, sessionDir)
    res.flatMap { _ =>
      orms.foldLeft(Try(Utils.createDir(sessionDir))) { case (acc, orm) =>
        acc.flatMap(_ =>
          // At this point we setup ORMs. We automatically create the ORM
          // models based on the provided schema, create all direcotires and
          // other ORM-specific files.
          Try(ProjectCreator.createProject(orm, succDBs)) match {
            case Failure(e) => {
              val msg = s"Unable to setup ORM ${orm.ormName}: ${e.getMessage}"
              logger.error(msg)
              Failure(new Exception(msg))
            }
            case Success(_) => Success(())
          }
        )
      }
    } match {
      case Success(_) =>
        Success(
          new TestRunner(
            schema,
            genTargets(orms, succDBs),
            options,
            logger,
            pBar
          )
        )
      case Failure(e) => Failure(e)
    }
  }
}

case class Stats(
    pBar: ProgressBar,
    totalQ: Int = 0,
    mismatches: Int = 0,
    invalid: Int = 0,
    solverTimeouts: Int = 0
) {

  override def toString() =
    (Str("Passed \u2714: ") << passed.toString() << ", " <<
      "Failed \u2718: " << mismatches.toString() << ", " <<
      "Unsp: " << invalid.toString() << ", " <<
      "Timeouts: " << solverTimeouts.toString()).!

  def passed =
    totalQ - mismatches - invalid - solverTimeouts

  def ++(
      mism: Boolean = false,
      invd: Boolean = false,
      timedout: Boolean = false
  ) = {
    pBar.step()
    val newStats =
      if (mism)
        Stats(pBar, totalQ + 1, mismatches + 1, invalid, solverTimeouts)
      else if (invd)
        Stats(pBar, totalQ + 1, mismatches, invalid + 1, solverTimeouts)
      else if (timedout)
        Stats(pBar, totalQ + 1, mismatches, invalid, solverTimeouts + 1)
      else
        Stats(pBar, totalQ + 1, mismatches, invalid, solverTimeouts)
    pBar.setExtraMessage(" " + newStats.toString())
    newStats
  }
}

class TestRunner(
    schema: Schema,
    targets: Seq[Target],
    options: Options,
    logger: Logger,
    pBar: ProgressBar
) {

  private val genEnumerator = LazyList.from(1).iterator

  private val dbs = targets.map(x => x.db).toSet

  private val mismatchTxt = "MISMATCH"

  private val matchTxt = "MATCH"

  private val unsupportedTxt = "UNSUPPORTED"

  val qGen = QueryGenerator(
    options.minDepth,
    options.maxDepth,
    options.noCombined,
    options.wellTyped,
    options.onlyConstrainedQ
  )

  // test and generate modes
  def genQuery(schema: Schema, limit: Int = 10) = {
    // At this point, we generate queries lazily.
    def _genQuery(i: Int, acc: LazyList[(Int, Query)]): LazyList[(Int, Query)] =
      if (i > limit) acc
      else _genQuery(i + 1, (i, qGen(schema)) #:: acc)
    _genQuery(1, LazyList.empty)
  }

  // Load queries from a list of *.aql.json files lazily.
  def loadQueriesFromFiles(qids: Seq[Int], files: List[String]) = {
    def _loadQuery(
        qids: Seq[Int],
        files: List[String],
        acc: LazyList[(Int, Query)]
    ): LazyList[(Int, Query)] = {
      files match {
        case Nil => acc
        case h :: t => {
          _loadQuery(qids.tail, t, (qids.head, Utils.loadQuery(h)) #:: acc)
        }
      }
    }
    _loadQuery(qids, files, LazyList.empty)
  }

  // Load queries from a file or a given directory (run mode)
  def getQueriesFromDisk(path: String) = {
    if (Files.isDirectory(Paths.get(path))) {
      val files = Utils.getListOfFiles(path)
      pBar.maxHint(files.size)
      loadQueriesFromFiles(LazyList.from(1), files)
    } else {
      pBar.maxHint(1)
      LazyList((1, Utils.loadQuery(path)))
    }
  }

  // Load queries from the cynthia working directory (replay mode)
  def getQueriesFromCynthia() = {
    val dirs =
      if (options.all) Utils.getListOfDirs(getQueriesDir())
      // We cannot have options.all and options.mismatches
      else
        Utils.getListOfDirs(getQueriesDir()) filter (p => {
          val diffOut = Utils.joinPaths(List(p, "diff_test.out"))
          val isMismatch =
            Utils.exists(diffOut) &&
              Source.fromFile(diffOut).mkString.equals(mismatchTxt)
          if (options.mismatches.isEmpty) isMismatch
          else
            options.mismatches.contains(p.split('/').last.toInt) && isMismatch
        })
    pBar.maxHint(dirs.size)
    // Get files where queries from mismatches and probably matches are stored.
    val (qids, files) = dirs
      .map(x =>
        (x.split('/').last.toInt, Utils.joinPaths(List(x, "query.aql.json")))
      )
      .unzip
    loadQueriesFromFiles(qids, files)
  }

  def getQueries(): LazyList[(Int, Query)] = {
    options.mode match {
      case Some("test") =>
        genQuery(schema, limit = options.nuqueries)
      case Some("generate") =>
        genQuery(schema, limit = options.nuqueries)
      case Some("run") =>
        options.aql match {
          case Some(x) => getQueriesFromDisk(x)
          case None    => ??? // unreachable
        }
      case Some("replay") => getQueriesFromCynthia()
      case _              => ??? // unreachable
    }
  }

  def getQueriesDir() =
    Utils.joinPaths(List(Utils.getSessionDir(), schema.name))

  def getQueryOutputDir(qid: Int) =
    Utils.joinPaths(List(Utils.getSessionDir(), schema.name, qid.toString))

  def getInitialDataFile() =
    Utils.joinPaths(List(Utils.getSchemaDir(), s"data_${schema.name}.sql"))

  def getSQLQueryData(qid: Int) =
    Utils.joinPaths(
      List(Utils.getSessionDir(), schema.name, qid.toString, "data.sql")
    )

  def storeResults(
      q: Query,
      results: Map[(String, String), Seq[Target]],
      sessionDir: String,
      diffOut: String
  ) = {
    storeQueries(q, sessionDir)
    Utils.writeToFile(
      Utils.joinPaths(List(sessionDir, "diff_test.out")),
      diffOut
    )
    if (!diffOut.equals(unsupportedTxt))
      results.foreach { case ((_, k), v) =>
        v.foreach { x =>
          // FIXME: For debugging purposes only.
          s"cp -r ${x.orm.projectDir} $sessionDir".!!
          val filename = s"${x.orm.ormName}_${x.db.getName()}.out"
          Utils.writeToFile(Utils.joinPaths(List(sessionDir, filename)), k)
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

  def generateInitialData() =
    if (!options.solverGen) {
      DataGeneratorController(
        schema,
        getInitialDataFile(),
        Some(NaiveDataGenerator(schema, options.records)),
        options.regenerateData
      ) populateDBs dbs match {
        case DataGenSucc(thunk) => thunk()
        case _                  => ()
      }
    }

  def generate() = {
    logger.info("Generating testing session")
    generateInitialData()
    getQueries().foreach { case (qid, q) =>
      pBar.step()
      val queriesDir = getQueryOutputDir(qid)
      storeQueries(q, queriesDir)
      if (!options.solverGen && options.generateData) ()
      else
        DataGeneratorController(
          schema,
          getSQLQueryData(qid),
          Some(
            SolverDataGenerator(
              schema,
              options.records,
              q,
              QueryInterpreter(q),
              options.solverTimeout
            )
          ),
          options.regenerateData
        ) populateDBs dbs match {
          case DataGenSucc(thunk) => thunk()
          case DataGenFailed =>
            logger.info("Z3 timed out for query " + qid)
          case _ => ()
        }
    }
    pBar.close()
    logger.info("Generation ended")
  }

  def loadInitialData() =
    Source.fromFile(getInitialDataFile()).mkString

  def evalThunk(dataThunk: Option[() => Unit]) = dataThunk match {
    case None        => () // Empty insert statements.
    case Some(thunk) => thunk()
  }

  def prepareFuture(
      stats: Stats,
      qid: Int,
      q: Query,
      s: State,
      dataThunk: Option[() => Unit]
  ): Future[Stats] = {
    val futures =
      targets map (t => (t, s.copy(numGen = LazyList.from(1).iterator))) map {
        case (t, s) =>
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
        res.foldLeft(results) {
          case ((oks, failed), x) => {
            x match {
              case (target, Unsupported(_)) => (oks, failed)
              case (_, Invalid(_))          => (oks, failed)
              case (target, Ok(res)) => {
                val k = (target.db.getName(), res)
                val targets = oks getOrElse (k, Seq())
                (oks + (k -> (targets :+ target)), failed)
              }
              case (target, Fail(err)) => {
                val k = (target.db.getName(), err)
                val targets = failed getOrElse (k, Seq())
                (oks, failed + (k -> (targets :+ target)))
              }
            }
          }
        }
      val okDbs = oks.keys.map { case (k, _) => k }.toSet
      val sessionDir = getQueryOutputDir(qid)
      if (
        (failed.keys.exists { case (k, _) => okDbs.contains(k) }) ||
        oks.size > dbs.size
      ) {
        storeResults(q, oks ++ failed, sessionDir, mismatchTxt)
        evalThunk(dataThunk)
        logger.info(s"Mismatch found in query ${qid}")
        stats ++ (mism = true)
      } else if (failed.size == 0 && oks.size == 0) {
        storeResults(q, oks ++ failed, sessionDir, unsupportedTxt)
        logger.info(s"Query ${qid} is unsupported")
        stats ++ (invd = true)
      } else {
        if (options.storeMatches || options.mode.get.equals("replay")) {
          evalThunk(dataThunk)
          storeResults(q, oks ++ failed, sessionDir, matchTxt)
        }
        stats.++()
      }
    }
  }

  def start(): Unit = {
    generateInitialData()
    logger.info("Starting testing session")
    getQueries()
      .foldLeft(Stats(pBar)) {
        case (stats, (qid, q)) => {
          try {
            val s = QueryInterpreter(q)
            val (newStats, thunk) =
              if (!options.solverGen && options.generateData) (stats, None)
              else
                DataGeneratorController(
                  schema,
                  getSQLQueryData(qid),
                  if (options.generateData)
                    Some(
                      SolverDataGenerator(
                        schema,
                        options.records,
                        q,
                        s,
                        options.solverTimeout
                      )
                    )
                  else None,
                  options.regenerateData
                ) populateDBs dbs match {
                  case DataGenFailed => {
                    logger.info("Z3 timed out for query " + qid)
                    (stats ++ (timedout = true), None)
                  }
                  case DataExists         => (stats, None)
                  case DataGenSucc(thunk) => (stats, Some(thunk))
                }
            Await.result(prepareFuture(newStats, qid, q, s, thunk), 10 seconds)
          } catch {
            case e: TimeoutException => {
              stats
            }
            case e: Exception => {
              logger.error(
                s"Unexpected error in Query ${qid.toString}: ${e.getMessage}"
              )
              stats ++ (invd = true)
            }
          }
        }
      }
    logger.info("Testing session ended")
    pBar.close()
  }
}
