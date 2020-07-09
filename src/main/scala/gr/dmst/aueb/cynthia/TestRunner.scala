package gr.dmst.aueb.cynthia

import java.io.File
import java.nio.file.{Paths, Files}
import scala.concurrent.{Await, Future, TimeoutException}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.{Try, Success, Failure}
import scala.language.postfixOps
import scala.sys.process._

import pprint.PPrinter.BlackWhite
import spray.json._
import DefaultJsonProtocol._

import gr.dmst.aueb.cynthia.serializers.AQLJsonProtocol._


case class Target(orm: ORM, db: DB) {
  def getTargetCommand() = orm match {
    case Django (_, _, _) | SQLAlchemy (_, _)
    | Peewee(_, _) => "python3 " + orm.getDriverPath(db)
    case Sequelize(_, _)    => "node " + orm.getDriverPath(db)
    case ActiveRecord(_, _) => "ruby " + orm.getDriverPath(db)
  }

  override def toString() =
    orm.ormName + "[" + db.getName() + "]"
}

object TestRunnerCreator {
  def genBackends(backends: Seq[String], workdir: String,
    dbname: Option[String], dbUser: String, dbPass: String) = {
    val (pdb, mdb, sdb) = dbname match {
      case None         => ("postgres", "sys", workdir)
      case Some(dbname) => (dbname, dbname, workdir)
    }
    backends.map { x => x match {
        // In postgress, the database should be in lowercase.
        case "postgres" => Postgres(dbUser, dbPass, pdb.toLowerCase)
        case "mysql"    => MySQL(dbUser, dbPass, mdb)
        case "sqlite"   => SQLite(sdb)
        case _          => ???
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

case class Stats(totalQ: Int = 0, mismatches: Int = 0, invalid: Int = 0) {
  def ++(mism: Boolean = false, invd: Boolean = false) =
    if (mism)
      Stats(totalQ + 1, mismatches + 1, invalid)
    else
      if (invd) Stats(totalQ + 1, mismatches, invalid + 1)
      else Stats(totalQ + 1, mismatches, invalid)
}

class TestRunner(schema: Schema, targets: Seq[Target], options: Options) {
  val mismatchEnumerator = LazyList.from(1).iterator
  val matchesEnumerator = LazyList.from(1).iterator
  val genEnumerator = LazyList.from(1).iterator

  val qGen = QueryGenerator(
    options.minDepth, options.maxDepth, options.noCombined)

  // test and generate modes
  def genQuery(schema: Schema, limit: Int = 10) = {
    def _genQuery(i: Int): LazyList[Query] = {
      val q = qGen(schema)
      if (i >= limit) q #:: LazyList.empty
      else q #:: _genQuery(i + 1)
    }
    _genQuery(1)
  }

  // run mode
  def getQueriesFromDisk(path: String) = {
    if (Files.isDirectory(Paths.get(path)))
      // Revisit We want to return a LazyList
      Utils.getListOfFiles(path).map(Utils.loadQuery(_))
    else
      LazyList(Utils.loadQuery(path))
  }

  // replay mode
  def getQueriesFromCynthia() = {
    // Revisit We want to return a LazyList
    val dirs =
      if (options.all)
        Utils.getListOfDirs(getMismatchDir) ++ Utils.getListOfDirs(getMatchDir()) ++ Utils.getListOfDirs(getQDir())
      // We cannot have options.all and options.mismatches
      else
        Utils.getListOfDirs(getMismatchDir) filter { x =>
          if (options.mismatches.isEmpty) true
          else options.mismatches.contains(x.split('/').last.toInt)
        }
    // Get invalid queries
    val invalidQueries: Seq[Query] =
      if (options.all) {
        val invalidQDir = getInvalidQDir
        Utils.listFiles(invalidQDir, ".aql") match {
          case Some(x) => x.toIndexedSeq.map(q => {
            val queryJsonFile = Utils.joinPaths(List(invalidQDir, q + ".json"))
            val queryFile = Utils.joinPaths(List(invalidQDir, q))
            val query = Utils.loadQuery(queryJsonFile)
            Utils.deleteRecursively(new File(queryFile))
            Utils.deleteRecursively(new File(queryJsonFile))
            query
          })
          case _ => Seq[Query]()
        }
      } else {
        Seq[Query]()
      }
    // Get queries from mismatches and probably matches
    val queries = dirs map (x => {
      val queryJsonFile = Utils.joinPaths(List(x, "query.aql.json"))
      val query = Utils.loadQuery(queryJsonFile)
      // Remove old report
      Utils.deleteRecursively(new File(x))
      query
    })
    queries ++ invalidQueries
  }

  def getQueries(): Seq[Query] = {
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
      case Some("replay") => {
        getQueriesFromCynthia()
      }
      case _ => ??? // unreachable
    }
  }

  def getMismatchesDir(i: Int) =
    Utils.joinPaths(List(
      Utils.getReportDir(),
      schema.name,
      "mismatches",
      i.toString)
    )

  def getMismatchDir() =
    Utils.joinPaths(List(
      Utils.getReportDir(),
      schema.name,
      "mismatches")
    )

  def getInvalidQDir() =
    Utils.joinPaths(List(
      Utils.getReportDir(),
      schema.name,
      "invalid")
    )

  def getMatchesDir(i: Int) =
    Utils.joinPaths(List(
      Utils.getReportDir(),
      schema.name,
      "matches",
      i.toString)
    )

  def getMatchDir() =
    Utils.joinPaths(List(
      Utils.getReportDir(),
      schema.name,
      "matches")
    )

  def getQDir() =
    Utils.joinPaths(List(
      Utils.getReportDir(),
      schema.name,
      "queries")
    )

  def getQueriesDir(i: Int) =
    Utils.joinPaths(List(
      Utils.getReportDir(),
      schema.name,
      "queries",
      i.toString)
    )

  def storeInvalid(q: Query, i: Int) = {
    val invDir = getInvalidQDir()
    new File(invDir).mkdirs
    val queryFile = Utils.joinPaths(List(invDir, s"query_$i.aql"))
    val queryJsonFile = Utils.joinPaths(List(invDir, s"query_$i.aql.json"))
    Utils.writeToFile(queryFile, BlackWhite.tokenize(q).mkString)
    Utils.writeToFile(queryJsonFile, q.toJson.prettyPrint)
  }

  def storeResults(q: Query, results: Map[(String, String), Seq[Target]],
                   reportDir: String) = {
    storeQueries(q, reportDir)
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
    getQueries().foreach { q =>
      val qid = genEnumerator.next()
      val queriesDir = getQueriesDir(qid)
      storeQueries(q, queriesDir)
    }
  }

  def start() = {
    println(s"Starting testing session for ${schema.name}...")
    val stats = getQueries()
      .foldLeft(Stats()) { (acc, q) => {
        try {
          val futures = targets map { t =>
            Future {
              (t, QueryExecutor(q, t))
            }
          }
          val f = Future.sequence(futures) map { res =>
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
              // Get the number of backends
              val ndbs = targets.map(x => x.db).toSet.size
              val okDbs = oks.keys.map { case (k, _) => k }.toSet
              if ((failed.keys.exists { case (k, _) => okDbs.contains(k) }) ||
                  oks.size > ndbs) {
                val qid = mismatchEnumerator.next()
                val reportDir = getMismatchesDir(qid)
                val msg =
                  s"""${Console.GREEN}[INFO]: Mismatch found in schema '${schema.name}':${Console.RESET}
                  |  - Query ID: $qid
                  |  - Report Directory: $reportDir
                  |  - OK Target Groups:\n${oks.map { case (_, v) =>
                      "    * " + (v.map {_.toString} mkString ", ")
                    } mkString "\n"}
                  |  - Failed Target Group: ${failed.values.flatten.map { _.toString } mkString ", " }
                  """.stripMargin
                storeResults(q, oks ++ failed, reportDir)
                println(msg)
                acc ++ (mism = true)
              } else if (failed.size == 0 && oks.size == 0) {
                val qid = mismatchEnumerator.next()
                storeInvalid(q, qid)
                acc ++ (invd = true)
              } else {
                if (options.storeMatches) {
                  val qid = matchesEnumerator.next()
                  val reportDir = getMatchesDir(qid)
                  storeResults(q, oks ++ failed, reportDir)
                }
                acc.++()
              }
          }
          Await.result(f, 10 seconds)
        } catch {
          case e: TimeoutException => {
            BlackWhite.tokenize(q).mkString
            acc
          }
        }
      }
    }
    val msg =
      s"""Testing session for ${schema.name} ends...
      |Statistics
      |----------
      |Total Queries: ${stats.totalQ}
      |Queries Passed: ${stats.totalQ - stats.mismatches - stats.invalid}
      |Mismatches: ${stats.mismatches}
      |Invalid Queries: ${stats.invalid}\n""".stripMargin
    println(msg)
  }
}
