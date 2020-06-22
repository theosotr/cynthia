package gr.dmst.aueb.cynthia

import java.io.File
import scala.concurrent.{Await, Future, TimeoutException}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.{Try, Success, Failure}
import scala.language.postfixOps
import scala.sys.process._

import pprint.PPrinter.BlackWhite

import gr.dmst.aueb.cynthia.gen.SchemaGenerator
import gr.dmst.aueb.cynthia.translators.SchemaTranslator


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
    dbname: Option[String]) = {
    val dbUser = "orm_testing"
    val dbPass = "orm_testing"
    val (pdb, mdb, sdb) = dbname match {
      case None         => ("postgres", "sys", workdir)
      case Some(dbname) => (dbname, dbname, workdir)
    }
    backends.map { x => x match {
        case "postgres" => Postgres(dbUser, dbPass, pdb)
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
    val createdb = Try(genBackends(options.dbs, dbDir, None).foreach { db =>
      DBSetup.createdb(db, schema.name)
    })
    val dbs = genBackends(options.dbs, dbDir, Some(schema.name))
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
  val mismatchEnumerator = Stream.from(1).iterator

  def genQuery(schema: Schema, limit: Int = 10) = {
    def _genQuery(i: Int): LazyList[Query] = {
      val q = QueryGenerator(schema)
      if (i >= limit) q #:: LazyList.empty
      else q #:: _genQuery(i + 1)
    }
    _genQuery(1)
  }

  def getQueries(): Seq[Query] =
    options.mode match {
      case Some("auto")  =>
        genQuery(schema, limit = options.nuqueries)
      // TODO get query/queries from options.aql
      case Some("select") => ???
      // TODO get queries from options.dotCynthia and use options.mismatches
      case Some("replay") => ???
      case _ => ??? // unreachable
    }

  def getMismatchesDir(i: Int) =
    Utils.joinPaths(List(
      Utils.getReportDir(),
      schema.name,
      "mismatches",
      i.toString)
    )

  def getInvalidQDir() =
    Utils.joinPaths(List(
      Utils.getReportDir(),
      schema.name,
      "invalid")
    )

  def storeInvalid(q: Query, i: Int) = {
    val invDir = getInvalidQDir()
    new File(invDir).mkdirs
    val queryFile = Utils.joinPaths(List(invDir, s"query_$i.aql"))
    Utils.writeToFile(queryFile, BlackWhite.tokenize(q).mkString)
  }

  def storeMismatch(q: Query, mismatches: Map[(String, String), Seq[Target]],
                    reportDir: String) = {
    new File(reportDir).mkdirs
    val queryFile = Utils.joinPaths(List(reportDir, "query.aql"))
    Utils.writeToFile(queryFile, BlackWhite.tokenize(q).mkString)
    mismatches.foreach { case ((_, k), v) => v.foreach { x =>
        // FIXME: For debugging purposes only.
        s"cp -r ${x.orm.projectDir} $reportDir".!!
        val filename = s"${x.orm.ormName}_${x.db.getName}.out"
        Utils.writeToFile(Utils.joinPaths(List(reportDir, filename)), k)
      }
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
                  s"""${Console.GREEN}[INFO]: Mismatch found in schema '${schema.name}':${Console.WHITE}
                  |  - Query ID: $qid
                  |  - Report Directory: $reportDir
                  |  - OK Target Groups:\n${oks.map { case (_, v) =>
                      "    * " + (v.map {_.toString} mkString ", ")
                    } mkString "\n"}
                  |  - Failed Target Group: ${failed.values.flatten.map { _.toString } mkString ", " }
                  """.stripMargin
                storeMismatch(q, oks ++ failed, reportDir)
                println(msg)
                acc ++ (mism = true)
              } else if (failed.size == 0 && oks.size == 0) {
                val qid = mismatchEnumerator.next()
                storeInvalid(q, qid)
                acc ++ (invd = true)
              } else acc.++()
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


object Controller {

  // TODO Remove dead code
  val listingModel = Model("Listing", Seq(
    Field("id", Serial),
    Field("yearly_rent", Numeric),
    Field("sale_price", Numeric)
  ))

  var authorModel = Model("Author", Seq(
    Field("id", Serial),
    Field("first_name", VarChar(50)),
    Field("surname", VarChar(50))
  ))

  val bookModel = Model("Book", Seq(
    Field("id", Serial),
    Field("title", VarChar(100)),
    Field("isbn", VarChar(100)),
    Field("author", Foreign("Author"))
  ))

  val reviewModel = Model("Review", Seq(
    Field("id", Serial),
    Field("reviewer_name", VarChar(255)),
    Field("content", VarChar(255)),
    Field("rating", Int16),
    Field("book", Foreign("Book"))
  ))

  val listingSchema = Schema("listing", Map("Listing" -> listingModel))
  val bookSchema = Schema("book", Map(
    "Author" -> authorModel,
    "Book" -> bookModel,
    "Review" -> reviewModel
  ))

  def genSchemas() =
    List(bookSchema, listingSchema)

  def apply(options: Options) = {
    def isEmpty(x: String) = x == null || x.isEmpty
    def basenameWithoutExtension(x: String) = x.split("\\.(?=[^\\.]+$)").head.split("/").last
    Utils.setWorkDir()
    val testSessions: List[Future[Unit]] =
      options.mode match {
        case Some("auto") =>
          List.range(0, options.schemas) map { _ => SchemaGenerator() } map { s => Future {
            Utils.writeToFile(s.getSchemaPath, SchemaTranslator(s))
            TestRunnerCreator(options, s) match {
              case Success(testRunner) => testRunner.start()
              case Failure(e)          => println(e.getMessage)
            }
          }}
        case Some("select") =>
          List { Future {
            val dst = basenameWithoutExtension(options.sql)
            Utils.copyFile(options.sql, Utils.joinPaths(List(Utils.getSchemaDir(), dst)))
            val s = Schema(dst, Map())
            TestRunnerCreator(options, s) match {
                case Success(testRunner) => testRunner.start()
                case Failure(e)          => println(e.getMessage)
              }
          }}
        // TODO set testSessions based on options.dotCynthia
        case Some("replay") => ???
        case _ => ???
      }
    Await.ready(
      Future.sequence(testSessions) map { _ =>
        println("All testing sessions finished.")
      },
      Duration.Inf
    )
  }
}
