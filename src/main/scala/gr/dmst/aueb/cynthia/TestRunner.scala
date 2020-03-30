package gr.dmst.aueb.cynthia

import java.io.File
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.{Try, Success, Failure}
import scala.language.postfixOps

import pprint.PPrinter.BlackWhite


case class Target(orm: ORM, db: DB) {
  def getTargetCommand() = orm match {
    case Django (_, _, _) | SQLAlchemy (_, _) => "python3 " + orm.getDriverPath(db)
    case Sequelize(_, _) => "node " + orm.getDriverPath(db)
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
        case "django"     => Django(dbname,
                                   Utils.joinPaths(List(workdir, "django")),
                                   "djangoproject")
        case "sqlalchemy" => SQLAlchemy(dbname,
                                        Utils.joinPaths(List(workdir, "sqlalchemy")))
        case "sequelize"  => Sequelize(dbname,
                                       Utils.joinPaths(List(workdir, "sequelize")))
        case _            => ???
      }
    }

  def genTargets(orms: Seq[ORM], backends: Seq[DB]) =
    orms.flatMap(x => backends.map(y => Target(x, y)))

  def apply(options: Options, schema: String) = {
    val dbname = schema.replace(".sql", "")
    val schemaPath = Utils.joinPaths(List(options.schemas, schema))
    val dbDir = Utils.joinPaths(List(Utils.getDBDir(), dbname))
    val createdb = Try(genBackends(options.dbs, dbDir, None).foreach { db =>
      DBSetup.createdb(db, dbname)
    })
    val dbs = genBackends(options.dbs, dbDir, Some(dbname))
    val schemadb = createdb.flatMap { _ =>
      Try(dbs.foreach { db =>
        DBSetup.setupSchema(db, schemaPath)
      })
    }
    val projectDir = Utils.joinPaths(List(Utils.getProjectDir(), dbname))
    val orms = genORMs(options.orms, dbname, projectDir)
    schemadb.flatMap { _ =>
      Try(Utils.createDir(projectDir))
        .flatMap { _ => Try (
          orms.foreach { orm => ProjectCreator.createProject(orm, dbs) })
        }
    } match {
      case Success(_) => Success(new TestRunner(dbname, genTargets(orms, dbs)))
      case Failure(e) => Failure(e)
    }
  }
}


class TestRunner(schema: String, targets: Seq[Target]) {
  val mismatchEnumerator = Stream.from(1).iterator

  def genListingQueries() =
    List(
      // Query 1
      SetRes(
        Union(
          New("Listing", None),
          New("Listing", None)
        )
      ),

      // Query 2
      AggrRes(
        Seq(Count(Some("count"))),
        Union(
          New("Listing", None),
          Union(
            New("Listing", None),
            New("Listing", None)
          )
        )
      ),

      // Query3
      SetRes(
        Apply(
          Filter(
            Not(
              Eq("Listing.foo", Value("bar", Quoted))
            )
          ),
          New("Listing", None)
        )
      ),

      // Query 4
      SetRes(
        Apply(
          Sort(
            Seq(("Listing.yearly_rent", Asc))
          ),
          Union(
            New("Listing", None),
            New("Listing", None)
          )
        )
      ),
 
      // Query 5
      SetRes(
        Apply(
          Sort(
            Seq(
              ("Listing.foo", Desc),
              ("Listing.sale_price", Asc)
            )
          ),
          New("Listing", None)
        )
      ),

      // Query 6
      AggrRes(
        Seq(Count(Some("count"))),
        Union(
          New("Listing", None),
          Union(
            New("Listing", None),
            New("Listing", None)
          )
        )
      ),

      // Query 7
      SetRes(
        Apply(
          Filter(
            And(
              Not(
                Eq("Listing.foo", Value("bar", Quoted))
              ),
              Gte("Listing.sale_price", Value("100", UnQuoted))
            )
          ),
          New ("Listing", None)
        )
      ),

      // Query 8
      AggrRes(
        Seq(Count(Some("count"))),
        New("Listing", None)
      ),

      // Query 9
      AggrRes(
        Seq(
          Sum("Listing.sale_price", Some("sum_sale")),
          Max("Listing.sale_price", Some("max_sale")),
          Min("Listing.sale_price", Some("min_sale")),
          Avg("Listing.sale_price", Some("avg_sale"))
        ),
        New("Listing", None)
      ),

      // Query 10
      AggrRes(
        Seq(
          Add(
            Mul(
              Sum("Listing.sale_price", None),
              Avg("Listing.yearly_rent", None)
            ),
            Div(
              Max("Listing.sale_price", None),
              Min("Listing.sale_price", None)
            ),
            Some("complex_add")
          ),
          Sum("Listing.yearly_rent", Some("yearly")),
          Sub(
            Min("Listing.yearly_rent"),
            Add(
              Avg("Listing.sale_price"),
              Max("Listing.sale_price")
            ),
            Some("complex_sub")
          )
        ),
        New("Listing", None)
      ),

      // Query 11
      AggrRes(
        Seq(
          Max("Listing.foo", Some("max")),
          Min("Listing.foo", Some("min"))
        ),
        New("Listing", None)
      )
    )

  def genBooksQueries() =
    Seq(

      // Query 1
      SetRes(New("Book", None)),

      // Query 2
      SetRes(
        Apply(
          Sort(Seq(("Review.rating", Desc))),
          New("Review", None)
        )
      ),

      // Query 3
      SetRes(
        Apply(
          Filter(Eq("Review.book.title", Value("Random book", Quoted))),
          New("Review", None)
        )
      ),

      // Query 3
      SetRes(
        Apply(
          Filter(Eq("Review.book.author.surname", Value("Coecker", Quoted))),
          New("Review", None)
        )
      ),

      // Query 4
      SetRes(
        Apply(
          Sort(Seq(
            ("Review.book.title", Desc))
          ),
          New("Review", None)
        )
      ),

      // Query 5
      SetRes(
        Apply(
          Sort(Seq(
            ("Review.book.title", Desc),
            ("Review.reviewer_name", Asc))
          ),
          New("Review", None)
        )
      ),

      // Query 6
      SetRes(
        Apply(
          Sort(Seq(
            ("Review.book.title", Desc),
            ("Review.reviewer_name", Asc))
          ),
          Apply(
            Filter(
              And(
                Gte("Review.rating", Value("2", UnQuoted)),
                Lte("Review.rating", Value("4", UnQuoted))
              )
            ),
            New("Review", None)
          )
        )
      ),

      // Query 7
      SetRes(
        Apply(
          Sort(Seq(
            ("Review.book.title", Desc),
            ("Review.reviewer_name", Asc))
          ),
          Apply(
            Filter(
              And(
                Gte("Review.rating", Value("2", UnQuoted)),
                Contains("Review.book.author.surname", "o")
              )
            ),
            New("Review", None)
          )
        )
      ),

      // Query 8
      FirstRes(
        Apply(
          Sort(Seq(
            ("Review.book.title", Desc),
            ("Review.reviewer_name", Asc))
          ),
          Apply(
            Filter(
              And(
                Gte("Review.rating", Value("2", UnQuoted)),
                Contains("Review.book.author.surname", "o")
              )
            ),
            New("Review", None)
          )
        )
      ),

      // Query 9
      SubsetRes(
        1,
        Some(3),
        Apply(
          Sort(Seq(
            ("Review.book.title", Desc),
            ("Review.reviewer_name", Asc))
          ),
          Apply(
            Filter(
              Gte("Review.rating", Value("2", UnQuoted))
            ),
            New("Review", None)
          )
        )
      )
    )

  def genQueries() = schema match {
    case "listing" => genListingQueries()
    case "books"   => genBooksQueries()
    case _         => genListingQueries()
  }

  def getReportDir(i: Int) =
    Utils.joinPaths(List(
      Utils.getMismatchesDir(),
      schema,
      i.toString)
    )

  def storeMismatch(q: Query, mismatches: Map[String, Seq[Target]],
                    reportDir: String) = {
    new File(reportDir).mkdirs
    val queryFile = Utils.joinPaths(List(reportDir, "query.aql"))
    Utils.writeToFile(queryFile, BlackWhite.tokenize(q).mkString)
    mismatches.foreach { case (k, v) => v.foreach { x =>
        val filename = s"${x.orm.ormName}_${x.db.getName}.out"
        Utils.writeToFile(Utils.joinPaths(List(reportDir, filename)), k)
      }
    }
  }

  def start() =
    genQueries()
      .foreach { q => {
        val futures = targets.map { t =>
          Future {
            (t, QueryExecutor(q, t))
          }
        }
        val f = Future.sequence(futures) map { res =>
            val results = (
              Map[String, Seq[Target]](),
              Map[String, Seq[Target]]()
            )
            val (oks, failed) =
              res.foldLeft(results) { case ((oks, failed), x) => {
                x match {
                  case (target, Unsupported(_)) => (oks, failed)
                  case (target, Ok(res)) => {
                    val targets = oks getOrElse(res, Seq())
                    (oks + (res -> (targets :+ target)), failed)
                  }
                  case (target, Fail(err)) => {
                    val targets = failed getOrElse(err, Seq())
                    (oks, failed + (err -> (targets :+ target)))
                  }
                }
              }}
            if (failed.size >= 1 || oks.size > 1) {
              val qid = mismatchEnumerator.next()
              val reportDir = getReportDir(qid)
              val msg =
                s"""${Console.GREEN}[INFO]: Mismatch found in schema '$schema':${Console.WHITE}
                |  - Query ID: $qid
                |  - Report Directory: $reportDir
                |  - OK Target Groups:\n${oks.map { case (_, v) =>
                    "    * " + (v.map {_.toString} mkString ", ")
                  } mkString "\n"}
                |  - Failed Target Group: ${failed.values.flatten.map { _.toString } mkString ", " }
                """.stripMargin
              storeMismatch(q, oks ++ failed, reportDir)
              println(msg)
            }
        }
        Await.result(f, 5 seconds) 
      }
    }
}
