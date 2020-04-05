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
          New("Listing", Set()),
          New("Listing", Set())
        )
      ),

      // Query 2
      AggrRes(
        Seq(
          FieldDecl(Count(None), "count", IntF)
        ),
        Union(
          New("Listing", Set()),
          Union(
            New("Listing", Set()),
            New("Listing", Set())
          )
        )
      ),

      // Query3
      SetRes(
        Apply(
          Filter(
            Not(
              Eq("Listing.foo", Constant("bar", Quoted))
            )
          ),
          New("Listing", Set())
        )
      ),

      // Query 4
      SetRes(
        Apply(
          Sort(
            Seq(("Listing.yearly_rent", Asc))
          ),
          Union(
            New("Listing", Set()),
            New("Listing", Set())
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
          New("Listing", Set())
        )
      ),

      // Query 6
      AggrRes(
        Seq(
          FieldDecl(Count(None), "count", IntF)
        ),
        Union(
          New("Listing", Set()),
          Union(
            New("Listing", Set()),
            New("Listing", Set())
          )
        )
      ),

      // Query 7
      SetRes(
        Apply(
          Filter(
            And(
              Not(
                Eq("Listing.foo", Constant("bar", Quoted))
              ),
              Gte("Listing.sale_price", Constant("100", UnQuoted))
            )
          ),
          New ("Listing", Set())
        )
      ),

      // Query 8
      AggrRes(
        Seq(
          FieldDecl(Count(None), "count", IntF)
        ),
        New("Listing", Set())
      ),

      // Query 9
      AggrRes(
        Seq(
          FieldDecl(Sum(F("Listing.sale_price")), "sum_sale", DoubleF),
          FieldDecl(Max(F("Listing.sale_price")), "max_sale", DoubleF),
          FieldDecl(Min(F("Listing.sale_price")), "min_sale", DoubleF),
          FieldDecl(Avg(F("Listing.sale_price")), "avg_sale", DoubleF)
        ),
        New("Listing", Set())
      ),

      // Query 10
      AggrRes(
        Seq(
          FieldDecl(
            Add(
              Mul(
                Sum(F("Listing.sale_price")),
                Avg(F("Listing.yearly_rent"))
              ),
              Div(
                Max(F("Listing.sale_price")),
                Min(F("Listing.sale_price"))
              )
            ),
            "complex_add",
            DoubleF
          ),
          FieldDecl(Sum(F("Listing.yearly_rent")), "yearly", DoubleF),
          FieldDecl(
            Sub(
              Min(F("Listing.yearly_rent")),
              Add(
                Avg(F("Listing.sale_price")),
                Max(F("Listing.sale_price"))
              )
            ),
            "complex_sub",
            DoubleF
          )
        ),
        New("Listing", Set())
      ),

      // Query 11
      AggrRes(
        Seq(
          FieldDecl(Max(F("Listing.foo")), "max", StringF),
          FieldDecl(Min(F("Listing.foo")), "min", StringF)
        ),
        New("Listing", Set())
      ),

      // Query 12
      AggrRes(
        Seq(
          FieldDecl(
            Sum(
              Sub(
                F("Listing.yearly_rent"),
                F("Listing.sale_price")
              )
            ),
            "sum",
            DoubleF
          )
        ),
        New("Listing", Set())
      ),

      // Query 13
      SetRes(
        Apply(
          Filter(
            Gte("custom", Constant("50", UnQuoted))
          ),
          New("Listing", Set(
            FieldDecl(
              Add(
                F("Listing.yearly_rent"),
                F("Listing.sale_price")
              ),
              "custom",
              DoubleF
            )
          ))
        )
      ),

      // Query 14
      SetRes(
        Apply(
          Filter(
            And(
              Eq("custom", Constant("20", UnQuoted)),
              Eq("text", Constant("foobar", Quoted))
            )
          ),
          New("Listing", Set(
            FieldDecl(
              Add(
                Constant("5", UnQuoted),
                Constant("15", UnQuoted)
              ),
              "custom", IntF
            ),
            FieldDecl(
              Constant("foobar", Quoted),
              "text", StringF
            )
          ))
        )
      ),

      // Query 15
      SetRes(
        Apply(
          GroupBy(Seq("sales")),
          New("Listing", Set(
            FieldDecl(F("Listing.sale_price"), "sales", DoubleF),
            FieldDecl(Sum(F("Listing.yearly_rent")), "sum", DoubleF)
          ))
        )
      ),

      // Query 16
      SetRes(
        Apply(
          Filter(Gte("sales", Constant("1", UnQuoted))),
          Apply(
            GroupBy(Seq("sales")),
            New("Listing", Set(
              FieldDecl(
                Mul(
                  Constant("10", UnQuoted),
                  Div(
                    Constant("5", UnQuoted),
                    F("Listing.sale_price")
                  )
                ),
                "sales", DoubleF
              ),
              FieldDecl(Sum(F("Listing.yearly_rent")), "sum", DoubleF)
            ))
          )
        )
      ),

      // Query 17
      SetRes(
        Apply(
          GroupBy(Seq("sales")),
          New("Listing", Set(
            FieldDecl(F("Listing.sale_price"), "sales", DoubleF),
            FieldDecl(
              Avg(
                Mul(
                  F("Listing.sale_price"),
                  F("Listing.sale_price")
                )
              ),
              "squared", DoubleF
            )
          ))
        )
      )
    )

  def genBooksQueries() =
    Seq(

      // Query 1
      SetRes(New("Book", Set())),

      // Query 2
      SetRes(
        Apply(
          Sort(Seq(("Review.rating", Desc))),
          New("Review", Set())
        )
      ),

      // Query 3
      SetRes(
        Apply(
          Filter(Eq("Review.book.title", Constant("Random book", Quoted))),
          New("Review", Set())
        )
      ),

      // Query 3
      SetRes(
        Apply(
          Filter(Eq("Review.book.author.surname", Constant("Coecker", Quoted))),
          New("Review", Set())
        )
      ),

      // Query 4
      SetRes(
        Apply(
          Sort(Seq(
            ("Review.book.title", Desc))
          ),
          New("Review", Set())
        )
      ),

      // Query 5
      SetRes(
        Apply(
          Sort(Seq(
            ("Review.book.title", Desc),
            ("Review.reviewer_name", Asc))
          ),
          New("Review", Set())
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
                Gte("Review.rating", Constant("2", UnQuoted)),
                Lte("Review.rating", Constant("4", UnQuoted))
              )
            ),
            New("Review", Set())
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
                Gte("Review.rating", Constant("2", UnQuoted)),
                Contains("Review.book.author.surname", Constant("o", Quoted))
              )
            ),
            New("Review", Set())
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
                Gte("Review.rating", Constant("2", UnQuoted)),
                Contains("Review.book.author.surname", Constant("o", Quoted))
              )
            ),
            New("Review", Set())
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
              Gte("Review.rating", Constant("2", UnQuoted))
            ),
            New("Review", Set())
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
