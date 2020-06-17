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
        schema, genTargets(orms, dbs), options.nuqueries))
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

class TestRunner(schema: Schema, targets: Seq[Target], queries: Int) {
  val mismatchEnumerator = Stream.from(1).iterator

  def genQuery(schema: Schema, limit: Int = 10) = {
    def _genQuery(i: Int): LazyList[Query] = {
      val q = QueryGenerator(schema)
      if (i >= limit) q #:: LazyList.empty
      else q #:: _genQuery(i + 1)
    }
    _genQuery(1)
  }

  def genListingQueries() =
    List(
      // Query 1
      SetRes(
        Union(
          New("Listing", Seq()),
          New("Listing", Seq())
        )
      ),

      // Query 2
      AggrRes(
        Seq(
          FieldDecl(Count(None), "count", IntF)
        ),
        Union(
          New("Listing", Seq()),
          Union(
            New("Listing", Seq()),
            New("Listing", Seq())
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
          New("Listing", Seq())
        )
      ),

      // Query 4
      SetRes(
        Apply(
          Sort(
            Seq(("Listing.yearly_rent", Asc))
          ),
          Union(
            New("Listing", Seq()),
            New("Listing", Seq())
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
          New("Listing", Seq())
        )
      ),

      // Query 6
      AggrRes(
        Seq(
          FieldDecl(Count(None), "count", IntF)
        ),
        Union(
          New("Listing", Seq()),
          Union(
            New("Listing", Seq()),
            New("Listing", Seq())
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
          New ("Listing", Seq())
        )
      ),

      // Query 8
      AggrRes(
        Seq(
          FieldDecl(Count(None), "count", IntF)
        ),
        New("Listing", Seq())
      ),

      // Query 9
      AggrRes(
        Seq(
          FieldDecl(Sum(F("Listing.sale_price")), "sum_sale", DoubleF),
          FieldDecl(Max(F("Listing.sale_price")), "max_sale", DoubleF),
          FieldDecl(Min(F("Listing.sale_price")), "min_sale", DoubleF),
          FieldDecl(Avg(F("Listing.sale_price")), "avg_sale", DoubleF)
        ),
        New("Listing", Seq())
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
        New("Listing", Seq())
      ),

      // Query 11
      AggrRes(
        Seq(
          FieldDecl(Max(F("Listing.foo")), "max", StringF),
          FieldDecl(Min(F("Listing.foo")), "min", StringF)
        ),
        New("Listing", Seq())
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
        New("Listing", Seq())
      ),

      // Query 13
      SetRes(
        Apply(
          Filter(
            Gte("custom", Constant("50", UnQuoted))
          ),
          New("Listing", Seq(
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
          New("Listing", Seq(
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
        New("Listing", Seq(
          FieldDecl(F("Listing.sale_price"), "sales", DoubleF),
          FieldDecl(Sum(F("Listing.yearly_rent")), "sum", DoubleF)
        ))
      ),

      // Query 16
      SetRes(
        Apply(
          Filter(Gte("sales", Constant("1", UnQuoted))),
          New("Listing", Seq(
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
      ),

      // Query 17
      SetRes(
        New("Listing", Seq(
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
      ),

      // Query 18
      SetRes(
        Apply(
          Filter(Gte("max", Add(Constant("10", UnQuoted), F("sales")))),
          New("Listing", Seq(
            FieldDecl(F("Listing.sale_price"), "sales", DoubleF),
            FieldDecl(
              Max(
                Add(
                  F("Listing.yearly_rent"),
                  Constant("10", UnQuoted)
                )
              ), "max", DoubleF)
          ))
        )
      ),

      // Query 19
      SetRes(
        Apply(
          Filter(
            And(
              Eq("fooF", Constant("baz", Quoted)),
              Gte("max", Add(Constant("10", UnQuoted), F("sales")))
            )
          ),
          New("Listing", Seq(
            FieldDecl(F("Listing.foo"), "fooF", StringF),
            FieldDecl(F("Listing.sale_price"), "sales", DoubleF),
            FieldDecl(
              Max(
                Add(
                  F("Listing.yearly_rent"),
                  Constant("10", UnQuoted)
                )
              ), "max", DoubleF)
          ))
        )
      ),

      // Query 20
      SetRes(
        New("Listing", Seq(
          FieldDecl(F("Listing.sale_price"), "sales", DoubleF),
          FieldDecl(
            Mul(
              F("Listing.sale_price"),
              F("Listing.sale_price")
            ),
            "mul", DoubleF, hidden = true
          ),
          FieldDecl(
            Sub(
              Avg(F("mul")),
              Constant("10", UnQuoted)
            ), "squared", DoubleF)
        ))
      ),

      // Query 21
      SetRes(
        New("Listing", Seq(
          FieldDecl(F("Listing.sale_price"), "sales", DoubleF),
          FieldDecl(
            Mul(
              F("Listing.sale_price"),
              F("Listing.sale_price")
            ),
            "mul", DoubleF, hidden = true
          ),
          FieldDecl(Avg(F("mul")), "squared", DoubleF)
        ))
      ),

      // Query 22
      SetRes(
        New("Listing", Seq(
          FieldDecl(F("Listing.sale_price"), "sales", DoubleF),
          FieldDecl(F("Listing.foo"), "fooF", StringF),
          FieldDecl(
            Mul(
              F("Listing.sale_price"),
              F("Listing.sale_price")
            ),
            "mul", DoubleF, hidden = true
          ),
          FieldDecl(Avg(F("mul")), "squared", DoubleF)
        ))
      ),

      // Query 23
      SetRes(
        New("Listing", Seq(
          FieldDecl(
            Mul(
              Avg(F("Listing.yearly_rent")),
              Avg(F("Listing.sale_price"))
            ),
            "mul",
            DoubleF
          )
        ))
      ),

      // Query 24
      SetRes(
        New(
          "Listing",
          Seq(
            FieldDecl(F("Listing.sale_price"), "V", DoubleF, false),
            FieldDecl(Mul(F("Listing.id"), F("V")), "jjG", DoubleF, false),
            FieldDecl(
              Mul(Div(F("Listing.id"), F("jjG")), Min(Sub(F("V"), Constant("7", UnQuoted)))),
              "qmIqh",
              DoubleF,
              false
            )
          )
        )
      ),

      // Query 25
      SetRes(
        New(
          "Listing",
          Seq(
            FieldDecl(Constant("JB", Quoted), "Jble", StringF, false),
            FieldDecl(Constant("1", UnQuoted), "cn", IntF, true),
            FieldDecl(
              Min(Add(Constant("qdbiycgpD", Quoted), Constant("3WnBi", Quoted))),
              "FJOKGoi",
              DoubleF,
              false
            )
          )
        )
      ),

      // Query 26
      SetRes(
        New(
          "Listing",
          Seq(
            FieldDecl(Constant("EcwE", Quoted), "wduesdvc", StringF, false),
            FieldDecl(Sub(F("wduesdvc"), F("wduesdvc")), "rrW", DoubleF, false),
            FieldDecl(Add(Constant("2", UnQuoted), F("Listing.sale_price")), "bCGVwr", DoubleF, false)
          )
        )
      ),

      // Query 27
      SetRes(
        New(
          "Listing",
          Seq(
            FieldDecl(Sub(F("Listing.sale_price"), F("Listing.sale_price")), "GTfzOMrj", DoubleF, true),
            FieldDecl(Sub(F("Listing.sale_price"), F("GTfzOMrj")), "lqA", DoubleF, false),
            FieldDecl(Min(F("Listing.sale_price")), "Rp", DoubleF, false),
            FieldDecl(Sum(F("GTfzOMrj")), "blny", DoubleF, false)
          )
        )
      ),

      // Query 28
      SetRes(
        New(
          "Listing",
          Seq(
            FieldDecl(Constant("3", UnQuoted), "WXDdG", IntF, false),
            FieldDecl(
              Mul(Sum(Add(F("Listing.id"), Constant("2", UnQuoted))), F("Listing.id")),
              "CBG",
              DoubleF,
              false
            )
          )
        )
      ),

      // Query 29
      SetRes(
        New(
          "Listing",
          Seq(
            FieldDecl(
              Add(
                Mul(Constant("3", UnQuoted), Sub(Add(F("Listing.yearly_rent"), F("Listing.id")), Sum(Constant("obAoS5v", Quoted)))),
                Constant("bfJWBP7p", Quoted)
              ),
              "QutFiZgOg",
              DoubleF,
              false
            ),
            FieldDecl(
              Avg(Sub(F("Listing.sale_price"), Constant("4", UnQuoted))),
              "SsJNcy",
              DoubleF,
              false
            ),
            FieldDecl(F("Listing.sale_price"), "wdVXax", DoubleF, false),
            FieldDecl(
              Sub(
                Add(F("Listing.id"), Min(F("Listing.sale_price"))),
                Div(F("SsJNcy"), Sub(F("SsJNcy"), Constant("HYQvPq", Quoted)))
              ),
              "wmWMTQf",
              DoubleF,
              false
            )
          )
        )
      ),

      // Query 30
      SetRes(
        New(
          "Listing",
          Seq(
            FieldDecl(Constant("6quxR", Quoted), "y", StringF, false),
            FieldDecl(Sum(Div(F("y"), Constant("2", UnQuoted))), "WIhPq", DoubleF, false)
          )
        )
      ),

      // Query 31
      SetRes(
        New(
          "Listing",
          Seq(
            FieldDecl(Max(F("Listing.sale_price")), "Bypp", DoubleF, true),
            FieldDecl(F("Bypp"), "XcRfBTT", DoubleF, false),
            FieldDecl(Constant("duySFSo3w", Quoted), "PsJ", StringF, true)
          )
        )
      ),

      // Query 32
      SetRes(
        New(
          "Listing",
          Seq(
            FieldDecl(
              Add(
                Sub(
                  Add(F("Listing.id"), Min(Mul(F("Listing.sale_price"), F("Listing.sale_price")))),
                  Constant("7", UnQuoted)
                ),
                Add(
                  F("Listing.sale_price"),
                  Div(
                    Add(
                      Add(Min(Constant("e5LIn", Quoted)), F("Listing.sale_price")),
                      Count(Some(F("Listing.id")))
                    ),
                    F("Listing.sale_price")
                  )
                )
              ),
              "heTkuJqO",
              DoubleF,
              true
            ),
            FieldDecl(F("heTkuJqO"), "BIKnH", DoubleF, false)
          )
        )
      ),

      // Query 33
      SetRes(
        New(
          "Listing",
          Seq(
            FieldDecl(Avg(F("Listing.id")), "Szgw", DoubleF, false),
            FieldDecl(Constant("VWSKU7", Quoted), "K", StringF, true),
            FieldDecl(F("Listing.sale_price"), "ee", DoubleF, false)
          )
        )
      ),

      // Query 34
      SetRes(
        New(
          "Listing",
          Seq(
            FieldDecl(
              Sub(Min(F("Listing.sale_price")), F("Listing.sale_price")),
              "ppVMYfS",
              DoubleF,
              true
            )
          )
        )
      ),

      // Query 35
      SetRes(
        New(
          "Listing",
          Seq(
            FieldDecl(Count(Some(F("Listing.id"))), "QpUmZQeX", IntF, false),
            FieldDecl(Mul(Constant("p4d", Quoted), Constant("qYCGT", Quoted)), "dvdddN", DoubleF, true)
          )
        )
      ),

      // Query 36
      SetRes(
        New(
          "Listing",
          Seq(
            FieldDecl(Count(Some(F("Listing.id"))), "jJqOpWLil", IntF, true),
            FieldDecl(F("Listing.sale_price"), "ij", DoubleF, true),
            FieldDecl(F("jJqOpWLil"), "FPCwjJlb", IntF, false)
          )
        )
      ),

      // Query 37
      SetRes(
        New(
          "Listing",
          Seq(
            FieldDecl(
              Div(Min(Constant("0", UnQuoted)), F("Listing.sale_price")),
              "yVmbvGHWe",
              DoubleF,
              true
            ),
            FieldDecl(Add(F("yVmbvGHWe"), F("yVmbvGHWe")), "Te", DoubleF, false)
          )
        )
      ),

      // Query 38
      SetRes(
        New(
          "Listing",
          List(
            FieldDecl(Constant("6", UnQuoted), "ORsHBgQ", IntF, false),
            FieldDecl(
              Div(Sum(Constant("pD1B", Quoted)), F("Listing.sale_price")),
              "othaPT",
              DoubleF,
              false
            ),
            FieldDecl(F("othaPT"), "gOtzP", DoubleF, false),
            FieldDecl(Avg(Constant("AOjkd", Quoted)), "S", DoubleF, true),
            FieldDecl(Constant("4", UnQuoted), "x", IntF, true),
            FieldDecl(F("gOtzP"), "G", DoubleF, true)
          )
        )
      ),

      // Query 39
      SetRes(
        New(
          "Listing",
          List(
            FieldDecl(Avg(Constant("1", UnQuoted)), "irOHtSumD", DoubleF, false),
            FieldDecl(F("irOHtSumD"), "jiAtXcec", DoubleF, false)
          )
        )
      ),

      // Query 40
      SetRes(
        New(
          "Listing",
          List(
            FieldDecl(Sum(F("Listing.yearly_rent")), "eqQy", DoubleF, false),
            FieldDecl(F("eqQy"), "rQe", DoubleF, true),
            FieldDecl(Count(Some(F("Listing.id"))), "dvcVCurv", IntF, true),
            FieldDecl(F("eqQy"), "I", DoubleF, false),
            FieldDecl(F("Listing.sale_price"), "TPH", DoubleF, true),
            FieldDecl(Constant("5", UnQuoted), "bqZCpnWO", IntF, true)
          )
        )
      ),

      // Query 41
      SetRes(
        New(
          "Listing",
          List(
            FieldDecl(Sub(F("Listing.sale_price"), F("Listing.yearly_rent")), "EoDFfvD", DoubleF, true),
            FieldDecl(Min(Constant("8", Quoted)), "aJZXPcub", StringF, false),
            FieldDecl(Div(F("Listing.sale_price"), F("EoDFfvD")), "KWpgDGBLp", DoubleF, false)
          )
        )
      ),

      // Query 42
      SetRes(
        New(
          "Listing",
          List(
            FieldDecl(
              Div(
                Sub(Constant("M0rf", Quoted), Mul(F("Listing.sale_price"), Max(F("Listing.yearly_rent")))),
                F("Listing.sale_price")
              ),
              "nbos",
              DoubleF,
              true
            ),
            FieldDecl(
              Mul(
                Div(Count(Some(F("Listing.id"))), Div(F("nbos"), Constant("7G", Quoted))),
                Add(F("nbos"), Constant("ilkQN", Quoted))
              ),
              "lxg",
              DoubleF,
              false
            ),
            FieldDecl(Constant("L", Quoted), "xoOkrOnc", StringF, false),
            FieldDecl(Div(Constant("5", UnQuoted), F("Listing.id")), "vHtTZ", DoubleF, true),
            FieldDecl(F("xoOkrOnc"), "UBXB", StringF, false),
            FieldDecl(Constant("NZ2", Quoted), "SNzkSd", StringF, false),
            FieldDecl(Avg(F("Listing.yearly_rent")), "MdHFe", DoubleF, false)
          )
        )
      ),

      // Query 43
      SetRes(
        Apply(
          Sort(List(("cCqQb", Asc))),
          New(
            "Listing",
            List(
              FieldDecl(Div(F("Listing.yearly_rent"), F("Listing.sale_price")), "WrTj", DoubleF, false),
              FieldDecl(Constant("5", UnQuoted), "MPi", IntF, true),
              FieldDecl(Count(Some(Constant("wAi8pi4f", Quoted))), "jcVbPimsG", IntF, false),
              FieldDecl(F("WrTj"), "hj", DoubleF, true),
              FieldDecl(Constant("0", UnQuoted), "cCqQb", IntF, false)
            )
          )
        )
      ),
      AggrRes(
        List(FieldDecl(Count(Some(F("EfLr"))), "KiqJoUU", IntF, false)),
        New(
          "Listing",
          List(
            FieldDecl(
              Sub(
                Add(
                  F("Listing.id"),
                  Div(
                    Add(
                      Div(Constant("9", UnQuoted), Constant("n3zKcdj2", Quoted)),
                      Add(
                        Div(Div(Constant("8", UnQuoted), Constant("1", UnQuoted)), F("Listing.id")),
                        F("Listing.yearly_rent")
                      )
                    ),
                    Constant("px5me", Quoted)
                  )
                ),
                Sub(Constant("y1e8rj", Quoted), Constant("f1cay92kV", Quoted))
              ),
              "EfLr",
              DoubleF,
              false
            )
          )
        )
      ),


      AggrRes(
        List(FieldDecl(Count(Some(F("R"))), "R", IntF, false)),
        New(
          "Listing",
          List(
            FieldDecl(F("Listing.sale_price"), "R", DoubleF, false),
            FieldDecl(Div(F("Listing.sale_price"), F("R")), "vlrkx", DoubleF, false),
            FieldDecl(F("vlrkx"), "mUoSMAxdl", DoubleF, false),
            FieldDecl(
              Div(Mul(Constant("8", UnQuoted), F("Listing.id")), Constant("PHoJX5b", Quoted)),
              "uBnSWipsi",
              DoubleF,
              false
            ),
            FieldDecl(
              Div(Constant("9", UnQuoted), Add(Constant("", Quoted), Constant("1", UnQuoted))),
              "OAYwxOOET",
              DoubleF,
              false
            )
          )
        )
      )

    )

  def genBooksQueries() =
    Seq(

      // Query 1
      SetRes(New("Book", Seq())),

      // Query 2
      SetRes(
        Apply(
          Sort(Seq(("Review.rating", Desc))),
          New("Review", Seq())
        )
      ),

      // Query 3
      SetRes(
        Apply(
          Filter(Eq("Review.book.title", Constant("Random book", Quoted))),
          New("Review", Seq())
        )
      ),

      // Query 3
      SetRes(
        Apply(
          Filter(Eq("Review.book.author.surname", Constant("Coecker", Quoted))),
          New("Review", Seq())
        )
      ),

      // Query 4
      SetRes(
        Apply(
          Sort(Seq(
            ("Review.book.title", Desc))
          ),
          New("Review", Seq())
        )
      ),

      // Query 5
      SetRes(
        Apply(
          Sort(Seq(
            ("Review.book.title", Desc),
            ("Review.reviewer_name", Asc))
          ),
          New("Review", Seq())
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
            New("Review", Seq())
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
            New("Review", Seq())
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
            New("Review", Seq())
          )
        )
      ),

      // Query 10
      SetRes(
        Apply(
          Filter(
            And (
              Gte("Review.rating", Constant("2", UnQuoted)),
              Lte("Review.book.author.first_name", Constant("Z", Quoted))
            )
          ),
          New("Review", Seq(
            FieldDecl(
              Mul(
                F("Review.rating"),
                Constant("-1", UnQuoted)
              ),
              "mul", DoubleF
            ),
            FieldDecl(F("Review.book.author.first_name"), "name", StringF)
          ))
        )
      ),

      // Query 11
      SetRes(
        New(
          "Review",
          List(
            FieldDecl(Constant("3", UnQuoted), "dqBZvjQX", IntF, true),
            FieldDecl(F("Review.book.author.first_name"), "TbPEVGKp", StringF, false)
          )
        )
      ),

      // Query 12
      SetRes(
        New(
          "Review",
          List(
            FieldDecl(Constant("3", UnQuoted), "dqBZvjQX", IntF, true),
            FieldDecl(F("Review.book.author.first_name"), "TbPEVGKp", StringF, false)
          )
        )
      ),

      // Query 13
      SetRes(
        Apply(
          Sort(
            List(
              ("Review.content", Asc),
              ("Review.book.id", Asc),
              ("Review.id", Asc),
              ("Review.reviewer_name", Asc)
            )
          ),
          New(
            "Review",
            List(
              FieldDecl(
                Sub(Constant("8", UnQuoted), Sum(Add(Constant("t7nVx", Quoted), F("Review.content")))),
                "DniIHgWk",
                DoubleF,
                false
              )
            )
          )
        )
      ),

      // Query 14
      SetRes(
        New(
          "Author",
          List(
            FieldDecl(
              Add(
                Mul(
                  Sub(Add(Min(Constant("T26", Quoted)), F("Author.first_name")), Constant("5", UnQuoted)),
                  Constant("8", UnQuoted)
                ),
                Add(F("Author.first_name"), Avg(Constant("4", UnQuoted)))
              ),
              "YUA",
              DoubleF,
              false
            ),
            FieldDecl(Avg(Div(Constant("2BRyR3", Quoted), Constant("3", UnQuoted))), "oD", DoubleF, false),
            FieldDecl(Constant("7", UnQuoted), "C", IntF, true)
          )
        )
      ),

      // Query 15
      SetRes(
        Apply(
          Sort(
            List(
              ("Review.id", Asc),
              ("V", Desc),
              ("Review.reviewer_name", Desc),
              ("Review.content", Desc),
              ("Review.book.id", Desc)
            )
          ),
          New("Review", List(FieldDecl(Constant("0", UnQuoted), "V", IntF, false)))
        )
      ),

      // Query 16
      SetRes(
        Apply(
          Sort(List(("Book.author.id", Asc), ("Book.isbn", Asc), ("Book.id", Desc))),
          New(
            "Book",
            List(
              FieldDecl(F("Book.title"), "e", StringF, true),
              FieldDecl(Constant("", Quoted), "YejRlb", StringF, true),
              FieldDecl(Sub(F("YejRlb"), F("Book.isbn")), "aqVzP", DoubleF, false),
              FieldDecl(Constant("0", UnQuoted), "PYBKyS", IntF, true)
            )
          )
        )
      ),

      // Query 17
      SetRes(
        Apply(
          Sort(List(("Book.isbn", Desc), ("Izi", Desc), ("Book.author.id", Asc))),
          New(
            "Book",
            List(
              FieldDecl(Sub(F("Book.author.first_name"), F("Book.title")), "RvimPPZOm", DoubleF, false),
              FieldDecl(Max(Constant("Xl950", Quoted)), "Izi", StringF, false)
            )
          )
        )
      ),

      // Query 18
      SetRes(
        Apply(
          Sort(List(("Book.isbn", Desc), ("Book.title", Desc), ("ODQw", Desc), ("Book.id", Desc))),
          New(
            "Book",
            List(
              FieldDecl(Constant("0", UnQuoted), "gLMHoT", IntF, true),
              FieldDecl(Add(Sum(F("Book.title")), Max(F("Book.isbn"))), "caxccB", DoubleF, false),
              FieldDecl(Constant("8", UnQuoted), "VUV", IntF, false),
              FieldDecl(Sum(Div(F("gLMHoT"), Constant("VY", Quoted))), "ODQw", DoubleF, false)
            )
          )
        )
      ),

      AggrRes(
        List(FieldDecl(Count(Some(F("Review.content"))), "g", IntF, false)),
        New(
          "Review",
          List(
            FieldDecl(Constant("SBkK", Quoted), "vICp", StringF, false),
            FieldDecl(Add(F("Review.content"), Div(F("vICp"), F("vICp"))), "OIeH", DoubleF, false),
            FieldDecl(F("vICp"), "sEwe", StringF, false),
            FieldDecl(Mul(F("Review.id"), F("Review.id")), "KB", DoubleF, false),
            FieldDecl(Constant("4", UnQuoted), "JFSL", IntF, false)
          )
        )
      ),

      
      SetRes(
        New(
          "Review",
          List(
            FieldDecl(Sum(Div(F("Review.rating"), F("Review.book.title"))), "TkhJ", DoubleF, false),
            FieldDecl(Sub(F("Review.rating"), F("TkhJ")), "II", DoubleF, true)
          )
        )
      )
    )

  def genQueries(): Seq[Query] =
    genQuery(schema, limit = queries)

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
    val stats = genQueries()
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
    Utils.setWorkDir()
    val testSessions =
      List.range(0, options.schemas) map { _ => SchemaGenerator() } map { s => Future {
        Utils.writeToFile(s.getSchemaPath, SchemaTranslator(s))
        TestRunnerCreator(options, s) match {
          case Success(testRunner) => testRunner.start()
          case Failure(e)          => println(e.getMessage)
        }
      }}
    Await.ready(
      Future.sequence(testSessions) map { _ =>
        println("All testing sessions finished.")
      },
      Duration.Inf
    )
  }
}
