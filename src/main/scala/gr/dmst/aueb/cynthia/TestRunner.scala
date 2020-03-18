package gr.dmst.aueb.cynthia

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.{Try, Success, Failure}
import scala.language.postfixOps


case class Target(orm: ORM, db: DB) {
  def getTargetCommand() = orm match {
    case Django (_, _, _) | SQLAlchemy (_, _) => "python3 " + orm.getDriverPath(db)
  }

  override def toString() =
    orm.getName() + "[" + db.getName() + "]"
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
      case Success(_) => Success(new TestRunner(genTargets(orms, dbs)))
      case Failure(e) => Failure(e)
    }
  }
}


class TestRunner(targets: Seq[Target]) {

  def genQueries() =
    List(
      SetRes(Union(New ("Listing", None), New ("Listing", None))),
      AggrRes(Count, New ("Listing", None)),
      AggrRes(Count, Union(New("Listing", None), Union(New("Listing", None), New("Listing", None)))),
      SetRes(Apply(Filter(Not(Eq("Listing.foo", Value("bar", Quoted)))), New ("Listing", None))),
      SetRes(Apply(Filter(And(Not(Eq("Listing.foo", Value("bar", Quoted))), Gte("Listing.sale_price", Value("100", UnQuoted)))), New ("Listing", None))),
      SetRes(Apply(Sort(Seq(("Listing.foo", Desc), ("Listing.sale_price", Asc))), New("Listing", None))),
      SetRes(Apply(Sort(Seq(("Listing.yearly_rent", Asc))), Union(New("Listing", None), New("Listing", None))))
    )

  def start() =
    genQueries()
      .foreach { q => {
        val futures = targets.map { t =>
          Future {
            (t, QueryExecutor(ORMTranslator(q, t), t))
          }
        }
        val f = Future.sequence(futures) map { res =>
            val comps =
              res.foldLeft(Map[String, Seq[String]]()) ((acc, x) => {
                val (target, res) = x
                acc + (res.toString ->
                  (acc.getOrElse(res.toString(), Seq()) :+ target.toString()))
              })
            if (comps.size > 1) {
              println("Mismatches\n")
              comps.foreach {
                case (_, v) => {
                  println("Target Group: " + v.mkString(","))
                }
              }
            }
        }
        Await.result(f, 5 seconds) 
      }
    }
}
