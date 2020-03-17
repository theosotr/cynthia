package gr.dmst.aueb.cynthia


import scala.util.{Try, Success, Failure}


case class Target(orm: ORM, db: DB)

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
      SetRes(Union(New ("Listing", None), New ("Listing", None)))
    )

  def start() =
    genQueries()
      .foreach { q =>
        targets.foreach { t =>
          ORMTranslator.translate(q, t)
        }
      }
}
