package gr.dmst.aueb.cynthia

import scala.sys.process._


object ProjectCreator {

  def createProject(orm: ORM, dbs: Seq[DB]) = dbs match {
    case Nil    => ()
    case h :: _ => {
      setupProject(orm, dbs)
      createModels(orm, h)
    }
  }

  def setupProject(orm: ORM, dbs: Seq[DB]) = orm match {
    case Django (_, _, _)  => createDjangoProject(orm, dbs)
    case SQLAlchemy (_, _) => createSQLAlchemyProject(orm, dbs)
    case Sequelize (_, _)  => createSequelizeProject(orm, dbs)
  }

  def createModels(orm: ORM, db: DB) = orm match {
    case Django(_, pdir, _) => {
      val models = Utils.runCmd("python3 manage.py inspectdb", Some(pdir))
      val models2 = models.replaceAll(
        "id = models.AutoField\\(", "id = models.AutoField\\(primary_key=True,")
      Utils.writeToFile(orm.getModelsPath(), models2)
    }
    case SQLAlchemy(_, pdir) => {
      val models = Utils.runCmd("sqlacodegen " + db.getURI, None)
      Utils.writeToFile(orm.getModelsPath(), models)
    }
    case Sequelize(_, pdir) => {
      val bcmd = "node_modules/sequelize-auto/bin/sequelize-auto"
      val cmd = db match {
        case Postgres(user, password, dbname) =>
          Seq(
            bcmd, "-h", "localhost",
            "-u", user, "-x", password, "-d",
            dbname, "--dialect", db.getName(),
            "-o", pdir
          ).mkString(" ")
        case MySQL(user, password, dbname) =>
          Seq(
            bcmd, "-h", "localhost",
            "-u", user, "-x", password, "-d",
            dbname, "--dialect", db.getName(),
            "-o", pdir
          ).mkString(" ")
        case SQLite(dbname) =>
          Seq(
            bcmd, "-h", "localhost",
            "-u", "foo", "-d",
            dbname, "--dialect", db.getName(),
            "-o", pdir
          ).mkString(" ")
      }
      Utils.runCmd(cmd, None)
    }
  }

  def createSequelizeProject(orm: ORM, dbs: Seq[DB]) = orm match {
    case Sequelize(name, pdir) => Utils.emptyFile(pdir)
    case _                     => ()
  }

  def createSQLAlchemyProject(orm: ORM, dbs: Seq[DB]) = orm match {
    case SQLAlchemy(name, pdir) => {
      Utils.emptyFile(pdir)
      Utils.writeToFile(Utils.joinPaths(List(pdir, "__init__.py")), "")
    }
    case _ => ()
  }

  def createDjangoProject(orm: ORM, dbs: Seq[DB]) = orm match {
    case Django(name, pdir, setDir) => {
      Utils.emptyFile(pdir)
      Utils.runCmd("django-admin startproject djangoproject " + pdir, None)
      Utils.runCmd("python3 manage.py startapp " + name, Some(pdir))
      val dbsettings = dbs.foldLeft("DATABASES = {\n") { (acc, db) => db match {
          case Postgres(user, password, dbname) =>
            acc + "'postgres': {\n" +
              Map(
                "'NAME'" -> ("'" + dbname + "'"),
                "'HOST'" -> "'localhost'",
                "'USER'" -> ("'" + user + "'"),
                "'PASSWORD'" -> ("'" + password + "'"),
                "'PORT'" -> "5432",
                "'ENGINE'" -> "'django.db.backends.postgresql'"
              ).map(_.productIterator.mkString(":")).mkString(",\n") +
            "},"
          case MySQL(user, password, dbname) =>
            acc + "'mysql': {" +
              Map(
                "'NAME'" -> ("'" + dbname + "'"),
                "'HOST'" -> "'localhost'",
                "'USER'" -> ("'" + user + "'"),
                "'PASSWORD'" -> ("'" + password + "'"),
                "'PORT'" -> "3306",
                "'ENGINE'" -> "'django.db.backends.mysql'"
              ).map(_.productIterator.mkString(":")).mkString(",\n") +
            "},"
          case SQLite(dbname) =>
            acc +"'default': {" +
              Map(
                "'NAME'" -> ("'" + dbname + "'"),
                "'HOST'" -> "''",
                "'USER'" -> "''",
                "'PASSWORD'" -> "''",
                "'PORT'" -> "''",
                "'ENGINE'" -> "'django.db.backends.sqlite3'"
              ).map(_.productIterator.mkString(":")).mkString(",\n") +
            "},"
        }
      } + "}\n" + "INSTALLED_APPS += ['" + name + "']"
      Utils.appendToFile(orm.getSettingsPath(), dbsettings)
    }
    case _ => ()
  }
}
