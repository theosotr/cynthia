package gr.dmst.aueb.cynthia

import scala.sys.process._


object ProjectCreator {

  def createProject(orm: ORM, dbs: List[DB]) = {
    setupProject(orm, dbs)
    createModels(orm)
  }

  def setupProject(orm: ORM, dbs: List[DB]) = orm match {
    case Django(_, _, _) => createDjangoProject(orm, dbs)
    case _ =>
  }

  def createModels(orm: ORM) = orm match {
    case Django(_, pdir, _) => {
      val models = Utils.runCmd("python3 manage.py inspectdb", Some(pdir))
      Utils.writeToFile(orm.getModelsPath(), models)
    }
    case _ => ()

  }

  def createDjangoProject(orm: ORM, dbs: List[DB]) = orm match {
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
      } + "}\n" + "INSTALLED_APPS = ['" + name + "']"
      Utils.appendToFile(orm.getSettingsPath(), dbsettings)
    }
    case _ => ()
  }
}
