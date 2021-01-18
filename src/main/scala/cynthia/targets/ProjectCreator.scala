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

import cynthia.utils.Utils

object ProjectCreator {

  def createProject(orm: ORM, dbs: Seq[DB]) = dbs match {
    case Seq() => ()
    case _ => {
      setupProject(orm, dbs)
      createModels(orm, dbs)
    }
  }

  def setupProject(orm: ORM, dbs: Seq[DB]) = orm match {
    case Django(_, _, _)    => createDjangoProject(orm, dbs)
    case SQLAlchemy(_, _)   => createSQLAlchemyProject(orm, dbs)
    case Sequelize(_, _)    => createSequelizeProject(orm, dbs)
    case Peewee(_, _)       => createPeeweeProject(orm, dbs)
    case ActiveRecord(_, _) => createActiveRecordProject(orm, dbs)
    case Pony(_, _)         => createPonyProject(orm, dbs)
  }

  def createModels(orm: ORM, dbs: Seq[DB]) = orm match {
    case Django(_, pdir, _) => {
      val models = Utils.runCmd("python3 manage.py inspectdb", Some(pdir))
      val models2 = models.replaceAll(
        "id = models.AutoField\\(",
        "id = models.AutoField\\(primary_key=True,"
      )
      Utils.writeToFile(orm.getModelsPath(), models2)
    }
    case SQLAlchemy(_, pdir) => {
      // Use the first element of the db sequence.
      val models =
        Utils.runCmd("sqlacodegen --noinflect " + dbs(0).getURI(), None)
      Utils.writeToFile(orm.getModelsPath(), models)
    }
    case Peewee(_, pdir) => {
      val bcmd = "python -m pwiz"
      // Generate a models.py file for each backend.
      dbs filter {
        case MSSQL(_, _, _) => false
        case _              => true
      } foreach { db =>
        {
          val cmd = db match {
            case Postgres(user, password, dbname) =>
              s" -e postgres -u $user -H localhost $dbname -P $password"
            case MySQL(user, password, dbname) =>
              s" -e mysql -u $user -H localhost $dbname -P $password"
            case SQLite(dbname) =>
              s" -e sqlite $dbname"
            case Cockroachdb(user, _, dbname) =>
              s" -e cockroachdb -u $user -H localhost $dbname "
            case MSSQL(_, _, _) =>
              ??? // Unreachable
          }
          val models = Utils.runCmd(bcmd + cmd, None)
          Utils.writeToFile(
            orm.getModelsPath().replace(".py", "_" + db.getName() + ".py"),
            models
          )
        }
      }
    }
    case ActiveRecord(_, pdir) => {
      val bcmd = "rmre"
      val cmd = dbs(0) match {
        case Postgres(user, password, dbname) =>
          Seq(
            bcmd,
            "-a",
            "postgresql",
            "-d",
            dbname,
            "-u",
            user,
            "-p",
            password,
            "-o",
            orm.getModelsPath()
          ).mkString(" ")
        case MySQL(user, password, dbname) =>
          Seq(
            bcmd,
            "-a",
            "mysql2",
            "-d",
            dbname.toLowerCase,
            "-u",
            user,
            "-p",
            password,
            "-o",
            orm.getModelsPath()
          ).mkString(" ")
        case SQLite(dbname) =>
          Seq(
            bcmd,
            "-a",
            "postgresql",
            "-d",
            dbname.split("/").last.toLowerCase,
            "-u",
            "orm_testing",
            "-p",
            "orm_testing",
            "-o",
            orm.getModelsPath()
          ).mkString(" ")
        case Cockroachdb(user, password, dbname) =>
          Seq(
            bcmd,
            "-a",
            "postgresql",
            "-s",
            "localhost",
            "--port",
            "26257",
            "-d",
            dbname,
            "-u",
            user,
            "-o",
            orm.getModelsPath()
          ).mkString(" ")
        case MSSQL(user, _, dbname) =>
          Seq(
            bcmd,
            "-a",
            "postgresql",
            "-d",
            dbname,
            "-u",
            user,
            "-p",
            "orm_testing",
            "-o",
            orm.getModelsPath()
          ).mkString(" ")
      }
      Utils.runCmd(cmd, None)
    }
    case Sequelize(_, pdir) => {
      val db = dbs(0)
      val bcmd = "node_modules/sequelize-auto/bin/sequelize-auto"
      val cmd = db match {
        case Postgres(user, password, dbname) =>
          Seq(
            bcmd,
            "-h",
            "localhost",
            "-u",
            user,
            "-x",
            password,
            "-d",
            dbname,
            "--dialect",
            db.getName(),
            "-o",
            pdir
          ).mkString(" ")
        case MySQL(user, password, dbname) =>
          Seq(
            bcmd,
            "-h",
            "localhost",
            "-u",
            user,
            "-x",
            password,
            "-d",
            dbname,
            "--dialect",
            db.getName(),
            "-o",
            pdir
          ).mkString(" ")
        case SQLite(dbname) =>
          Seq(
            bcmd,
            "-h",
            "localhost",
            "-u",
            "foo",
            "-d",
            dbname,
            "--dialect",
            db.getName(),
            "-o",
            pdir
          ).mkString(" ")
        case Cockroachdb(user, password, dbname) =>
          Seq(
            bcmd,
            "-h",
            "localhost",
            "-p",
            "26257",
            "-u",
            user,
            "-d",
            dbname,
            "--dialect",
            db.getName(),
            "-o",
            pdir
          ).mkString(" ")
        case MSSQL(user, password, dbname) =>
          Seq(
            bcmd,
            "-h",
            "localhost",
            "-u",
            "foo",
            "-d",
            dbname,
            "--dialect",
            db.getName(),
            "-o",
            pdir
          ).mkString(" ")
      }
      Utils.runCmd(cmd, None)
    }
    case Pony(_, pdir) => {
      val bcmd = "python scripts/ponywiz.py"
      // Generate a models.py file for each backend.
      dbs filter {
        case MSSQL(_, _, _) => false
        case _              => true
      } foreach { db =>
        {
          val cmd = db match {
            case Postgres(user, password, dbname) =>
              s" -e postgres -u $user -H localhost $dbname -P $password"
            case MySQL(user, password, dbname) =>
              s" -e mysql -u $user -H localhost $dbname -P $password"
            case SQLite(dbname) =>
              s" -e sqlite $dbname"
            case Cockroachdb(user, _, dbname) =>
              s" -e cockroachdb -u $user -H localhost $dbname "
            case MSSQL(_, _, _) =>
              ??? // Unreachable
          }
          val models = Utils.runCmd(bcmd + cmd, None)
          Utils.writeToFile(
            orm.getModelsPath().replace(".py", "_" + db.getName() + ".py"),
            models
          )
        }
      }
    }
  }

  def createSequelizeProject(orm: ORM, dbs: Seq[DB]) = orm match {
    case Sequelize(_, pdir) => Utils.emptyFile(pdir)
    case _                  => ()
  }

  def createActiveRecordProject(orm: ORM, dbs: Seq[DB]) = orm match {
    case ActiveRecord(_, pdir) => Utils.emptyFile(pdir)
    case _                     => ()
  }

  def createPeeweeProject(orm: ORM, dbs: Seq[DB]) = orm match {
    case Peewee(_, pdir) => Utils.emptyFile(pdir)
    case _               => ()
  }

  def createSQLAlchemyProject(orm: ORM, dbs: Seq[DB]) = orm match {
    case SQLAlchemy(name, pdir) => {
      Utils.emptyFile(pdir)
      Utils.writeToFile(Utils.joinPaths(List(pdir, "__init__.py")), "")
    }
    case _ => ()
  }

  def createPonyProject(orm: ORM, dbs: Seq[DB]) = orm match {
    case Pony(_, pdir) => Utils.emptyFile(pdir)
    case _             => ()
  }

  def createDjangoProject(orm: ORM, dbs: Seq[DB]) = orm match {
    case Django(name, pdir, setDir) => {
      Utils.emptyFile(pdir)
      Utils.runCmd("django-admin startproject djangoproject " + pdir, None)
      Utils.runCmd("python3 manage.py startapp " + name, Some(pdir))
      val dbsettings = dbs.foldLeft("DATABASES = {\n") { (acc, db) =>
        db match {
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
          case MSSQL(user, password, dbname) =>
            acc + "'mssql': {" +
              Map(
                "'NAME'" -> ("'" + dbname + "'"),
                "'HOST'" -> "'localhost'",
                "'USER'" -> ("'" + user + "'"),
                "'PASSWORD'" -> ("'" + password + "'"),
                "'ENGINE'" -> "'sql_server.pyodbc'",
                "'OPTIONS'" -> "{'driver': 'ODBC Driver 17 for SQL Server',}"
              ).map(_.productIterator.mkString(":")).mkString(",\n") +
              "},"
          case Cockroachdb(user, password, dbname) =>
            acc + "'cockroachdb': {" +
              Map(
                "'NAME'" -> ("'" + dbname + "'"),
                "'HOST'" -> "'localhost'",
                "'USER'" -> ("'" + user + "'"),
                "'PASSWORD'" -> ("'" + password + "'"),
                "'PORT'" -> "26257",
                "'ENGINE'" -> "'django_cockroachdb'"
              ).map(_.productIterator.mkString(":")).mkString(",\n") +
              "},"
          case SQLite(dbname) =>
            acc + "'default': {" +
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
