package gr.dmst.aueb.cynthia


sealed abstract class ORM(
  val ormName: String,
  val projectName: String,
  val projectDir: String) {

  def getModelsPath(): String

  def getSettingsPath(): String

  def getDriverPath(db: DB): String
}

case class Django(name: String, pDir: String, setDir: String)
extends ORM("django", name, pDir) {
  Utils.createDir(pDir)

  override def getModelsPath() =
    Utils.joinPaths(List(pDir, name, "models.py"))

  override def getSettingsPath() =
    Utils.joinPaths(List(pDir, setDir, "settings.py"))

  override def getDriverPath(db: DB) =
    Utils.joinPaths(List(pDir, "driver_" + db.getName() + ".py"))
}

case class SQLAlchemy(name: String, pDir: String)
extends ORM("sqlalchemy", name, pDir) {
  Utils.createDir(pDir)

  override def getModelsPath() =
    Utils.joinPaths(List(pDir, "models.py"))

  override def getSettingsPath() = ""

  override def getDriverPath(db: DB) =
    Utils.joinPaths(List(pDir, "driver_" + db.getName() + ".py"))
}

case class Sequelize(name: String, pDir: String)
extends ORM("sequelize", name, pDir) {
  Utils.createDir(pDir)

  override def getModelsPath() =
    Utils.joinPaths(List(pDir, "models"))

  override def getSettingsPath() = ""

  override def getDriverPath(db: DB) =
    Utils.joinPaths(List(pDir, "driver_" + db.getName() + ".js"))
}

case class Peewee(name: String, pDir: String)
extends ORM("peewee", name, pDir) {
  Utils.createDir(pDir)

  override def getModelsPath() =
    Utils.joinPaths(List(pDir, "models.py"))

  override def getSettingsPath() = ""

  override def getDriverPath(db: DB) =
    Utils.joinPaths(List(pDir, "driver_" + db.getName() + ".py"))
}

case class ActiveRecord(name: String, pDir: String)
extends ORM("activerecord", name, pDir) {
  Utils.createDir(pDir)

  override def getModelsPath() =
    Utils.joinPaths(List(pDir, "models"))

  override def getSettingsPath() = ""

  override def getDriverPath(db: DB) =
    Utils.joinPaths(List(pDir, "driver_" + db.getName() + ".rb"))
}
