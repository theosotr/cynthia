package gr.dmst.aueb.cynthia


sealed trait ORM {
  def getModelsPath(): String

  def getSettingsPath(): String

  def getDriverPath(db: DB): String

  def getName(): String
}

case class Django(name: String, pDir: String, setDir: String) extends ORM {
  Utils.createDir(pDir)

  override def getModelsPath() =
    Utils.joinPaths(List(pDir, name, "models.py"))

  override def getSettingsPath() =
    Utils.joinPaths(List(pDir, setDir, "settings.py"))

  override def getDriverPath(db: DB) =
    Utils.joinPaths(List(pDir, "driver_" + db.getName() + ".py"))

  override def getName() =
    "django"
}

case class SQLAlchemy(name: String, pDir: String) extends ORM {
  Utils.createDir(pDir)

  override def getModelsPath() =
    Utils.joinPaths(List(pDir, "models.py"))

  override def getSettingsPath() = ""

  override def getDriverPath(db: DB) =
    Utils.joinPaths(List(pDir, "driver_" + db.getName() + ".py"))

  override def getName() =
    "sqlalchemy"
}

case class Sequelize(name: String, pDir: String) extends ORM {
  Utils.createDir(pDir)

  override def getModelsPath() =
    Utils.joinPaths(List(pDir, "models"))

  override def getSettingsPath() = ""

  override def getDriverPath(db: DB) =
    Utils.joinPaths(List(pDir, "driver_" + db.getName() + ".js"))

  override def getName() =
    "sequelize"
}
