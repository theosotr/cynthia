package gr.dmst.aueb.cynthia


sealed trait ORM {
  def getModelsPath(): String

  def getSettingsPath(): String

  def getDriverPath(): String
}

case class Django(name: String, pDir: String, setDir: String) extends ORM {
  Utils.createDir(pDir)

  def getModelsPath() =
    Utils.joinPaths(List(pDir, name, "models.py"))

  def getSettingsPath() =
    Utils.joinPaths(List(pDir, setDir, "settings.py"))

  def getDriverPath() =
    Utils.joinPaths(List(pDir, "driver.py"))
}

case class SQLAlchemy(name: String, pDir: String) extends ORM {
  Utils.createDir(pDir)

  def getModelsPath() =
    Utils.joinPaths(List(pDir, "models.py"))

  def getSettingsPath() = ""

  def getDriverPath() =
    Utils.joinPaths(List(pDir, "driver.py"))
}
