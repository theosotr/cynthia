package gr.dmst.aueb.cynthia


sealed trait DataType {
  def isNumeric(): Boolean
  def isStr(): Boolean
}
case object Serial extends DataType {
  override def isNumeric() = true
  override def isStr() = false
}
case object Bool extends DataType {
  override def isNumeric() = true
  override def isStr() = false
}
case object Int8 extends DataType {
  override def isNumeric() = true
  override def isStr() = false
}
case object Int16 extends DataType {
  override def isNumeric() = true
  override def isStr() = false
}
case object Int32 extends DataType {
  override def isNumeric() = true
  override def isStr() = false
}
case object Int64 extends DataType {
  override def isNumeric() = true
  override def isStr() = false
}
case object Numeric extends DataType {
  override def isNumeric() = true
  override def isStr() = false
}
case class VarChar(n: Int) extends DataType {
  override def isNumeric() = false
  override def isStr() = true
}
case class Foreign(to: String) extends DataType {
  override def isNumeric() = false
  override def isStr() = false
}


// TODO Maybe we should add field constraints.
case class Field(name: String, ftype: DataType)
case object Field {
  def name(f: Field) = f match {
    case Field(n, _) => n
  }

  def dbname(f: Field) = f match {
    case Field(n, Foreign(_)) => n + "_id"
    case Field(n, _)          => n
  }

  def isForeign(f: Field) = f match {
    case Field(_, Foreign(_)) => true
    case _                    => false
  }
}
case class Model(name: String, fields: Seq[Field])
case class Schema(name: String, models: Map[String, Model]) {

  // Adds a new model to the existing schema.
  def +(m: Model): Schema =
    Schema(name, models + (m.name -> m))

  def getModels() =
    models.keys.toSeq

  def getSchemaPath() =
    Utils.joinPaths(List(Utils.getSchemaDir(), name))
}
