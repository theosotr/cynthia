package gr.dmst.aueb.cynthia


sealed trait DataType
case object Serial extends DataType
case object Bool extends DataType
case object Int8 extends DataType
case object Int16 extends DataType
case object Int32 extends DataType
case object Int64 extends DataType
case object Numeric extends DataType
case class VarChar(n: Int) extends DataType
case class Foreign(to: String) extends DataType


// TODO Maybe we should add field constraints.
case class Field(name: String, ftype: DataType)
case object Field {
  def name(f: Field) = f match {
    case Field(n, _) => n
  }
}
case class Model(name: String, fields: Seq[Field])
case class Schema(name: String, models: Map[String, Model])
