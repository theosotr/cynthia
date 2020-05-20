package gr.dmst.aueb.cynthia


sealed trait DataType
case object Serial extends DataType
case object Boolean extends DataType
case object Int8 extends DataType
case object Int16 extends DataType
case object Int32 extends DataType
case object Int64 extends DataType
case object Double extends DataType
case class VarChar(n: Int) extends DataType


// TODO Maybe we should add field constraints.
case class Field(name: String, ftype: DataType)
case class Model(name: String, fields: Set[Field])
case class Schema(name: String, models: Set[Model])
