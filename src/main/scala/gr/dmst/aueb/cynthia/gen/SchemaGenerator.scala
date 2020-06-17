package gr.dmst.aueb.cynthia.gen

import gr.dmst.aueb.cynthia._


object SchemaGenerator {
  val types = Seq(
    "int",
    "numeric",
    "string",
    "foreign"
  )

  def generateFields(schema: Schema) = {
    def _genFields(fields: Seq[Field], foreignF: Set[String],
        lowerBound: Int = 5): Seq[Field] =
      if (fields.size > lowerBound && RUtils.bool()) fields
      else {
        val res =
          RUtils.chooseFrom(
            if (schema.getModels.size > 0) types
            else types dropRight 1
          ) match {
            case "int"     => Some(RUtils.word.toLowerCase, Int32, foreignF)
            case "numeric" => Some(RUtils.word.toLowerCase, Numeric, foreignF)
            case "string"  => Some(RUtils.word.toLowerCase, VarChar(50), foreignF)
            case "foreign" => {
              val model = RUtils.chooseFrom(schema.getModels)
              if (foreignF.contains(model)) None
              else Some(model.toLowerCase, Foreign(model), foreignF + model)
            }
          }
        res match {
          case None            => _genFields(fields, foreignF)
          case Some((n, t, f)) => _genFields(fields :+ Field(n, t), f)
        }
      }
    _genFields(Seq(Field("id", Serial)), Set()) // Every model has an ID.
  }

  def generateModels(schema: Schema, lowerBound: Int = 5): Schema =
    if (schema.getModels.size > lowerBound && RUtils.bool()) schema
    else
      generateModels(schema + Model(RUtils.word.capitalize, generateFields(schema)))

  def apply(models: Option[Int] = None): Schema =
    generateModels(Schema(RUtils.word.capitalize, Map()))
}
