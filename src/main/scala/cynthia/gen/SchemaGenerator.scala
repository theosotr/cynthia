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

package cynthia.gen

import cynthia.lang._
import cynthia.utils.RUtils


object SchemaGenerator {
  val types = Seq(
    "int",
    "numeric",
    "string",
    "foreign"
  )

  private val usedTables: scala.collection.mutable.Set[String] =
    scala.collection.mutable.Set()

  def generateFields(schema: Schema) = {
    def _genFields(fields: Seq[Field], foreignF: Set[String],
        lowerBound: Int = 5): Seq[Field] =
      if (fields.size > lowerBound && RUtils.bool()) fields
      else {
        val res =
          RUtils.chooseFrom(
            if (schema.getModels().size > 0) types
            else types dropRight 1
          ) match {
            case "int"     => Some(RUtils.word(special = false).toLowerCase, Int32, foreignF)
            case "numeric" => Some(RUtils.word(special = false).toLowerCase, Numeric, foreignF)
            case "string"  => Some(RUtils.word(special = false).toLowerCase, VarChar(50), foreignF)
            case "foreign" => {
              val model = RUtils.chooseFrom(schema.getModels())
              if (foreignF.contains(model) || usedTables.contains(model)) None
              else {
                usedTables.add(model)
                Some(model.toLowerCase, Foreign(model), foreignF + model)
              }
            }
          }
        res match {
          case None            => _genFields(fields, foreignF)
          case Some((n, t, f)) => _genFields(fields :+ Field(n, t), f)
        }
      }
    _genFields(Seq(Field("id", Serial), Field("simvolosira", VarChar(50))), Set()) // Every model has an ID and a string field.
  }

  def generateModels(schema: Schema, lowerBound: Int = 5): Schema =
    if (schema.getModels().size > lowerBound && RUtils.bool()) schema
    else
      generateModels(schema + Model(
        RUtils.word(special = false).toLowerCase.capitalize,
        generateFields(schema)))

  def apply(models: Option[Int] = None): Schema =
    generateModels(Schema(RUtils.word(special = false).capitalize, Map()))
}
