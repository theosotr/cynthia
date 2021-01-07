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

  def getModelsInTopSort() = {
    val modelMap = models.foldLeft(Map[String, Set[String]]()) {
      case (acc, (k, v)) => {
        val acc2 = if (acc.contains(k)) acc else acc + (k -> Set[String]())
        (v.fields filter Field.isForeign).foldLeft(acc2) {
          case (acc, Field(_, Foreign(n))) => {
            acc get k match {
              case None    => acc + (k -> Set(n))
              case Some(e) => acc + (k -> (e + n))
            }
          }
        }
      }
    }
    Utils.topologicalSort(modelMap)  
  }
}
