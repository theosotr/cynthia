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

package cynthia.lang

import spray.json._


object ModelJsonProtocol extends DefaultJsonProtocol { 
  implicit object DataTypeJsonFormat extends JsonFormat[DataType] {

    def write(dt: DataType) = dt match {
      case Serial => JsString("Serial") 
      case Int8 => JsString("Int8")
      case Int16 => JsString("Int16")
      case Int32 => JsString("Int32")
      case Int64 => JsString("Int64")
      case Bool => JsString("Bool")
      case Numeric => JsString("Numeric")
      case VarChar(n) => JsObject("n" -> JsNumber(n)) 
      case Foreign(to) => JsObject("to" -> JsString(to))
    }

    def read(json: JsValue) = json match {
      case JsString("Serial")   => Serial
      case JsString("Bool") => Bool
      case JsString("Int8") => Int8
      case JsString("Int16") => Int16
      case JsString("Int32") => Int32
      case JsString("Int64") => Int64
      case JsString("Numeric") => Numeric
      case JsObject(fields) => {
        fields.keys.toList match {
          case "n" :: Nil => VarChar(fields("n").convertTo[Int])
          case "to" :: Nil => Foreign(fields("to").convertTo[String]) 
          case _ => deserializationError("DataType expected")
        }
      }
      case _ => deserializationError("DataType expected")
    }
  }

  implicit object FieldJsonFormat extends JsonFormat[Field] {

    def write(f: Field) =
      JsObject("name" -> JsString(f.name), "ftype" -> f.ftype.toJson)

    def read(json: JsValue) = json match {
      case JsObject(fields) => {
        if (fields.contains("name") && fields.contains("ftype"))
          Field(fields("name").convertTo[String] ,
                fields("ftype").convertTo[DataType])
        else deserializationError("Field expected")
      }
      case _ => deserializationError("Model expected")
    }
  }

  implicit object ModelJsonFormat extends JsonFormat[Model] {

    def write(m: Model) =
      JsObject("name" -> JsString(m.name), "fields" -> m.fields.toJson)

    def read(json: JsValue) = json match {
      case JsObject(fields) => {
        if (fields.contains("name") && fields.contains("fields"))
          Model(fields("name").convertTo[String] ,
                fields("fields").convertTo[Seq[Field]])
        else deserializationError("Model expected")
      }
      case _ => deserializationError("Model expected")
    }
  }

  implicit object SchemaFormat extends JsonFormat[Schema] {

    def write(s: Schema) =
      JsObject("name" -> JsString(s.name), "models" -> s.models.toJson)

    def read(json: JsValue) = json match {
      case JsObject(fields) => {
        if (fields.contains("name") && fields.contains("models"))
          Schema(fields("name").convertTo[String] ,
                 fields("models").convertTo[Map[String, Model]])
        else deserializationError("Schema expected")
      }
      case _ => deserializationError("Schema expected")
    }
  }
}
