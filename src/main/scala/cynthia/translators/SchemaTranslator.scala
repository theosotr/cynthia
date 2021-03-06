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

package cynthia.translators

import cynthia.lang._
import cynthia.utils.{Utils, Str}

object SchemaTranslator {

  def convertDataType(t: DataType) = t match {
    case Serial | Int8 | Int16 | Int32 | Int64 | Foreign(_) => "integer"
    case Bool                                               => "boolean"
    case Numeric                                            => "numeric(10, 2)"
    case VarChar(n)                                         => "varchar(" + n + ")"
  }

  def translateModel(m: Model): QueryStr = {
    def getColumns() =
      m.fields.foldLeft(Str("")) { (acc, f) =>
        f.ftype match {
          case Foreign(_) =>
            acc << "\"" << f.name << "_id\" " << convertDataType(
              f.ftype
            ) << ",\n"
          case _ =>
            acc << Utils.quoteStr(f.name, quotes = "\"") << " " <<
              convertDataType(f.ftype) << ",\n"
        }
      }

    def getForeignKeys() =
      (m.fields filter Field.isForeign).foldLeft(Str("")) { (acc, x) =>
        acc << ",\nFOREIGN KEY (\"" << x.name << "_id\") REFERENCES " <<
          Utils.quoteStr(x.name, quotes = "\"") << "(id) ON DELETE NO ACTION"
      }

    QueryStr(
      None,
      Some(
        (Str("CREATE TABLE ") << Utils
          .quoteStr(m.name.toLowerCase(), quotes = "\"") <<
          " (\n" << getColumns() <<
          "PRIMARY KEY (id)" << getForeignKeys() << "\n);\n").!
      )
    )
  }

  def apply(schema: Schema): String = {
    // First we create a map that holds the dependencies among models
    val modelMap = schema.models.foldLeft(Map[String, Set[String]]()) {
      case (acc, (k, v)) => {
        val acc2 = if (acc.contains(k)) acc else acc + (k -> Set[String]())
        (v.fields filter Field.isForeign).foldLeft(acc2) {
          case (acc, Field(_, Foreign(n))) => {
            acc get k match {
              case None    => acc + (k -> Set(n))
              case Some(e) => acc + (k -> (e + n))
            }
          }
          case _ => ??? // Unreachable case
        }
      }
    }
    val topSort = Utils.topologicalSort(modelMap)
    // Create drop statements in reverse topological order
    val qstr = topSort.reverse.foldLeft(QueryStr()) {
      case (acc, m) => {
        acc >> QueryStr(
          None,
          Some(
            "DROP TABLE IF EXISTS " + Utils.quoteStr(
              m.toLowerCase,
              quotes = "\""
            ) + ";"
          )
        )
      }
    }
    // Traverse models in topological order and create the corresponding
    // CREATE TABLE statements.
    topSort
      .foldLeft(qstr) { case (acc, m) =>
        acc >> translateModel(schema.models(m))
      }
      .toString
  }
}
