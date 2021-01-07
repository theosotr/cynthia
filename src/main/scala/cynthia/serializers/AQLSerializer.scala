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

package cynthia.serializers

import spray.json._
import DefaultJsonProtocol._

import cynthia._


object AQLJsonProtocol extends DefaultJsonProtocol {
  implicit object ConstantTypeJsonFormat extends JsonFormat[ConstantType] {
    def write(c: ConstantType) = c match {
      case Quoted   => JsString("Quoted")
      case UnQuoted => JsString("UnQuoted")
    }
    def read(json: JsValue) = json match {
      case JsString("Quoted")   => Quoted
      case JsString("UnQuoted") => UnQuoted
      case _ => deserializationError("ConstantType expected")
    }
  }

  implicit object OrderTypeJsonFormat extends JsonFormat[Order] {
    def write(o: Order) = o match {
      case Asc  => JsString("Asc")
      case Desc => JsString("Desc")
    }
    def read(json: JsValue) = json match {
      case JsString("Asc")  => Asc
      case JsString("Desc") => Desc
      case _ => deserializationError("Order expected")
    }
  }

  implicit object FieldTypeTypeJsonFormat extends JsonFormat[FieldType] {
    def write(ft: FieldType) = ft match {
      case StringF   => JsString("StringF")
      case IntF      => JsString("IntF")
      case DoubleF   => JsString("DoubleF")
      case BooleanF  => JsString("BooleanF")
      case DateTimeF => JsString("DateTimeF")
    }
    def read(json: JsValue) = json match {
      case JsString("StringF")   => StringF
      case JsString("IntF")      => IntF
      case JsString("DoubleF")   => DoubleF
      case JsString("BooleanF")  => BooleanF
      case JsString("DateTimeF") => DateTimeF
      case _ => deserializationError("FieldType expected")
    }
  }

  implicit object FieldExprJsonFormat extends RootJsonFormat[FieldExpr] {
    def write(fexpr: FieldExpr) = {
      val value = fexpr match {
        case F(f)             => JsObject("F" -> JsObject("f" -> JsString(f)))
        case Constant(v, vt)  => JsObject("Constant" -> JsObject(
          "v" -> JsString(v),
          "vt" -> vt.toJson
        ))
        case Count(None)    => JsObject("Count" -> JsObject())
        case Count(Some(f)) => JsObject("Count" -> JsObject("f" -> f.toJson))
        case Sum(f)         => JsObject("Sum"   -> JsObject("f" -> f.toJson))
        case Avg(f)         => JsObject("Avg"   -> JsObject("f" -> f.toJson))
        case Min(f)         => JsObject("Min"   -> JsObject("f" -> f.toJson))
        case Max(f)         => JsObject("Max"   -> JsObject("f" -> f.toJson))
        case Add(f1, f2)    => JsObject("Add"   -> JsObject("f1" -> f1.toJson, "f2" -> f2.toJson))
        case Sub(f1, f2)    => JsObject("Sub"   -> JsObject("f1" -> f1.toJson, "f2" -> f2.toJson))
        case Mul(f1, f2)    => JsObject("Mul"   -> JsObject("f1" -> f1.toJson, "f2" -> f2.toJson))
        case Div(f1, f2)    => JsObject("Div"   -> JsObject("f1" -> f1.toJson, "f2" -> f2.toJson))
      }
      JsObject("FieldExpr" -> value)
    }
    def read(json: JsValue) = {
      json match {
        case JsObject(fields) =>
          fields("FieldExpr") match {
            case JsObject(fields) =>
              fields.keys.toList.head match {
                case "F" =>
                  fields("F") match {
                    case JsObject(f) =>
                      F(f("f").convertTo[String])
                    case _ => deserializationError("F expected")
                  }
                case "Constant" =>
                  fields("Constant") match {
                    case JsObject(constant) =>
                      Constant(constant("v").convertTo[String], constant("vt").convertTo[ConstantType])
                    case _ => deserializationError("Constant expected")
                  }
                case "Count" =>
                  fields("Count") match {
                    case JsObject(count) =>
                      if (count.contains("f"))
                        Count(Some(count("f").convertTo[FieldExpr]))
                      else
                        Count(None)
                    case _ => deserializationError("Count expected")
                  }
                case "Sum" =>
                  fields("Sum") match {
                    case JsObject(sum) =>
                        Sum(sum("f").convertTo[FieldExpr])
                    case _ => deserializationError("Sum expected")
                  }
                case "Avg" =>
                  fields("Avg") match {
                    case JsObject(avg) =>
                        Avg(avg("f").convertTo[FieldExpr])
                    case _ => deserializationError("Avg expected")
                  }
                case "Min" =>
                  fields("Min") match {
                    case JsObject(min) =>
                        Min(min("f").convertTo[FieldExpr])
                    case _ => deserializationError("Min expected")
                  }
                case "Max" =>
                  fields("Max") match {
                    case JsObject(max) =>
                        Max(max("f").convertTo[FieldExpr])
                    case _ => deserializationError("Max expected")
                  }
                case "Add" =>
                  fields("Add") match {
                    case JsObject(add) =>
                        Add(add("f1").convertTo[FieldExpr], add("f2").convertTo[FieldExpr])
                    case _ => deserializationError("Add expected")
                  }
                case "Sub" =>
                  fields("Sub") match {
                    case JsObject(sub) =>
                        Sub(sub("f1").convertTo[FieldExpr], sub("f2").convertTo[FieldExpr])
                    case _ => deserializationError("Sub expected")
                  }
                case "Mul" =>
                  fields("Mul") match {
                    case JsObject(mul) =>
                        Mul(mul("f1").convertTo[FieldExpr], mul("f2").convertTo[FieldExpr])
                    case _ => deserializationError("Mul expected")
                  }
                case "Div" =>
                  fields("Div") match {
                    case JsObject(div) =>
                        Div(div("f1").convertTo[FieldExpr], div("f2").convertTo[FieldExpr])
                    case _ => deserializationError("Div expected")
                  }
                case _ => deserializationError("FieldExpr expected")
              }
            case _ => deserializationError("FieldExpr expected")
          }
        case _ => deserializationError("FieldExpr expected")
      }
    }
  }

  implicit object PredicateJsonFormat extends RootJsonFormat[Predicate] {
    def write(predicate: Predicate) = {
      val value = predicate match {
        case Eq(k, f)         => JsObject("Eq"         -> JsObject("k" -> JsString(k), "f" -> f.toJson))
        case Gt(k, f)         => JsObject("Gt"         -> JsObject("k" -> JsString(k), "f" -> f.toJson))
        case Lt(k, f)         => JsObject("Lt"         -> JsObject("k" -> JsString(k), "f" -> f.toJson))
        case Gte(k, f)        => JsObject("Gte"        -> JsObject("k" -> JsString(k), "f" -> f.toJson))
        case Lte(k, f)        => JsObject("Lte"        -> JsObject("k" -> JsString(k), "f" -> f.toJson))
        case Contains(k, f)   => JsObject("Contains"   -> JsObject("k" -> JsString(k), "f" -> f.toJson))
        case StartsWith(k, f) => JsObject("StartsWith" -> JsObject("k" -> JsString(k), "f" -> f.toJson))
        case EndsWith(k, f)   => JsObject("EndsWith"   -> JsObject("k" -> JsString(k), "f" -> f.toJson))
        case And(p1, p2)      => JsObject("And"        -> JsObject("p1" -> p1.toJson, "p2" -> p2.toJson))
        case Or(p1, p2)       => JsObject("Or"         -> JsObject("p1" -> p1.toJson, "p2" -> p2.toJson))
        case Not(p)           => JsObject("Not"        -> JsObject("p" -> p.toJson))
      }
      JsObject("Predicate" -> value)
    }
    def read(json: JsValue) = {
      json match {
        case JsObject(fields) =>
          fields("Predicate") match {
            case JsObject(fields) =>
              fields.keys.toList.head match {
                case "Eq" =>
                  fields("Eq") match {
                    case JsObject(values) =>
                      Eq(values("k").convertTo[String], values("f").convertTo[FieldExpr])
                    case _ => deserializationError("Eq expected")
                  }
                case "Gt" =>
                  fields("Gt") match {
                    case JsObject(values) =>
                      Gt(values("k").convertTo[String], values("f").convertTo[FieldExpr])
                    case _ => deserializationError("Gt expected")
                  }
                case "Lt" =>
                  fields("Lt") match {
                    case JsObject(values) =>
                      Lt(values("k").convertTo[String], values("f").convertTo[FieldExpr])
                    case _ => deserializationError("Lt expected")
                  }
                case "Gte" =>
                  fields("Gte") match {
                    case JsObject(values) =>
                      Gte(values("k").convertTo[String], values("f").convertTo[FieldExpr])
                    case _ => deserializationError("Gte expected")
                  }
                case "Lte" =>
                  fields("Lte") match {
                    case JsObject(values) =>
                      Lte(values("k").convertTo[String], values("f").convertTo[FieldExpr])
                    case _ => deserializationError("Lte expected")
                  }
                case "Contains" =>
                  fields("Contains") match {
                    case JsObject(values) =>
                      Contains(values("k").convertTo[String], values("f").convertTo[String])
                    case _ => deserializationError("Contains expected")
                  }
                case "StartsWith" =>
                  fields("StartsWith") match {
                    case JsObject(values) =>
                      StartsWith(values("k").convertTo[String], values("f").convertTo[String])
                    case _ => deserializationError("StartsWith expected")
                  }
                case "EndsWith" =>
                  fields("EndsWith") match {
                    case JsObject(values) =>
                      EndsWith(values("k").convertTo[String], values("f").convertTo[String])
                    case _ => deserializationError("EndsWith expected")
                  }
                case "And" =>
                  fields("And") match {
                    case JsObject(values) =>
                      And(values("p1").convertTo[Predicate], values("p2").convertTo[Predicate])
                    case _ => deserializationError("And expected")
                  }
                case "Or" =>
                  fields("Or") match {
                    case JsObject(values) =>
                      Or(values("p1").convertTo[Predicate], values("p2").convertTo[Predicate])
                    case _ => deserializationError("Or expected")
                  }
                case "Not" =>
                  fields("Not") match {
                    case JsObject(values) =>
                      Not(values("p").convertTo[Predicate])
                    case _ => deserializationError("Not expected")
                  }
                case _ => deserializationError("Predicate expected")
              }
            case _ => deserializationError("Predicate expected")
          }
        case _ => deserializationError("Predicate expected")
      }
    }
  }

  implicit object OperationTypeJsonFormat extends JsonFormat[Operation] {
    def write(o: Operation) = {
      val value = o match {
          case Filter(p)   => JsObject("Filter"   -> JsObject("p" -> p.toJson))
          case Distinct(f) => JsObject("Distinct" -> JsObject("f" -> f.toJson))
          case Sort(s)     => JsObject("Sort"     -> JsObject("s" -> s.toJson))
        }
      JsObject("Operation" -> value)
    }
    def read(json: JsValue) =
      json match {
        case JsObject(fields) =>
          fields("Operation") match {
            case JsObject(fields) =>
              fields.keys.toList.head match {
                case "Filter" =>
                  fields("Filter") match {
                    case JsObject(values) =>
                      Filter(values("p").convertTo[Predicate])
                    case _ => deserializationError("Filter expected")
                  }
                case "Distinct" =>
                  fields("Distinct") match {
                    case JsObject(values) =>
                      Distinct(values("f").convertTo[Option[String]])
                    case _ => deserializationError("Distinct expected")
                  }
                case "Sort" =>
                  fields("Sort") match {
                    case JsObject(values) =>
                      Sort(values("s").convertTo[Seq[(String, Order)]])
                    case _ => deserializationError("Sort expected")
                  }
                case _ => deserializationError("Operation expected")
              }
            case _ => deserializationError("Operation expected")
          }
        case _ => deserializationError("Operation expected")
      }
  }

  implicit object FieldDeclTypeJsonFormat extends JsonFormat[FieldDecl] {
    def write(fd: FieldDecl) = fd match {
      case FieldDecl(f, as, ftype, hidden) => JsObject("FieldDecl" -> JsObject(
        "f" -> f.toJson,
        "as" -> as.toJson,
        "ftype" -> ftype.toJson,
        "hidden" -> hidden.toJson))
    }
    def read(json: JsValue) =
      json match {
        case JsObject(fields) =>
          fields("FieldDecl") match {
            case JsObject(fields) =>
              FieldDecl(
                fields("f").convertTo[FieldExpr],
                fields("as").convertTo[String],
                fields("ftype").convertTo[FieldType],
                fields("hidden").convertTo[Boolean],
              )
            case _ => deserializationError("FieldDecl expected")
          }
        case _ => deserializationError("FieldDecl expected")
      }
  }

  implicit object QuerySetTypeJsonFormat extends JsonFormat[QuerySet] {
    def write(qs: QuerySet) = {
      val value = qs match {
        case New(m, f)         => JsObject("New" -> JsObject("m" -> m.toJson, "f" -> f.toJson))
        case Apply(op, q)      => JsObject("Apply" -> JsObject("op" -> op.toJson, "q" -> q.toJson))
        case Union(q1, q2)     => JsObject("Union" -> JsObject("q1" -> q1.toJson, "q2" -> q2.toJson))
        case Intersect(q1, q2) => JsObject("Intersect" -> JsObject("q1" -> q1.toJson, "q2" -> q2.toJson))
      }
      JsObject("QuerySet" -> value)
    }
    def read(json: JsValue) =
      json match {
        case JsObject(fields) =>
          fields("QuerySet") match {
            case JsObject(fields) =>
              fields.keys.toList.head match {
                case "New" =>
                  fields("New") match {
                    case JsObject(values) =>
                      New(values("m").convertTo[String], values("f").convertTo[Seq[FieldDecl]])
                    case _ => deserializationError("New expected")
                  }
                case "Apply" =>
                  fields("Apply") match {
                    case JsObject(values) =>
                      Apply(values("op").convertTo[Operation], values("q").convertTo[QuerySet])
                    case _ => deserializationError("Apply expected")
                  }
                case "Union" =>
                  fields("Union") match {
                    case JsObject(values) =>
                      Union(values("q1").convertTo[QuerySet], values("q2").convertTo[QuerySet])
                    case _ => deserializationError("Union expected")
                  }
                case "Intersect" =>
                  fields("Intersect") match {
                    case JsObject(values) =>
                      Intersect(values("q1").convertTo[QuerySet], values("q2").convertTo[QuerySet])
                    case _ => deserializationError("Intersect expected")
                  }
                case _ => deserializationError("QuerySet expected")
              }
            case _ => deserializationError("QuerySet expected")
          }
        case _ => deserializationError("QuerySet expected")
      }
  }

  implicit object QueryTypeJsonFormat extends JsonFormat[Query] {
    def write(q: Query) = {
      val value = q match {
        case FirstRes(qs)                       => JsObject("FirstRes" -> JsObject("qs" -> qs.toJson))
        case SubsetRes(offset, None, qs)        => JsObject("SubsetRes" -> JsObject("offset" -> offset.toJson, "qs" -> qs.toJson))
        case SubsetRes(offset, Some(limit), qs) => JsObject("SubsetRes" -> JsObject("offset" -> offset.toJson, "limit" -> limit.toJson, "qs" -> qs.toJson))
        case SetRes(qs)                         => JsObject("SetRes" -> JsObject("qs" -> qs.toJson))
        case AggrRes(aggrs, qs)                 => JsObject("AggrRes" -> JsObject("aggrs" -> aggrs.toJson, "qs" -> qs.toJson))
      }
      JsObject("Query" -> value)
    }
    def read(json: JsValue) =
      json match {
        case JsObject(fields) =>
          fields("Query") match {
            case JsObject(fields) =>
              fields.keys.toList.head match {
                case "FirstRes" =>
                  fields("FirstRes") match {
                    case JsObject(values) =>
                      FirstRes(values("qs").convertTo[QuerySet])
                    case _ => deserializationError("FirstRes expected")
                  }
                case "SubsetRes" =>
                  fields("SubsetRes") match {
                    case JsObject(values) =>
                      if (values.contains("limit"))
                        SubsetRes(values("offset").convertTo[Int], Some(values("limit").convertTo[Int]), values("qs").convertTo[QuerySet])
                      else
                        SubsetRes(values("offset").convertTo[Int], None, values("qs").convertTo[QuerySet])
                    case _ => deserializationError("SubsetRes expected")
                  }
                case "SetRes" =>
                  fields("SetRes") match {
                    case JsObject(values) =>
                      SetRes(values("qs").convertTo[QuerySet])
                    case _ => deserializationError("SetRes expected")
                  }
                case "AggrRes" =>
                  fields("AggrRes") match {
                    case JsObject(values) =>
                      AggrRes(values("aggrs").convertTo[Seq[FieldDecl]], values("qs").convertTo[QuerySet])
                    case _ => deserializationError("FirstRes expected")
                  }
                case _ => deserializationError("Query expected")
              }
            case _ => deserializationError("Query expected")
          }
        case _ => deserializationError("Query expected")
      }
  }
}
