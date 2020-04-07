package gr.dmst.aueb.cynthia

sealed trait ConstantType
case object Quoted extends ConstantType
case object UnQuoted extends ConstantType

sealed abstract class FieldExpr(val compound: Boolean) {
  def isAggregate(): Boolean
}
case class Constant(v: String, vt: ConstantType) extends FieldExpr(false) {
  def isAggregate() = false
}
case class F(f: String) extends FieldExpr(false) {
  def isAggregate() = false
}
case class Count(f: Option[FieldExpr]) extends FieldExpr(false) {
  def isAggregate() = true
}
case class Sum(f: FieldExpr) extends FieldExpr(false) {
  def isAggregate() = true
}
case class Avg(f: FieldExpr) extends FieldExpr(false) {
  def isAggregate() = true
}
case class Max(f: FieldExpr) extends FieldExpr(false) {
  def isAggregate() = true
}
case class Min(f: FieldExpr) extends FieldExpr(false) {
  def isAggregate() = true
}
case class Add(f1: FieldExpr, f2: FieldExpr) extends FieldExpr(true) {
  def isAggregate() =
    f1.isAggregate || f2.isAggregate
}
case class Sub(f1: FieldExpr, f2: FieldExpr) extends FieldExpr(true) {
  def isAggregate() =
    f1.isAggregate || f2.isAggregate
}
case class Mul(f1: FieldExpr, f2: FieldExpr) extends FieldExpr(true) {
  def isAggregate() =
    f1.isAggregate || f2.isAggregate
}
case class Div(f1: FieldExpr, f2: FieldExpr) extends FieldExpr(true) {
  def isAggregate() =
    f1.isAggregate || f2.isAggregate
}

sealed abstract class Predicate {
  def isAggregateField(field: String, fields: Map[String, FieldDecl]) =
    fields.get(field) match {
      case None                     => false
      case Some(FieldDecl(f, _, _)) => f.isAggregate
    }

  def hasAggregate(fields: Map[String, FieldDecl]): Boolean
}
case class Eq(key: String, value: FieldExpr) extends Predicate {
  def hasAggregate(fields: Map[String, FieldDecl]) =
    value.isAggregate || isAggregateField(key, fields)
}
case class Gt(key: String, value: FieldExpr) extends Predicate {
  def hasAggregate(fields: Map[String, FieldDecl]) =
    value.isAggregate || isAggregateField(key, fields)
}
case class Lt(key: String, value: FieldExpr) extends Predicate {
  def hasAggregate(fields: Map[String, FieldDecl]) =
    value.isAggregate || isAggregateField(key, fields)
}
case class Gte(key: String, value: FieldExpr) extends Predicate {
  def hasAggregate(fields: Map[String, FieldDecl]) =
    value.isAggregate || isAggregateField(key, fields)
}
case class Lte(key: String, value: FieldExpr) extends Predicate {
  def hasAggregate(fields: Map[String, FieldDecl]) =
    value.isAggregate || isAggregateField(key, fields)
}
case class Contains(key: String, value: FieldExpr) extends Predicate {
  def hasAggregate(fields: Map[String, FieldDecl]) =
    value.isAggregate || isAggregateField(key, fields)
}
case class And(p1: Predicate, p2: Predicate) extends Predicate {
  def hasAggregate(fields: Map[String, FieldDecl]) =
    p1.hasAggregate(fields) || p2.hasAggregate(fields)
}
case class Or(p1: Predicate, p2: Predicate) extends Predicate {
  def hasAggregate(fields: Map[String, FieldDecl]) =
    p1.hasAggregate(fields) || p2.hasAggregate(fields)
}
case class Not(p: Predicate) extends Predicate {
  def hasAggregate(fields: Map[String, FieldDecl]) =
    p.hasAggregate(fields)
}

sealed trait Order
case object Asc extends Order
case object Desc extends Order

sealed trait Operation
case class Filter (pred: Predicate) extends Operation
case class Sort(spec: Seq[(String, Order)]) extends Operation
case class GroupBy(spec: Seq[String]) extends Operation

sealed trait FieldType
case object StringF extends FieldType
case object IntF extends FieldType
case object DoubleF extends FieldType
case object BooleanF extends FieldType
case object DateTimeF extends FieldType

case class FieldDecl(f: FieldExpr, as: String, ftype: FieldType)

sealed trait QuerySet
case class New(model: String, fields: Set[FieldDecl]) extends QuerySet
case class Apply(op: Operation, q: QuerySet) extends QuerySet
case class Union(q1: QuerySet, q2: QuerySet) extends QuerySet
case class Intersect(q1: QuerySet, q2: QuerySet) extends QuerySet

sealed trait Query
case class FirstRes(qs: QuerySet) extends Query
case class SubsetRes(offset: Int = 0, limit: Option[Int], qs: QuerySet) extends Query
case class SetRes (qs: QuerySet) extends Query
case class AggrRes (aggrs: Seq[FieldDecl], qs: QuerySet) extends Query
