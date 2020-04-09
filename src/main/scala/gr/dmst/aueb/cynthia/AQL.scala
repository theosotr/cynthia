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
      case None                        => false
      case Some(FieldDecl(f, _, _, _)) => f.isAggregate
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
case object Group extends Operation

sealed trait FieldType
case object StringF extends FieldType
case object IntF extends FieldType
case object DoubleF extends FieldType
case object BooleanF extends FieldType
case object DateTimeF extends FieldType

case class FieldDecl(f: FieldExpr, as: String, ftype: FieldType, hidden: Boolean = false)
object FieldDecl {

  def as(fieldDecl: FieldDecl) = fieldDecl match {
    case FieldDecl(_, as, _, _) => as
  }

  def hidden(fieldDecl: FieldDecl) = fieldDecl match {
    case FieldDecl(_, _, _, h) => h
  }

  def isAggregate(fieldDecl: FieldDecl) = fieldDecl match {
    case FieldDecl(f, _, _, _) => f.isAggregate
  }
}

sealed trait QuerySet {
  def hasSort(): Boolean
}
case class New(model: String, fields: Set[FieldDecl]) extends QuerySet {
  def hasSort() = false
}
case class Apply(op: Operation, q: QuerySet) extends QuerySet {
  def hasSort() = op match {
    case Sort(_) => true
    case _       => q.hasSort
  }
}
case class Union(q1: QuerySet, q2: QuerySet) extends QuerySet {
  def hasSort() = q1.hasSort && q2.hasSort
}
case class Intersect(q1: QuerySet, q2: QuerySet) extends QuerySet {
  def hasSort() = q1.hasSort && q2.hasSort
}

sealed abstract class Query(val queryset: QuerySet)
case class FirstRes(qs: QuerySet) extends Query(qs)
case class SubsetRes(offset: Int = 0, limit: Option[Int], qs: QuerySet) extends Query(qs)
case class SetRes(qs: QuerySet) extends Query(qs)
case class AggrRes(aggrs: Seq[FieldDecl], qs: QuerySet) extends Query(qs)
