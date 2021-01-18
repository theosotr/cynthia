import org.scalatest.funsuite.AnyFunSuite

import cynthia.lang._


class AQLTest extends AnyFunSuite {
  test("check a field expression is an aggregate") {
    var expr: FieldExpr = Add(
      Mul(Div(F("foo"), F("bar")), F("baz")),
      Sub(F("f"), Constant("1", UnQuoted)))
    assert(!expr.isAggregate())
    
    expr = Count(None)
    assert(expr.isAggregate())

    expr = Sum(F("col"))
    assert(expr.isAggregate())

    expr = Avg(F("col"))
    assert(expr.isAggregate())

    expr = Max(F("col"))
    assert(expr.isAggregate())

    expr = Min(F("col"))
    assert(expr.isAggregate())

    expr = Add(F("foo"), Sub(F("f"), Max(F("col"))))
    assert(expr.isAggregate())
  }

  test("check a field expression is a naive aggregate") {
    var expr: FieldExpr = Add(
      Mul(Div(F("foo"), F("bar")), F("baz")),
      Sub(F("f"), Constant("1", UnQuoted)))
    assert(!expr.isNaiveAggregate())
    
    expr = Count(None)
    assert(expr.isNaiveAggregate())

    expr = Sum(F("col"))
    assert(expr.isNaiveAggregate())

    expr = Avg(F("col"))
    assert(expr.isNaiveAggregate())

    expr = Max(F("col"))
    assert(expr.isNaiveAggregate())

    expr = Min(F("col"))
    assert(expr.isNaiveAggregate())

    expr = Add(F("foo"), Sub(F("f"), Max(F("col"))))
    assert(!expr.isNaiveAggregate())
  }

  test("check a field expresion is a constant") {
    var expr: FieldExpr = Constant("1", Quoted)
    assert(expr.isConstant())

    expr = F("foo")
    assert(!expr.isConstant())
    
    expr = Count(Some(Constant("foo", Quoted)))
    assert(!expr.isConstant())

    expr = Sum(Constant("foo", Quoted))
    assert(!expr.isConstant())

    expr = Avg(Constant("foo", Quoted))
    assert(!expr.isConstant())

    expr = Max(Constant("foo", Quoted))
    assert(!expr.isConstant())

    expr = Min(Constant("foo", Quoted))
    assert(!expr.isConstant())

    expr = Add(F("foo"), Constant("foo", Quoted))
    assert(!expr.isConstant())
    expr = Add(Constant("foo", Quoted), F("foo"))
    assert(!expr.isConstant())
    expr = Add(Constant("foo", Quoted), Constant("foo", Quoted))
    assert(expr.isConstant())

    expr = Sub(F("foo"), Constant("foo", Quoted))
    assert(!expr.isConstant())
    expr = Sub(Constant("foo", Quoted), F("foo"))
    assert(!expr.isConstant())
    expr = Sub(Constant("foo", Quoted), Constant("foo", Quoted))
    assert(expr.isConstant())

    expr = Mul(F("foo"), Constant("foo", Quoted))
    assert(!expr.isConstant())
    expr = Mul(Constant("foo", Quoted), F("foo"))
    assert(!expr.isConstant())
    expr = Mul(Constant("foo", Quoted), Constant("foo", Quoted))
    assert(expr.isConstant())

    expr = Div(F("foo"), Constant("foo", Quoted))
    assert(!expr.isConstant())
    expr = Div(Constant("foo", Quoted), F("foo"))
    assert(!expr.isConstant())
    expr = Div(Constant("foo", Quoted), Constant("foo", Quoted))
    assert(expr.isConstant())
  }

  test("check predicate has aggregate") {
    val f1 = FieldDecl(
      Add(F("foo"), Div(F("foo"), Constant("1", UnQuoted))), "f1", IntF)
    val f2 = FieldDecl(
      Add(Sum(F("col")), Constant("2", UnQuoted)), "f1", IntF)
    val fields = Map("f1" -> f1, "f2" -> f2)

    var pred: Predicate = Eq("f1", Constant("v", Quoted))
    assert(!pred.hasAggregate(fields))
    pred = Eq("f1", Max(F("f1")))
    assert(pred.hasAggregate(fields))

    pred = Eq("f2", Constant("v", Quoted))
    assert(pred.hasAggregate(fields))
    pred = Eq("f2", Max(F("f1")))
    assert(pred.hasAggregate(fields))

    pred = Gt("f1", Constant("v", Quoted))
    assert(!pred.hasAggregate(fields))
    pred = Gt("f1", Max(F("f1")))
    assert(pred.hasAggregate(fields))

    pred = Gt("f2", Constant("v", Quoted))
    assert(pred.hasAggregate(fields))
    pred = Gt("f2", Max(F("f1")))
    assert(pred.hasAggregate(fields))

    pred = Gte("f1", Constant("v", Quoted))
    assert(!pred.hasAggregate(fields))
    pred = Gte("f1", Max(F("f1")))
    assert(pred.hasAggregate(fields))

    pred = Gte("f2", Constant("v", Quoted))
    assert(pred.hasAggregate(fields))
    pred = Gte("f2", Max(F("f1")))
    assert(pred.hasAggregate(fields))

    pred = Lt("f1", Constant("v", Quoted))
    assert(!pred.hasAggregate(fields))
    pred = Lt("f1", Max(F("f1")))
    assert(pred.hasAggregate(fields))

    pred = Lt("f2", Constant("v", Quoted))
    assert(pred.hasAggregate(fields))
    pred = Lt("f2", Max(F("f1")))
    assert(pred.hasAggregate(fields))

    pred = Lte("f1", Constant("v", Quoted))
    assert(!pred.hasAggregate(fields))
    pred = Lte("f1", Max(F("f1")))
    assert(pred.hasAggregate(fields))

    pred = Lte("f2", Constant("v", Quoted))
    assert(pred.hasAggregate(fields))
    pred = Lte("f2", Max(F("f1")))
    assert(pred.hasAggregate(fields))

    pred = Not(Lt("f1", Constant("v", Quoted)))
    assert(!pred.hasAggregate(fields))
    pred = Not(Lt("f1", Max(F("f1"))))
    assert(pred.hasAggregate(fields))

    pred = And(Lt("f2", Constant("v", Quoted)), Eq("f1", Constant("v1", Quoted)))
    assert(pred.hasAggregate(fields))
    pred = Or(Lt("f2", Constant("v", Quoted)), Eq("f1", Constant("v1", Quoted)))
    assert(pred.hasAggregate(fields))
  }

  test("check numeric types") {
    assert(IntF.isNumeric())
    assert(DoubleF.isNumeric())
    assert(!StringF.isNumeric())
    assert(BooleanF.isNumeric())
    assert(DateTimeF.isNumeric())
  }

  test("check string types") {
    assert(!IntF.isStr())
    assert(!DoubleF.isStr())
    assert(StringF.isStr())
    assert(!BooleanF.isStr())
    assert(!DateTimeF.isStr())
  }

  test("check filtered query sets") {
    var qs: QuerySet = New("T1", Seq())
    assert(!qs.filtered())
    
    qs = Apply(Sort(List(("T1", Asc))), New("T1", Seq()))
    assert(!qs.filtered())

    qs = Apply(Distinct(None), New("T1", Seq()))
    assert(!qs.filtered())

    qs = Apply(Filter(Eq("q", F("q"))), New("T1", Seq()))
    assert(qs.filtered())

    qs = Union(
      New("T1", Seq()),
      Intersect(
        New("T1", Seq()),
        Apply(Filter(Eq("q", F("q"))), New("T1", Seq()))
      )
    )
    assert(qs.filtered())
  }

  test("check ordered query sets") {
    var qs: QuerySet = New("T1", Seq())
    assert(!qs.ordered())
    
    qs = Apply(Sort(List(("T1", Asc))), New("T1", Seq()))
    assert(qs.ordered())

    qs = Apply(Distinct(None), New("T1", Seq()))
    assert(!qs.ordered())

    qs = Apply(Filter(Eq("q", F("q"))), New("T1", Seq()))
    assert(!qs.ordered())

    qs = Union(
      New("T1", Seq()),
      Intersect(
        New("T1", Seq()),
        Apply(Sort(List(("q", Asc))), New("T1", Seq()))
      )
    )
    assert(!qs.ordered())

    qs = Union(
      Apply(Sort(List(("1", Asc))), New("T1", Seq())),
      Apply(Sort(List(("1", Asc))), New("T1", Seq()))
    )
    assert(!qs.ordered())

    qs = Intersect(
      Apply(Sort(List(("1", Asc))), New("T1", Seq())),
      Apply(Sort(List(("1", Asc))), New("T1", Seq()))
    )
    assert(!qs.ordered())
  }
  
}
