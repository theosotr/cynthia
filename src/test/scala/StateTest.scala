import scala.collection.immutable.ListMap

import org.scalatest.funsuite.AnyFunSuite

import cynthia.lang._


class StateTest extends AnyFunSuite {
  test("check that we're getting joins as a set of pairs") {
    var joins = Seq(
        Seq("a", "b"),
        Seq("a", "c"),
        Seq("a", "b")
    )
    assert(State(joins = joins).getJoinPairs() == Set(("a", "b"), ("a", "c")))
    
    joins = Seq(
        Seq("a", "b", "c"),
        Seq("a", "c"),
        Seq("a", "b", "c"),
        Seq("b", "c", "d", "e"),
        Seq("a", "c", "e"),
    )
    assert(State(joins = joins).getJoinPairs() == Set(
      ("b", "c"), ("a", "c"), ("d", "e"), ("c", "e"))
    )
  }

  test("check that we're getting non-constant grouping fields") {
    var s = State(nonAggrF = Set("f1", "f2"))
    assert(s.getNonConstantGroupingFields() == Set("f1", "f2"))

    s = State(nonAggrF = Set("f1", "f2"), constantF = Set("f1"))
    assert(s.getNonConstantGroupingFields() == Set("f2"))
  }

  test("check combinination of states") {
    val s1 = State(
      source = "t1",
      fields = ListMap("f1" -> FieldDecl(F("f1"), "f", IntF)),
      orders = Seq(("a", Asc)),
      nonAggrF = Set("f1"),
      aggrF = Set("f2"),
      constantF = Set("f3"),
      distinct = Some("f"),
      aggrs = Seq(FieldDecl(Count(None), "a", IntF)),
      joins = Seq(Seq("a", "b")),
    )
    val s2 = State(
      source = "t2",
      fields = ListMap("f2" -> FieldDecl(F("f1"), "f", IntF)),
      preds = Set(Eq("a", Constant("b", Quoted))),
      nonAggrF = Set("f2"),
      aggrF = Set("f3"),
      constantF = Set("f4"),
      distinct = Some("a"),
      aggrs = Seq(FieldDecl(Count(None), "a", IntF)),
    )
    var s3 = UnionState.combine(s1, s2)
    assert(s3.source == s1.source)
    assert(s3.fields == s1.fields)
    assert(s3.aggrs.isEmpty)
    assert(s3.aggrF.isEmpty)
    assert(s3.orders.isEmpty)
    assert(s3.preds.isEmpty)
    assert(!s3.distinct.isDefined)
    assert(s3.nonAggrF.isEmpty)
    assert(s3.constantF.isEmpty)
    assert(s3.combined)
    assert(s3.from == Some(UnionState(s1, s2)))

    s3 = IntersectState.combine(s1, s2)
    assert(s3.source == s1.source)
    assert(s3.fields == s1.fields)
    assert(s3.aggrs.isEmpty)
    assert(s3.aggrF.isEmpty)
    assert(s3.orders.isEmpty)
    assert(s3.preds.isEmpty)
    assert(!s3.distinct.isDefined)
    assert(s3.nonAggrF.isEmpty)
    assert(s3.constantF.isEmpty)
    assert(s3.combined)
    assert(s3.from == Some(IntersectState(s1, s2)))
  }
  
}
