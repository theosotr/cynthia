import scala.collection.immutable.ListMap

import org.scalatest.funsuite.AnyFunSuite

import cynthia.lang._

class QueryInterpreterTest extends AnyFunSuite {

  def assertState(s1: State, s2: State) = {
    assert(s1.source == s2.source)
    assert(s1.fields == s2.fields)
    assert(s1.orders == s2.orders)
    assert(s1.preds == s2.preds)
    assert(s1.aggrF == s2.aggrF)
    assert(s1.nonAggrF == s2.nonAggrF)
    assert(s1.constantF == s2.constantF)
    assert(s1.aggrs == s2.aggrs)
    assert(s1.distinct == s2.distinct)
    assert(s1.combined == s2.combined)
    assert(s1.joins == s2.joins)
  }

  test("interpret simple query") {
    val aql = SetRes(New("T1", Seq()))
    val s = QueryInterpreter(aql)
    assert(s.source == "T1")
    assert(s.fields.isEmpty)
    assert(s.orders.isEmpty)
    assert(s.preds.isEmpty)
    assert(s.aggrF.isEmpty)
    assert(s.nonAggrF.isEmpty)
    assert(s.constantF.isEmpty)
    assert(s.aggrs.isEmpty)
    assert(!s.distinct.isDefined)
    assert(!s.combined)
    assert(s.joins.isEmpty)
  }

  test("interpret simple query with fields") {
    val fields = Seq(
      FieldDecl(F("T1.id"), "id", IntF),
      FieldDecl(F("T1.col"), "col", IntF)
    )
    val aql = SetRes(New("T1", fields))
    val s = QueryInterpreter(aql)
    assert(s.source == "T1")
    assert(s.fields == ListMap("id" -> fields(0), "col" -> fields(1)))
    assert(s.orders.isEmpty)
    assert(s.preds.isEmpty)
    assert(s.aggrF.isEmpty)
    assert(s.nonAggrF.isEmpty)
    assert(s.constantF.isEmpty)
    assert(!s.distinct.isDefined)
    assert(s.aggrs.isEmpty)
    assert(!s.combined)
    assert(s.joins.isEmpty)
  }

  test("interpret simple query with ordering") {
    val fields = Seq(
      FieldDecl(F("T1.id"), "id", IntF),
      FieldDecl(F("T1.col"), "col", IntF)
    )
    val aql = SetRes(
      Apply(
        Sort(Seq(("col", Asc), ("T1.col2", Desc))),
        New("T1", fields)
      )
    )
    val s = QueryInterpreter(aql)
    assert(s.source == "T1")
    assert(s.fields == ListMap("id" -> fields(0), "col" -> fields(1)))
    assert(s.orders == Seq(("col", Asc), ("T1.col2", Desc), ("T1.id", Desc)))
    assert(s.preds.isEmpty)
    assert(s.aggrF.isEmpty)
    assert(s.nonAggrF.isEmpty)
    assert(s.constantF.isEmpty)
    assert(s.aggrs.isEmpty)
    assert(!s.distinct.isDefined)
    assert(!s.combined)
    assert(s.joins.isEmpty)
  }

  test("interpret query with predicates") {
    val pred1 = And(
      Eq("T1.col", Constant("val", Quoted)),
      Lt("T1.col", Constant("v", UnQuoted))
    )
    val pred2 = Gte("T1.col", Add(F("T1.col"), F("T1.col")))
    val aql = SetRes(
      Apply(
        Filter(pred1),
        Apply(
          Filter(pred2),
          New("T1", Seq())
        )
      )
    )
    val s = QueryInterpreter(aql)
    assert(s.source == "T1")
    assert(s.fields.isEmpty)
    assert(s.orders.isEmpty)
    assert(s.preds == Set(pred1, pred2))
    assert(s.aggrF.isEmpty)
    assert(s.nonAggrF.isEmpty)
    assert(s.constantF.isEmpty)
    assert(s.aggrs.isEmpty)
    assert(!s.distinct.isDefined)
    assert(!s.combined)
    assert(s.joins.isEmpty)
  }

  test("interpret query with aggregate fields (1)") {
    val f = FieldDecl(Sum(F("T1.col")), "sum", IntF)
    val aql = SetRes(
      New("T1", Seq(f))
    )
    val s = QueryInterpreter(aql)
    assert(s.source == "T1")
    assert(s.fields == ListMap("sum" -> f))
    assert(s.orders.isEmpty)
    assert(s.preds.isEmpty)
    assert(s.aggrF == Set("sum"))
    assert(s.nonAggrF == Set("T1.id"))
    assert(s.constantF.isEmpty)
    assert(s.aggrs.isEmpty)
    assert(!s.distinct.isDefined)
    assert(!s.combined)
    assert(s.joins.isEmpty)
  }

  test("interpret query with aggregate fields (2)") {
    val f = FieldDecl(Sum(F("T1.col")), "sum", IntF)
    val aql = SetRes(
      Apply(
        Sort(Seq(("T1.col", Desc))),
        New("T1", Seq(f))
      )
    )
    val s = QueryInterpreter(aql)
    assert(s.source == "T1")
    assert(s.fields == ListMap("sum" -> f))
    assert(s.orders == List(("T1.col", Desc), ("T1.id", Desc)))
    assert(s.preds.isEmpty)
    assert(s.aggrF == Set("sum"))
    assert(s.nonAggrF == Set("T1.id", "T1.col"))
    assert(s.constantF.isEmpty)
    assert(s.aggrs.isEmpty)
    assert(!s.distinct.isDefined)
    assert(!s.combined)
    assert(s.joins.isEmpty)
  }

  test("interpret query with aggregate fields (3)") {
    val f = FieldDecl(Sum(F("T1.col")), "sum", IntF)
    val f2 = FieldDecl(F("T1.col2"), "col2", IntF)
    val pred = Eq("T1.col3", Constant("!", Quoted))
    val aql = SetRes(
      Apply(
        Filter(pred),
        Apply(
          Sort(Seq(("T1.col", Desc))),
          New("T1", Seq(f, f2))
        )
      )
    )
    val s = QueryInterpreter(aql)
    assert(s.source == "T1")
    assert(s.fields == ListMap("sum" -> f, "col2" -> f2))
    assert(s.orders == List(("T1.col", Desc), ("T1.id", Desc)))
    assert(s.preds == Set(pred))
    assert(s.aggrF == Set("sum"))
    assert(s.nonAggrF == Set("col2", "T1.col", "T1.col2", "T1.id", "T1.col3"))
    assert(s.constantF.isEmpty)
    assert(s.aggrs.isEmpty)
    assert(!s.distinct.isDefined)
    assert(!s.combined)
    assert(s.joins.isEmpty)
  }

  test("interpret query with aggregate fields and constants") {
    val f = FieldDecl(Sum(F("T1.col")), "sum", IntF)
    val f2 = FieldDecl(Constant("Val", Quoted), "expr", IntF)
    val aql = SetRes(
      Apply(
        Sort(Seq(("T1.col", Desc))),
        New("T1", Seq(f, f2))
      )
    )
    val s = QueryInterpreter(aql)
    assert(s.source == "T1")
    assert(s.fields == ListMap("sum" -> f, "expr" -> f2))
    assert(s.orders == List(("T1.col", Desc), ("T1.id", Desc)))
    assert(s.preds.isEmpty)
    assert(s.aggrF == Set("sum"))
    assert(s.nonAggrF == Set("T1.col", "T1.id", "expr"))
    assert(s.constantF == Set("expr"))
    assert(s.aggrs.isEmpty)
    assert(!s.combined)
    assert(!s.distinct.isDefined)
    assert(s.joins.isEmpty)
  }

  test("interpret query with aggregate fields and nested expressions") {
    val f = FieldDecl(Sum(F("T1.col")), "sum", IntF)
    val f2 = FieldDecl(
      Add(
        F("T1.col2"),
        Sub(F("T1.col5"), F("T1.col4"))
      ),
      "col2",
      IntF
    )
    val pred = Eq("T1.col3", Constant("!", Quoted))
    val aql = SetRes(
      Apply(
        Filter(pred),
        Apply(
          Sort(Seq(("T1.col", Desc))),
          New("T1", Seq(f, f2))
        )
      )
    )
    val s = QueryInterpreter(aql)
    assert(s.source == "T1")
    assert(s.fields == ListMap("sum" -> f, "col2" -> f2))
    assert(s.orders == List(("T1.col", Desc), ("T1.id", Desc)))
    assert(s.preds == Set(pred))
    assert(s.aggrF == Set("sum"))
    assert(
      s.nonAggrF == Set(
        "col2",
        "T1.col",
        "T1.col2",
        "T1.id",
        "T1.col3",
        "T1.col4",
        "T1.col5"
      )
    )
    assert(s.constantF.isEmpty)
    assert(s.aggrs.isEmpty)
    assert(!s.combined)
    assert(!s.distinct.isDefined)
    assert(s.joins.isEmpty)
  }

  test("interpret query with aggregate fields and hidden fields") {
    val f = FieldDecl(Sum(F("T1.col")), "sum", IntF)
    val f2 = FieldDecl(F("T1.col2"), "expr", IntF, true)
    val aql = SetRes(New("T1", Seq(f, f2)))
    val s = QueryInterpreter(aql)
    assert(s.source == "T1")
    assert(s.fields == ListMap("sum" -> f, "expr" -> f2))
    assert(s.orders.isEmpty)
    assert(s.preds.isEmpty)
    assert(s.aggrF == Set("sum"))
    assert(s.nonAggrF == Set("T1.id"))
    assert(s.constantF.isEmpty)
    assert(s.aggrs.isEmpty)
    assert(!s.distinct.isDefined)
    assert(!s.combined)
    assert(s.joins.isEmpty)
  }

  test("interpret query with joins") {
    val f = FieldDecl(F("T1.t2.id"), "f1", IntF)
    val pred = Eq("T1.t3.id", Constant("v", Quoted))
    val aggrF = FieldDecl(Sum(F("T1.t2.t3.id")), "expr", IntF)
    val aql = AggrRes(
      Seq(aggrF),
      Apply(
        Sort(List(("T1.t4.t5.t6.id", Asc))),
        Apply(
          Filter(pred),
          New("T1", Seq(f))
        )
      )
    )
    val s = QueryInterpreter(aql)
    assert(s.source == "T1")
    assert(s.fields == ListMap("f1" -> f))
    assert(s.orders == List(("T1.t4.t5.t6.id", Asc), ("T1.id", Desc)))
    assert(s.preds == Set(pred))
    assert(s.aggrF.isEmpty)
    assert(s.nonAggrF.isEmpty)
    assert(s.constantF.isEmpty)
    assert(s.aggrs == Seq(aggrF))
    assert(!s.combined)
    assert(!s.distinct.isDefined)
    assert(
      s.joins == Seq(
        Seq("T1", "T2"),
        Seq("T1", "T3"),
        Seq("T1", "T4", "T5", "T6"),
        Seq("T1", "T4", "T5"),
        Seq("T1", "T4"),
        Seq("T1", "T2", "T3"),
        Seq("T1", "T2")
      )
    )
  }

  test("interpret UNION query") {
    val f = FieldDecl(Sum(F("T1.col")), "sum", IntF)
    val f2 = FieldDecl(F("T1.col2"), "col2", IntF)
    val pred = Eq("T1.col3", Constant("!", Quoted))
    val q1 = (
      Apply(
        Filter(pred),
        Apply(
          Sort(Seq(("T1.col", Desc))),
          New("T1", Seq(f, f2))
        )
      )
    )

    val f3 = FieldDecl(Sum(F("T1.col")), "sum", IntF)
    val f4 = FieldDecl(Sum(F("T1.col")), "sum2", IntF)
    val q2 = New("T1", Seq(f3, f4))

    val aggrF = FieldDecl(Max(F("id")), "expr", IntF)
    val aql = AggrRes(
      Seq(aggrF),
      Apply(
        Sort(Seq(("T1.col", Asc))),
        Union(q1, q2)
      )
    )
    val s = QueryInterpreter(aql)
    val s1 = QueryInterpreter(SetRes(q1))
    val s2 = QueryInterpreter(SetRes(q2))

    assert(s.source == "T1")
    assert(s.fields == ListMap("sum" -> f, "col2" -> f2))
    assert(s.orders == Seq(("T1.col", Asc)))
    assert(s.preds.isEmpty)
    assert(s.aggrs == Seq(aggrF))
    assert(s.combined)
    assert(!s.distinct.isDefined)
    assert(s.joins.isEmpty)
    val (expS1, expS2) = s.from match {
      case Some(UnionState(s1, s2)) => (s1, s2)
      case _                        => ???
    }
    assertState(s1, expS1)
    assertState(s2, expS2)
  }

  test("interpret INTERSECTION query") {
    val f = FieldDecl(Sum(F("T1.col")), "sum", IntF)
    val f2 = FieldDecl(F("T1.col2"), "col2", IntF)
    val pred = Eq("T1.col3", Constant("!", Quoted))
    val q1 = (
      Apply(
        Filter(pred),
        Apply(
          Sort(Seq(("T1.col", Desc))),
          New("T1", Seq(f, f2))
        )
      )
    )

    val f3 = FieldDecl(Sum(F("T1.col")), "sum", IntF)
    val f4 = FieldDecl(Sum(F("T1.col")), "sum2", IntF)
    val q2 = New("T1", Seq(f3, f4))

    val aggrF = FieldDecl(Max(F("id")), "expr", IntF)
    val aql = AggrRes(
      Seq(aggrF),
      Apply(
        Sort(Seq(("T1.col", Asc))),
        Intersect(q1, q2)
      )
    )
    val s = QueryInterpreter(aql)
    val s1 = QueryInterpreter(SetRes(q1))
    val s2 = QueryInterpreter(SetRes(q2))

    assert(s.source == "T1")
    assert(s.fields == ListMap("sum" -> f, "col2" -> f2))
    assert(s.orders == Seq(("T1.col", Asc)))
    assert(s.preds.isEmpty)
    assert(s.aggrs == Seq(aggrF))
    assert(s.combined)
    assert(!s.distinct.isDefined)
    assert(s.joins.isEmpty)
    val (expS1, expS2) = s.from match {
      case Some(IntersectState(s1, s2)) => (s1, s2)
      case _                            => ???
    }

    assertState(s1, expS1)
    assertState(s2, expS2)
  }

  test("interpret query with distict") {
    val aql = SetRes(
      Apply(
        Distinct(None),
        New("T1", Seq())
      )
    )
    val s = QueryInterpreter(aql)
    assert(s.source == "T1")
    assert(s.fields.isEmpty)
    assert(s.orders.isEmpty)
    assert(s.preds.isEmpty)
    assert(s.aggrF.isEmpty)
    assert(s.nonAggrF.isEmpty)
    assert(s.constantF.isEmpty)
    assert(s.aggrs.isEmpty)
    assert(s.distinct == Some(""))
    assert(!s.combined)
    assert(s.joins.isEmpty)
  }

  test("interpret query with distinct ON") {
    val aql = SetRes(
      Apply(
        Distinct(Some("T1.t2.id")),
        New("T1", Seq())
      )
    )
    val s = QueryInterpreter(aql)
    assert(s.source == "T1")
    assert(s.fields.isEmpty)
    assert(s.orders.isEmpty)
    assert(s.preds.isEmpty)
    assert(s.aggrF.isEmpty)
    assert(s.nonAggrF.isEmpty)
    assert(s.constantF.isEmpty)
    assert(s.aggrs.isEmpty)
    assert(s.distinct == Some("T1.t2.id"))
    assert(!s.combined)
    assert(s.joins == Seq(Seq("T1", "T2")))
  }

  test("FirstRes quey throws exception when queryset is unordered") {
    val aql = FirstRes(New("T1", Seq()))
    assertThrows[InvalidQuery] {
      QueryInterpreter(aql)
    }
  }

  test("SubsetRes query throws exception when queryset is unordered") {
    val aql = SubsetRes(0, None, New("T1", Seq()))
    assertThrows[InvalidQuery] {
      QueryInterpreter(aql)
    }
  }
}
