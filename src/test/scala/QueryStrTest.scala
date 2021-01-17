import org.scalatest.funsuite.AnyFunSuite

import cynthia.translators.QueryStr


class QueryStrTest extends AnyFunSuite {
  test("check query str concatenation (1)") {
    val q1 = QueryStr(None, Some("stm"))
    val q2 = QueryStr(None, Some("stm2"))

    var q3 = q1 >> q2
    assert(q3.toBuiltQ() == Seq("stm", "stm2"))
    assert(!q3.ret.isDefined)

    q3 = q1 << q2
    assert(q3.toBuiltQ() == Seq("stm", "stm2"))
    assert(!q3.ret.isDefined)
  }

  test("check query str concatenation (2)") {
    val q1 = QueryStr(Some("ret1"), Some("stm"))
    val q2 = QueryStr(None, Some("stm2"))

    var q3 = q1 >> q2
    assert(q3.toBuiltQ() == Seq("ret1 = stm", "stm2"))
    assert(!q3.ret.isDefined)

    q3 = q1 << q2
    assert(q3.toBuiltQ() == Seq("ret1 = stm", "stm2"))
    assert(q3.ret == Some("ret1"))
  }

  test("check query str concatenation (3)") {
    val q1 = QueryStr(None, Some("stm"))
    val q2 = QueryStr(Some("ret1"), Some("stm2"))

    var q3 = q1 >> q2
    assert(q3.toBuiltQ() == Seq("stm", "ret1 = stm2"))
    assert(q3.ret == Some("ret1"))

    q3 = q1 << q2
    assert(q3.toBuiltQ() == Seq("stm", "ret1 = stm2"))
    assert(!q3.ret.isDefined)
  }

  test("check query str concatenation (4)") {
    val q1 = QueryStr(Some("ret1"), Some("stm"))
    val q2 = QueryStr(Some("ret2"), Some("stm2"))

    var q3 = q1 >> q2
    assert(q3.toBuiltQ() == Seq("ret1 = stm", "ret2 = stm2"))
    assert(q3.ret == Some("ret2"))

    q3 = q1 << q2
    assert(q3.toBuiltQ() == Seq("ret1 = stm", "ret2 = stm2"))
    assert(q3.ret == Some("ret1"))
  }
}
