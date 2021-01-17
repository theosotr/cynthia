import org.scalatest.funsuite.AnyFunSuite

import cynthia.lang.{F, FieldDecl, IntF, Add, Max}
import cynthia.translators.TUtils


class TranslatorUtilsTest extends AnyFunSuite {
  test("filter fields and get their names") {
    val fields = Seq(
      FieldDecl(F("f"), "f", IntF),
      FieldDecl(F("f"), "a", IntF),
      FieldDecl(F("f"), "f2", IntF)
    )

    var v = TUtils.filterMapAs(x => true)(fields)
    assert(v == Seq("f", "a", "f2"))

    v = TUtils.filterMapAs(x => x.as.startsWith("f"))(fields)
    assert(v == Seq("f", "f2"))
  }


  test("get hidden fields and map") {
    val fields = Seq(
      FieldDecl(F("f"), "f", IntF),
      FieldDecl(F("f"), "a", IntF, true),
      FieldDecl(F("f"), "f2", IntF)
    )

    val v = TUtils.mapHiddenFields(fields, x => x.as)
    assert(v.toSeq == Seq("a"))
  }

  test("get non-hidden fields and map") {
    val fields = Seq(
      FieldDecl(F("f"), "f", IntF),
      FieldDecl(F("f"), "a", IntF, true),
      FieldDecl(F("f"), "f2", IntF)
    )

    val v = TUtils.mapNonHiddenFields(fields, x => x.as)
    assert(v.toSeq == Seq("f", "f2"))
  }

  test("filter hidden fields") {
    val fields = Seq(
      FieldDecl(F("f"), "f", IntF),
      FieldDecl(F("f"), "a", IntF, true),
      FieldDecl(F("f"), "f2", IntF)
    )

    val v = TUtils.filterHidden(fields)
    assert(v.toSeq == Seq("a"))
  }

  test("filter hidden and aggregate fields") {
    val fields = Seq(
      FieldDecl(F("f"), "f", IntF),
      FieldDecl(F("f"), "a", IntF, true),
      FieldDecl(Add(F("f"), Max(F("f2"))), "f2", IntF)
    )

    val v = TUtils.filterNonAggrHidden(fields)
    assert(v.toSeq == Seq("f"))
  }

  test("split fields into aggregate and non-aggregate ones") {
    val fields = Seq(
      FieldDecl(F("f"), "f1", IntF),
      FieldDecl(Add(F("f"), Max(F("f2"))), "f4", IntF),
      FieldDecl(F("f"), "f2", IntF),
      FieldDecl(F("f"), "f3", IntF, true),
      FieldDecl(Max(F("f4")), "f5", IntF),
      FieldDecl(Add(F("f"), Max(F("f2"))), "f6", IntF, true),
    )

    val (aggrF, nonAggrF) = TUtils.getAggrAndNonAggr(fields)
    assert(aggrF.toSeq == Seq("f4", "f5"))
    assert(nonAggrF.toSeq == Seq("f1", "f2"))
  }
  
}
