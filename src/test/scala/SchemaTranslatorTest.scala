import scala.io.Source

import org.scalatest.funsuite.AnyFunSuite

import cynthia.lang._
import cynthia.translators.SchemaTranslator

class SchemaTranslatorTest extends AnyFunSuite {
  val schema =
    Schema(
      "Resorting",
      Map(
        "Hyena" -> Model(
          "Hyena",
          List(
            Field("id", Serial),
            Field("simvolosira", VarChar(50)),
            Field("mammon", Numeric),
            Field("button", VarChar(50)),
            Field("walked", VarChar(50)),
            Field("facsimile", VarChar(50)),
            Field("cowhand", Int32)
          )
        ),
        "Awesome" -> Model(
          "Awesome",
          List(
            Field("id", Serial),
            Field("simvolosira", VarChar(50)),
            Field("donkey", Int32),
            Field("tunney", VarChar(50)),
            Field("pottering", VarChar(50)),
            Field("paradox", VarChar(50)),
            Field("demoting", Numeric)
          )
        ),
        "Bananas" -> Model(
          "Bananas",
          List(
            Field("id", Serial),
            Field("simvolosira", VarChar(50)),
            Field("sonars", Foreign("Sonars")),
            Field("thrilling", Int32),
            Field("energies", Numeric),
            Field("canon", Numeric)
          )
        ),
        "Omissions" -> Model(
          "Omissions",
          List(
            Field("id", Serial),
            Field("simvolosira", VarChar(50)),
            Field("discard", VarChar(50)),
            Field("underhand", Numeric),
            Field("garbo", VarChar(50)),
            Field("baez", VarChar(50)),
            Field("pals", Numeric)
          )
        ),
        "Lotteries" -> Model(
          "Lotteries",
          List(
            Field("id", Serial),
            Field("simvolosira", VarChar(50)),
            Field("cowhands", Int32),
            Field("lexica", VarChar(50)),
            Field("frontenac", Numeric),
            Field("dines", VarChar(50)),
            Field("sedating", Int32),
            Field("friends", Int32)
          )
        ),
        "Sarcomas" -> Model(
          "Sarcomas",
          List(
            Field("id", Serial),
            Field("simvolosira", VarChar(50)),
            Field("bill", Numeric),
            Field("scalping", Numeric),
            Field("omissions", Foreign("Omissions")),
            Field("broaches", VarChar(50)),
            Field("inlay", Numeric)
          )
        ),
        "Sonars" -> Model(
          "Sonars",
          List(
            Field("id", Serial),
            Field("simvolosira", VarChar(50)),
            Field("sarcomas", Foreign("Sarcomas")),
            Field("averred", VarChar(50)),
            Field("martini", Int32),
            Field("commons", Numeric),
            Field("lotteries", Foreign("Lotteries"))
          )
        )
      )
    )

  test("check schema translation to SQL") {
    val expectedSchema = Source
      .fromFile(getClass.getResource("schemas/Resorting.out").getFile())
      .mkString
    assert(SchemaTranslator(schema) == expectedSchema)
  }
}
