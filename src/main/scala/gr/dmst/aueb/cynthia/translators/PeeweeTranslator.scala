package gr.dmst.aueb.cynthia.translators

import gr.dmst.aueb.cynthia._


case class PeeweeTranslator(t: Target) extends Translator(t) {

  override val preamble =
    s"""from models import *
    |
    |def dump(x):
    |    if isinstance(x, numbers.Number):
    |        print(round(decimal.Decimal(x), 2))
    |    else:
    |        print(x)
    |""".stripMargin

  override def emitPrint(q: Query, dFields: Seq[String], ret: String) = ""

  override def constructQuery(first: Boolean = false, offset: Int = 0,
      limit: Option[Int] = None)(s: State) = QueryStr(Some("var"))

  override def unionQueries(s1: State, s2: State) = s1

  override def intersectQueries(s1: State, s2: State) = s1
}
