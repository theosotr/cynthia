package gr.dmst.aueb.cynthia.translators

import gr.dmst.aueb.cynthia._


case class PonyTranslator(target: Target) extends Translator {
  target.db match {
    case MSSQL(_, _, _) =>
      throw UnsupportedException("Pony does not support MSSQL queries")
    case _ => ()
  }

  override val preamble =
    s"""import numbers
    |from decimal import Decimal
    |from pony.orm import *
    |from models import *
    |
    |# set_sql_debug(True)
    |db.generate_mapping()
    |
    |def dump(x, label):
    |    if isinstance(x, numbers.Number):
    |        print(label, round(Decimal(float(x) + 0.00), 2))
    |    else:
    |        try:
    |            print(label, round(Decimal(float(x) + 0.00), 2))
    |        except:
    |            if type(x) is bytes:
    |                print(label, str(x.decode('utf-8')))
    |            else:
    |                print(label, x if x is not None else '0.00')
    |""".stripMargin

  override def emitPrint(q: Query, dFields: Seq[String], ret: String) = ""

  override def constructCombinedQuery(s: State) = QueryStr(Some("var"))

  override def constructNaiveQuery(s: State, first: Boolean, offset: Int,
      limit: Option[Int]) = QueryStr(Some("var"))

  override def unionQueries(s1: State, s2: State) =
    throw new UnsupportedException("unions are not implemented")

  override def intersectQueries(s1: State, s2: State) =
    throw new UnsupportedException("unions are not implemented")
}
