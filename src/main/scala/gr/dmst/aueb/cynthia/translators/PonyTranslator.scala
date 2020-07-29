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
    |from models_${target.db.getName} import *
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


  def getPonyFieldName(field: String) =
    field.split('.').toList match {
      case Nil | _ :: Nil => field
      case _ :: t         => t mkString "__"
    }

  override def emitPrint(q: Query, dFields: Seq[String], ret: String) = {
    def _dumpField(v: String, fields: Iterable[String], ident: String = "") =
      fields map { as => s"${ident}dump(getattr($v, '$as', None), '$as')" } mkString "\n"

    val toPrint = q match {
      case SetRes(_) | SubsetRes(_, _, _) =>
        s"    for r in $ret:\n${_dumpField("r", dFields, ident = " " * 8)}"
      case _ => ???
    }
    "with db_session:\n" + toPrint
  }

  def constructQueryPrefix(s: State) =  s.query match {
    case None    =>
      val qstr = s"select(i for i in ${s.source})"
      QueryStr(
        Some("ret" + s.numGen.next().toString),
        Some(qstr)
      )
    case Some(q) => q
  }

  def constructOrderBy(s: State) = s.orders match {
    case Seq() => ""
    case spec  => {
      (
        Str("order_by(lambda i: (") << (
          spec map { x =>
            x match {
              case (k, Desc) => "desc(i." + getPonyFieldName(k) + ")"
              case (k, Asc)  => "i." + getPonyFieldName(k)
            }
          } mkString ","
        ) << "))"
      ).!
      "order_by(lambda i: (desc(i.rating), desc(i.id)))"
    }
  }

  override def constructCombinedQuery(s: State) = QueryStr(Some("combined"))

  override def constructNaiveQuery(s: State, first: Boolean, offset: Int,
      limit: Option[Int]) = {
    val qStr = constructQueryPrefix(s)
    qStr >> QueryStr(Some("ret" + s.numGen.next().toString),
      Some(
        Seq(
          qStr.ret.get,
          constructOrderBy(s)
        ) filter {
          case "" => false
          case _  => true
        }  mkString(".")
      )
    )
  }

  override def unionQueries(s1: State, s2: State) =
    throw new UnsupportedException("unions are not implemented")

  override def intersectQueries(s1: State, s2: State) =
    throw new UnsupportedException("unions are not implemented")
}
