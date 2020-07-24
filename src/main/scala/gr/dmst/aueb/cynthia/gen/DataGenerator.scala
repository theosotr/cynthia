package gr.dmst.aueb.cynthia.gen


import gr.dmst.aueb.cynthia._
import gr.dmst.aueb.cynthia.translators.{Z3SolverTranslator,State}

import scala.sys.process._


object NaiveDataGenerator {
  def generateRow(fields: Seq[Field], i: Int, foreignKeyCands: Int): Seq[Constant] =
    fields map { case Field(n, t) =>  t match {
      case Serial => Constant(i.toString, UnQuoted)
      case Int8 | Int16 | Int32 | Int64 | Numeric =>
        Constant(
          (RUtils.integer() * (RUtils.chooseFrom(List(1, -1)))).toString,
          UnQuoted
        )
      case VarChar(n) => Constant(Utils.escapeSQLStr(RUtils.word()), Quoted)
      case Bool => Constant(RUtils.bool().toString, UnQuoted)
      case Foreign(_) => Constant(
        (RUtils.integer(n = foreignKeyCands) + 1).toString, UnQuoted)
    }}

  def apply(model: Model, foreignKeyCands: Int, limit: Int = 1000) = {
    def _generateData(data: LazyList[Seq[Constant]], i: Int): LazyList[Seq[Constant]] =
      if (i <= 0) data
      else _generateData(generateRow(model.fields, i, foreignKeyCands) #:: data, i - 1)

    _generateData(LazyList.empty, limit)
  }
}


object SolverDataGenerator {
  def apply(q: Query, state: State, s: Schema) = {
    val formula = Z3SolverTranslator(s)(q, state)
    Utils.writeToFile("solve.py", formula)
  }
}
