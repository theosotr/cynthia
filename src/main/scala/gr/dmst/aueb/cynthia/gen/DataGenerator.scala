package gr.dmst.aueb.cynthia.gen


import gr.dmst.aueb.cynthia._


object DataGenerator {
  def generateRow(fields: Seq[Field], i: Int, foreignKeyCands: Int): Seq[Constant] =
    fields map { case Field(n, t) =>  t match {
      case Serial => Constant(i.toString, UnQuoted)
      case Int8 | Int16 | Int32 | Int64 | Numeric =>
        Constant(
          (RUtils.integer() * (RUtils.chooseFrom(List(1, -1)))).toString,
          UnQuoted
        )
      case VarChar(n) => Constant(Utils.escapeStr(RUtils.word()), Quoted)
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
