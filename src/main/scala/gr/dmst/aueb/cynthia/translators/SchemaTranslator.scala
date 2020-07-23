package gr.dmst.aueb.cynthia.translators

import gr.dmst.aueb.cynthia._
import gr.dmst.aueb.cynthia.gen.NaiveDataGenerator


object SchemaTranslator {

  def convertDataType(t: DataType) = t match {
    case Serial | Int8 | Int16 | Int32 | Int64 | Foreign(_) => "integer"  
    case Bool => "boolean"
    case Numeric => "numeric(10, 2)"
    case VarChar(n) => "varchar(" + n + ")"
  }


  def translateModel(m: Model, numRecords: Int): QueryStr = {
    def getColumns() =
      m.fields.foldLeft(Str("")) { (acc, f) => f.ftype match {
        case Foreign(_) =>
          acc << "\"" << f.name << "_id\" " << convertDataType(f.ftype) << ",\n"
        case _ =>
          acc << Utils.quoteStr(f.name, quotes = "\"") << " " <<
            convertDataType(f.ftype) << ",\n"
      }}

    def getForeignKeys() =
      (m.fields filter Field.isForeign).foldLeft(Str("")) { (acc, x) =>
        acc << ",\nFOREIGN KEY (\"" << x.name << "_id\") REFERENCES " <<
          Utils.quoteStr(x.name, quotes = "\"") << "(id) ON DELETE NO ACTION"
      }

    def getInsertStms() =
      NaiveDataGenerator(m, numRecords, limit = numRecords).foldLeft(Str("")) { (acc, row) =>
        acc << "INSERT INTO " << Utils.quoteStr(m.name.toLowerCase, quotes = "\"") << "(" <<
          (m.fields map { x => Utils.quoteStr(Field.dbname(x), quotes = "\"") } mkString ",") <<
          ") VALUES (" <<
          (row map {
            case Constant(v, Quoted)  => Utils.quoteStr(v)
            case Constant(v, UnQuoted) => v
          } mkString ",") << ");\n"
      }


    QueryStr(None,
      Some((Str("CREATE TABLE ") << Utils.quoteStr(m.name.toLowerCase, quotes = "\"") <<
        " (\n" << getColumns <<
      "PRIMARY KEY (id)" << getForeignKeys << "\n);\n" << getInsertStms).!)
    )
  }

  def apply(schema: Schema, numRecords: Int): String = {
    // First we create a map that holds the dependencies among models
    val modelMap = schema.models.foldLeft(Map[String, Set[String]]()) { case (acc, (k, v)) => {
      val acc2 = if (acc.contains(k)) acc else acc + (k -> Set[String]())
      (v.fields filter Field.isForeign).foldLeft(acc2) { case (acc, Field(_, Foreign(n))) => {
        acc get k match {
          case None    => acc + (k -> Set(n))
          case Some(e) => acc + (k -> (e + n))
        }
      }}
    }}
    val topSort = Utils.topologicalSort(modelMap)
    // Create drop statements in reverse topological order
    val qstr = topSort.reverse.foldLeft(QueryStr()) { case (acc, m) => {
      acc >> QueryStr(None, Some("DROP TABLE IF EXISTS " + Utils.quoteStr(m.toLowerCase, quotes = "\"") + ";"))
    }}
    // Traverse models in topological order and create the corresponding
    // CREATE TABLE statements.
    topSort.foldLeft(qstr) { case (acc, m) =>
      acc >> translateModel(schema.models(m), numRecords)
    }.toString
  }
}
