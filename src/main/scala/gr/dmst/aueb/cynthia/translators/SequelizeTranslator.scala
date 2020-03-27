package gr.dmst.aueb.cynthia.translators

import gr.dmst.aueb.cynthia._


case class SequelizeTranslator(t: Target) extends Translator(t) {
  val dbsettings = target.db match {
    case Postgres(user, password, dbname) =>
      Str("new Sequelize(") <<
        Utils.quoteStr(dbname) << ", " <<
        Utils.quoteStr(user) << ", " <<
        Utils.quoteStr(password) << ", {\n" <<
        "  dialect: " << Utils.quoteStr(target.db.getName()) << ",\n"
    case MySQL(user, password, dbname) =>
      Str("new Sequelize(") <<
        Utils.quoteStr(dbname) << ", " <<
        Utils.quoteStr(user) << ", " <<
        Utils.quoteStr(password) << ", {\n" <<
        "  dialect: " << Utils.quoteStr(target.db.getName()) << ",\n"
    case SQLite(dbname) =>
      Str("new Sequelize(") <<
        Utils.quoteStr(dbname) << "," <<
        "'user', 'password', {\n" <<
        "  dialect: " << Utils.quoteStr(target.db.getName()) << ",\n" <<
        "  storage: '" << dbname << "',\n"
  }
  val setstr = dbsettings << "  logging: false,\n" <<
    "  define: { timestamps: false }\n});\n"

  val preamble =
    s"""const {Op, Sequelize} = require('sequelize');
    |const sequelize = ${setstr.!}
    |
    |function dump(x) {
    |  if (typeof x === 'number') {
    |    console.log(x.toFixed(2))
    |  } else {
    |    console.log(x)
    |  }
    |}
    |
    """.stripMargin

  override def emitPrint(q: Query, ret: String) = q match {
    case SetRes (_) =>
      s"$ret.then((x) => { x.forEach((x) => dump(x.id)) })"
    case AggrRes (aggrs, _) => {
      val prints = aggrs map { x =>
        val label = x.label match {
          case None => throw new Exception(
            "You must provide a label for root aggregates")
          case Some(l) => l
        }
        s"dump(x[0].dataValues.$label)"
      } mkString ("\n  ")
      s"$ret.then((x) =>{\n  $prints})"
    }
  }

  def getSeqFieldName(field: String) =
    field.split('.').toList match {
      case Nil | _ :: Nil => field
      case _ :: t         => t.mkString(".")
    }

  def getSeqOrderSpec(field: String) = {
    def _getSeqOrderSpec(segs: List[String], acc: List[String]): List[String] =
      segs match {
        case Nil       => acc
        case h :: Nil => h :: acc
        case h :: t    => _getSeqOrderSpec(t, h.capitalize :: acc)
      }
    field.split('.').toList match {
      case Nil | _ :: Nil => field
      case _ :: t         => _getSeqOrderSpec(t, List()).reverse mkString "."
    }
  }

  def translatePred(pred: Predicate): String = pred match {
    case Eq(k, Value(v, Quoted))    =>
      (Str(getSeqFieldName(k)) << ": " << "{[Op.eq]: " << Utils.quoteStr(v) << "}").!
    case Eq(k, Value(v, UnQuoted))  =>
      (Str(getSeqFieldName(k)) << ": " << "{[Op.eq]: " << v << "}").!
    case Gt(k, Value(v, Quoted))    =>
      (Str(getSeqFieldName(k)) << ": " << "{[Op.gt]: " << Utils.quoteStr(v) << "}").!
    case Gt(k, Value(v, UnQuoted))  =>
      (Str(getSeqFieldName(k)) << ": " << "{[Op.gt]: " << v << "}").!
    case Gte(k, Value(v, Quoted))   =>
      (Str(getSeqFieldName(k)) << ": " << "{[Op.gte]: " << Utils.quoteStr(v) << "}").!
    case Gte(k, Value(v, UnQuoted)) =>
      (Str(getSeqFieldName(k)) << ": " << "{[Op.gte]: " << v << "}").!
    case Lt(k, Value(v, Quoted))    =>
      (Str(getSeqFieldName(k)) << ": " << "{[Op.lt]: " << Utils.quoteStr(v) << "}").!
    case Lt(k, Value(v, UnQuoted))  =>
      (Str(getSeqFieldName(k)) << ": " << "{[Op.lt]: " << v << "}").!
    case Lte(k, Value(v, Quoted))   =>
      (Str(getSeqFieldName(k)) << ": " << "{[Op.lte]: " << Utils.quoteStr(v) << "}").!
    case Lte(k, Value(v, UnQuoted)) =>
      (Str(getSeqFieldName(k)) << ": " << "{[Op.lte]: " << v << "}").!
    case Contains(k, v)             =>
      (Str(getSeqFieldName(k)) << ": " << "{[Op.substring]: " << Utils.quoteStr(v) << "}").!
    case Not(pred)                  =>
      (Str("[Op.not]: {") << translatePred(pred) << "}").!
    case Or(p1, p2)                 =>
      (Str("[Op.or]: [") << "{" << translatePred(p1) << "}," <<
          "{" << translatePred(p2) << "}" << "]").!
    case And(p1, p2)                =>
      (Str("[Op.and]: [") << "{" << translatePred(p1) << "}," <<
          "{" << translatePred(p2) << "}" << "]").!

  }

  def constructWhere(preds: Set[Predicate]) =
    if (preds.isEmpty) ""
    else
      (
        Str("where: {\n  [Op.and]: [\n") << (
          preds map { x => "    {" + translatePred(x) + "}" } mkString(",")
        ) << "  ]\n}"
      ).!

  def constructOrderBy(spec: Seq[(String, Order)]) = spec match {
    case Seq() => ""
    case _     =>
      (
        Str("order: [\n") << (
          spec map { x => x match {
            case (k, Asc)  => "  [" + Utils.quoteStr(getSeqOrderSpec(k)) + ", 'ASC']"
            case (k, Desc) => "  [" + Utils.quoteStr(getSeqOrderSpec(k)) + ", 'DESC']"
            }
          } mkString(",")
        ) << "]"
      ).!
  }

  def constructAggr(aggr: Aggregate) = {
    val (field, op, label) = aggr match {
      case Count(l)      => ("", "'count'", l)
      case Sum(field, l) => (field, "'sum'", l)
      case Avg(field, l) => (field, "'avg'", l)
      case Min(field, l) => (field, "'min'", l)
      case Max(field, l) => (field, "'max'", l)
      case _             => throw new UnsupportedException(
        "Complex aggregations are not supported")
    }
    val k = Utils.quoteStr(getSeqFieldName(field))
    val str = Str("[sequelize.fn(") << op
    (label, k) match {
      case (None, "''")    => (str << "), 0]").!
      case (None, _)       => (str << ", sequelize.col(" << k << ")), 0]").!
      case (Some(l), "''") => (str << "), " << Utils.quoteStr(l) << "]").!
      case (Some(l), _)    =>
        (str << ", sequelize.col(" << k << ")), " << Utils.quoteStr(l) << "]").!
    }
  }

  def constructAttributes(state: State) = {
    val attrStr = state.aggrs map { constructAggr } mkString(",\n")
    attrStr match {
      case "" => ""
      case _  => "attributes: [\n" + attrStr + "]"
    }
  }

  def importModels(joinedModels: Map[String, Set[String]], sourceModels: Set[String]) = {
    val models = joinedModels.foldLeft(sourceModels) {
      case (acc, (k, v)) => (acc + k) ++ v
    }
    models.foldLeft(QueryStr()) { (acc, x) =>
      acc >> QueryStr(Some(x),
        Some("sequelize.import(" + Utils.quoteStr(x.toLowerCase + ".js") + ")"))
    }
  }

  override def constructQuery(state: State): QueryStr = state.sources.toList match {
    case h :: Nil => {
      // Coverts set of pairs to map of lists.
      val joinMap = state.joins.groupBy(_._1).map { case (k,v) => (k, v.map(_._2)) }
      val qStr = importModels(joinMap, Set(h))
      val q = (Str(h) << ".findAll({\n" <<
        (
          Seq(
            constructAttributes(state),
            constructWhere(state.preds),
            constructOrderBy(state.orders)
          ) filter (x => x match {
            case "" => false
            case _  => true
          }) mkString(",\n")
        ) << "\n})").!
      qStr >> QueryStr(Some("ret" + state.numGen.next()), Some(q))
    }
    case _ => ???
  }

  override def unionQueries(s1: State, s2: State): State =
    throw new UnsupportedException("unions are not supported")

  override def intersectQueries(s1: State, s2: State): State =
    throw new UnsupportedException("intersections are not supported")
}
