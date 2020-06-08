package gr.dmst.aueb.cynthia.translators

import gr.dmst.aueb.cynthia._


case class SequelizeTranslator(t: Target) extends Translator(t) {
  private val fieldDecls: scala.collection.mutable.Set[String] =
    scala.collection.mutable.Set()

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
    |function getC(e) {
    |  if (e.col !== undefined)
    |    return e.col;
    |  if (e.val.col !== undefined)
    |    return e.val.col;
    |  if (e.val.val === undefined) {
    |    args = e.val.args
    |    if (args === undefined)
    |      return e.val.fn + "()"
    |    return args.val !== undefined ? e.val.fn + "(" + args.val + ")" :
    |      e.val.fn + "(" + args.col + ")"
    |  }
    |  if (typeof e.val.val === 'string') return e.val.val;
    |  else return getC(e.val);
    |}
    |
    |function dump(x) {
    |  if (x === null) {
    |    console.log('None')
    |    return;
    |  }
    |  if (typeof x === 'number') {
    |    console.log(x.toFixed(2))
    |  } else {
    |    console.log(x)
    |  }
    |}
    |
    """.stripMargin

  override def emitPrint(q: Query, dFields: Seq[String], ret: String) = {
    def _dumpField(v: String, fields: Iterable[String], ident: String = "") =
      fields map { as =>
        s"${ident}dump($v.dataValues.$as)"
      } mkString "\n"
    val str = q match {
      case SetRes(_)  | SubsetRes(_, _, _) =>
        s"""$ret.then((x) => {
        |  sequelize.close()
        |  x.forEach((x) => {
        |    ${_dumpField("x", dFields, ident = " " * 2)}
        |  })
        |})""".stripMargin
      case FirstRes(_) =>
        s"""$ret.then(x => {
        |  sequelize.close()
        |  ${_dumpField("x", dFields, ident = " " * 2)}
        |})""".stripMargin
      case AggrRes (aggrs, _) => {
        val aggrF = TUtils.mapNonHiddenFields(aggrs, FieldDecl.as)
        s"""$ret.then((x) => {
        |  sequelize.close()
        |  ${_dumpField("x[0]", aggrF, ident = " " * 2)}
        |})""".stripMargin
      }
    }
    str + ".catch(x => { sequelize.close(); throw x; })"
  }

  def getSeqFieldName(field: String, dollarSign: Boolean = true) = {
    def _constructField(field: String) =
      if (dollarSign) "'$" + field + "$'"
      else Utils.quoteStr(field)

    field.split('.').toList match {
      case Nil | _ :: Nil  => _constructField(field)
      case _ :: (h :: Nil) => _constructField(h)
      case _ :: t          =>
        if (dollarSign) "'$" + t.mkString(".") + "$'"
        else Utils.quoteStr(t mkString ".")
    }
  }

  def getSeqOrderSpec(field: String) = {
    def _getSeqOrderSpec(segs: List[String], acc: List[String]): List[String] =
      segs match {
        case Nil       => acc
        case h :: Nil  => h :: acc
        case h :: t    => _getSeqOrderSpec(t, h.capitalize :: acc)
      }
    field.split('.').toList match {
      case Nil | _ :: Nil => Utils.quoteStr(field)
      case _ :: t         => {
        val head :: tail = _getSeqOrderSpec(t, List())
        // Quote the first element, reverse the list and create the string.
        (Utils.quoteStr(head) :: tail).reverse mkString ","
      } 
    }
  }

  def translatePred(s: State, pred: Predicate): String = pred match {
    case Eq(k, e) =>
      (Str(getSeqFieldName(k)) << ": " << "{[Op.eq]: " << constructFieldExpr(s, e) << "}").!
    case Gt(k, e) =>
      (Str(getSeqFieldName(k)) << ": " << "{[Op.gt]: " << constructFieldExpr(s, e) << "}").!
    case Gte(k, e) =>
      (Str(getSeqFieldName(k)) << ": " << "{[Op.gte]: " << constructFieldExpr(s, e) << "}").!
    case Lt(k, e)  =>
      (Str(getSeqFieldName(k)) << ": " << "{[Op.lt]: " << constructFieldExpr(s, e) << "}").!
    case Lte(k, e) =>
      (Str(getSeqFieldName(k)) << ": " << "{[Op.lte]: " << constructFieldExpr(s, e) << "}").!
    case Contains(k, e) =>
      (Str(getSeqFieldName(k)) << ": " << "{[Op.substring]: " << constructFieldExpr(s, e) << "}").!
    case Not(pred)                  =>
      (Str("[Op.not]: {") << translatePred(s, pred) << "}").!
    case Or(p1, p2)                 =>
      (Str("[Op.or]: [") << "{" << translatePred(s, p1) << "}," <<
          "{" << translatePred(s, p2) << "}" << "]").!
    case And(p1, p2)                =>
      (Str("[Op.and]: [") << "{" << translatePred(s, p1) << "}," <<
          "{" << translatePred(s, p2) << "}" << "]").!
  }

  def constructFilter(s: State, preds: Set[Predicate], having: Boolean = false) = {
    val method = if (having) "having" else "where"
    if (preds.isEmpty) ""
    else
      (Str(method) << ": {\n  [Op.and]: [\n" << (
        preds map { x => "    {" + translatePred(s, x) + "}" } mkString(",")
       ) << "  ]\n}"
      ).!
  }

  def constructOrderBy(spec: Seq[(String, Order)],
      fields: Map[String, FieldDecl]) = spec match {
    case Seq() => ""
    case _     =>
      (
        Str("order: [\n") << (
          spec map { x => {
            val name = if (fields.contains(x._1)) x._1 else getSeqOrderSpec(x._1)
            x._2 match {
              case Asc  => "  [" + name + ", 'ASC']"
              case Desc => "  [" + name + ", 'DESC']"
            }
          }} mkString ","
        ) << "]"
      ).!
  }

  def constructNestedFieldExpr(s: State, fexpr: FieldExpr, subCol: Boolean): String = {
    def _f(expr: FieldExpr) = expr match {
      case Constant(v, UnQuoted) => v
      case Constant(v, Quoted)   => s"\\'${v}\\'"
      case F(f)                  => s.fields get f match {
        case None => {
          val segs = f split '.'
          ((segs drop (segs.size - 2)) mkString ".").toLowerCase
        }
        case _    => "${getC(" + f + ")}"
      }
      case _ => throw new UnsupportedException(
        "Unsupported field expression: " + fexpr.toString)
    }

    fexpr match {
      case Constant(v, UnQuoted) => "sequelize.literal(" + v + ")"
      case Constant(v, Quoted)   =>
        "sequelize.literal(" + Utils.quoteStr(s"\\'${v}\\'") + ")"
      case F(f) => {
        val str = "sequelize.col(" + getSeqFieldName(f, dollarSign = false) + ")"
        if (subCol && this.fieldDecls.contains(f)) f
        else str
      }
      case Add(f1, f2) =>
        "sequelize.literal(`" + _f(f1) + " + " + _f(f2) + "`)"
      case Sub(f1, f2) =>
        "sequelize.literal(`" + _f(f1) + " - " + _f(f2) + "`)"
      case Mul(f1, f2) =>
        "sequelize.literal(`" + _f(f1) + " * " + _f(f2) + "`)"
      case Div(f1, f2) =>
        "sequelize.literal(`" + _f(f1) + " / " + _f(f2) + "`)"
      case _ => throw new UnsupportedException(
        "Unsupported field expression: " + fexpr.toString)
    }
  }

  def getType(ftype: FieldType) = Utils.quoteStr(ftype.convertType(target.db))

  def constructFieldExpr(s: State, fexpr: FieldExpr, subCol: Boolean = false): String = {
    val (f, op) = fexpr match {
      case Count(None)        => ("'*'", Some("'count'"))
      case Count(Some(fexpr)) => (constructNestedFieldExpr(s, fexpr, subCol), Some("'count'"))
      case Sum(fexpr)         => (constructNestedFieldExpr(s, fexpr, subCol), Some("'sum'"))
      case Avg(fexpr)         => (constructNestedFieldExpr(s, fexpr, subCol), Some("'avg'"))
      case Min(fexpr)         => (constructNestedFieldExpr(s, fexpr, subCol), Some("'min'"))
      case Max(fexpr)         => (constructNestedFieldExpr(s, fexpr, subCol), Some("'max'"))
      case _                  => (constructNestedFieldExpr(s, fexpr, subCol), None)
    }
    op match {
      case None     => f
      case Some(op) =>
        val str = Str("sequelize.fn(") << op
        f match {
          case "" => (str << ")").!
          case _  =>
            (str << ", " << f << ")").!
        }
    }
  }

  def constructFieldDecls(s: State, fields: Iterable[FieldDecl]) = {
    def castField(f: String, t: String) =
      (Str("sequelize.cast(") << f << "," << t << ")").!

    fields.foldLeft(QueryStr()) { (acc, x) => {
      val (qfield, as, t) = x match { case FieldDecl(f, l, t, _) => (f, l, t) }
      val f = constructFieldExpr(s, qfield, subCol = true)
      val qstr = castField(f, getType(t))
      this.fieldDecls += as
      acc >> QueryStr(
        Some(as),
        Some(qstr)
      )
    }}
  }

  def constructAttributes(state: State) = {
    val attrStr = TUtils.mapNonHiddenFields(
      state.fields.values ++ state.aggrs,
      { case FieldDecl(_, as, _, _) =>
        (Str("[") << as << ", " << Utils.quoteStr(as) << "]").!
      }
    ) mkString(",\n    ")
    attrStr match {
      case "" => ""
      case _  => "attributes: [\n " + attrStr + "]"
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

  def createAssociations(joinedModels: Map[String, Set[String]]) =
    joinedModels.foldLeft(QueryStr()) { case (acc, (k, v)) =>
      v.foldLeft(acc) { (acc, x) => {
        val fk = s"{foreignKey: '${x.toLowerCase}_id'}"
        acc >>
          QueryStr(None, Some(s"$x.hasMany($k, ${fk})")) >>
          QueryStr(None, Some(s"$k.belongsTo($x, ${fk})"))
        }
      }
    }

  def constructIncludes(source: String, joinedModels: Map[String, Set[String]]) = {
    def _findIncludes(n: String): String = {
      val str = s"model: $n, as: $n.tableName"
      joinedModels.get(n) match {
        case None     => s"{$str}"
        case Some(e)  => {
          val includes = e map { _findIncludes } mkString (",\n")
          if (source.equals(n)) includes
          else s"{$str, include: [\n${includes}\n]}"
        }
      }
    }
    if (joinedModels.isEmpty) ""
    else "include: [\n" + _findIncludes(source) + "\n]"
  }

  def constructGroupBy(groupBy: Set[String]) =
    if (groupBy.isEmpty) ""
    else
      "group: [" + (
      groupBy map { x => getSeqFieldName(x, false) } mkString ", ") + "]"

  override def constructQuery(first: Boolean = false, offset: Int = 0,
      limit: Option[Int] = None)(s: State): QueryStr = {
    val fieldVals = s.fields.values
    val (aggrNHidden, nonAggrHidden) = TUtils.getAggrAndNonAggr(fieldVals)
    val method = if (first) ".findOne" else ".findAll"
    // Coverts set of pairs to map of lists.
    val joinMap = s.joins.groupBy(_._1).map { case (k,v) => (k, v.map(_._2)) }
    val qStr = importModels(joinMap, Set(s.source)) <<
      createAssociations(joinMap) <<
      constructFieldDecls(s, fieldVals ++ s.aggrs)
    val (aggrP, nonAggrP) = s.preds partition { _.hasAggregate(s.fields) }
    val q = (Str(s.source) << method << "({\n" <<
      (
        Seq(
          constructIncludes(s.source, joinMap),
          constructAttributes(s),
          constructFilter(s, nonAggrP),
          constructFilter(s, aggrP, having = true),
          constructGroupBy(s.getNonConstantGroupingFields),
          constructOrderBy(s.orders, s.fields),
          if (offset >= 0) s"offset: $offset" else "",
          limit match {
            case None        => ""
            case Some(limit) => s"limit: $limit"
          }
        ) filter (x => x match {
          case "" => false
          case _  => true
        }) mkString(",\n")
      ) << "\n})").!
    qStr >> QueryStr(Some("ret" + s.numGen.next()), Some(q))
  }

  override def unionQueries(s1: State, s2: State): State =
    throw new UnsupportedException("unions are not supported")

  override def intersectQueries(s1: State, s2: State): State =
    throw new UnsupportedException("intersections are not supported")
}
