package gr.dmst.aueb.cynthia


import java.io.File


case class Crash(desc: String, target: (String, String))
case class QueryResult(queryId: Int, out: String, backend: String)
case class GroupRes(
  crashes: Map[Crash, Seq[Int]] = Map(),
  queryRes: Map[QueryResult, Seq[String]] = Map()
) {
  def addCrash(crash: Crash, queryId: Int) =
    crashes get crash match {
      case None => GroupRes(
        crashes + (crash -> List(queryId)),
        queryRes)
      case Some(queries) => GroupRes(
        crashes + (crash -> (queries :+ queryId)),
        queryRes)
    }

  def addQueryRes(queryOut: QueryResult, orm: String) =
    queryRes get queryOut match {
      case None => GroupRes(
        crashes,
        queryRes + (queryOut -> List(orm)))
      case Some(orms) => GroupRes(
        crashes,
        queryRes + (queryOut -> (orms :+ orm)))
    }
}


object GroupUtil {
  val execPattern = ".*(Error|Exception):[^:\\n]*".r

  def examineContent(out: String, orm: String, backend: String,
                     queryId: Int, res: GroupRes) =
    execPattern.findFirstIn(out) match {
      case None    => res.addQueryRes(QueryResult(queryId, out, backend), orm)
      case Some(p) => res.addCrash(Crash(p, (orm, backend)), queryId)
    }

  def examineMismatch(dir: String, res: GroupRes) =
    Utils.listFiles(dir, ext = ".out") match {
      case None          => res
      case Some(outputs) => (outputs.foldLeft(Map[String, List[String]]()) { (acc, x) => {
        val Array(orm, backend) = new File(x).getName.replace(".out", "") split '_'
        acc get backend match {
          case None       => acc + (backend -> List(orm))
          case Some(orms) => acc + (backend -> (orm :: orms))
        }
      } }).foldLeft(res) { case (acc, (k, v)) =>
        v.foldLeft(res)  { case (res, orm) =>
          examineContent(
            Utils.readFromFile(Utils.joinPaths(List(dir, k + "_" + orm + ".out"))),
            orm,
            k,
            new File(dir).getName.toInt,
            res
          )
        }
      }
    }

  def apply(schema: String): Option[GroupRes] =
    Utils.listFiles(
      Utils.joinPaths(List(Utils.getReportDir, schema, "mismatches")),
      ext = ""
    ) match {
      case None          => None
      case Some(queries) =>
        Some(queries.foldLeft(GroupRes()) { (acc, x) => examineMismatch(x, acc) })
    }
}
