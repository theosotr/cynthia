package gr.dmst.aueb.cynthia

import java.io.File


case class Crash(desc: String, target: (String, String))
case class QueryResult(queryId: Int, out: String, backend: String)
case class InspectRes(
  crashes: Map[Crash, Seq[Int]] = Map(),
  queryRes: Map[QueryResult, Seq[String]] = Map()
) {
  def addCrash(crash: Crash, queryId: Int) =
    crashes get crash match {
      case None => InspectRes(
        crashes + (crash -> List(queryId)),
        queryRes)
      case Some(queries) => InspectRes(
        crashes + (crash -> (queries :+ queryId)),
        queryRes)
    }

  def addQueryRes(queryOut: QueryResult, orm: String) =
    queryRes get queryOut match {
      case None => InspectRes(
        crashes,
        queryRes + (queryOut -> List(orm)))
      case Some(orms) => InspectRes(
        crashes,
        queryRes + (queryOut -> (orms :+ orm)))
    }
}

case object InspectRes {
  def dump(str: String, ident: Int = 0) = {
    def dump_ident() =
      List.range(0, ident).foldLeft("") { (acc, x) => acc + " " }
    println(dump_ident + str)
  }

  def printMismatches(inspectRes: InspectRes, startIdent: Int) = {
    dump(s"${Console.YELLOW}Mismatches:${Console.RESET}", ident = startIdent)
    (inspectRes.queryRes.foldLeft(Map[(Int, String), Seq[Seq[String]]]()) { case (acc, (k, m)) =>
      acc get (k.queryId, k.backend) match {
        case None    => acc + ((k.queryId, k.backend) -> Seq(m))
        case Some(v) => acc + ((k.queryId, k.backend) -> (v :+ m))
      }
      }) foreach {
        case (_, Seq(_)) | (_, Seq()) => ()
        case ((query, backend), mis) => {
          dump(s"${Console.CYAN}* ${query.toString}[${backend}]: ${Console.RESET}", ident = startIdent + 1)
          mis foreach { x =>
            dump("- " + x.mkString(","), ident = startIdent + 3)
          }
      } }
  }

  def printCrashes(inspectRes: InspectRes, startIdent: Int) = {
    dump(s"${Console.RED}Crashes:${Console.RESET}", ident = startIdent)
    (inspectRes.crashes.foldLeft(Map[(String, String), Seq[(String, Seq[Int])]]()) { case (acc, (k, q)) =>
      acc get k.target match {
        case None    => acc + (k.target -> Seq((k.desc, q)))
        case Some(v) => acc + (k.target -> (v :+ (k.desc, q)))
      }
    }) foreach { case (k, v) =>
      dump(s"${Console.CYAN}* ${k._1}[${k._2}]:${Console.RESET}", ident = startIdent + 1)
      v foreach { case (desc, ids) => {
        dump("- " + desc + ": Queries(" + ids.mkString(",") + ")", ident = startIdent + 3)
      } }
    }
  }
}


object Inspector {
  val execPattern = ".*(Error|Exception):\\(?[^:\\n]*".r
  val nodePattern = "\\(node:[0-9]+\\)[ ]"
  val quotedStrPattern = "[\"'`][a-zA-Z\\.]+[\"'`]"

  def inspectContent(out: String, orm: String, backend: String,
                     queryId: Int, res: InspectRes) =
    execPattern.findFirstIn(out) match {
      case None    => res.addQueryRes(QueryResult(queryId, out, backend), orm)
      case Some(p) => res.addCrash(
        Crash(
          p.replaceAll(quotedStrPattern, "'x'").replaceAll(nodePattern, ""),
          (orm, backend)
        ),  queryId)
    }

  def inspectMismatch(dir: String, res: InspectRes) = {
    Utils.listFiles(dir, ext = ".out") match {
      case None          => res
      case Some(outputs) => (outputs.foldLeft(Map[String, List[String]]()) { (acc, x) => {
        val Array(orm, backend) = new File(x).getName.replace(".out", "") split '_'
        acc get backend match {
          case None       => acc + (backend -> List(orm))
          case Some(orms) => acc + (backend -> (orm :: orms))
        }
      } }).foldLeft(res) { case (acc, (k, v)) =>
        v.foldLeft(acc)  { case (res, orm) =>
          inspectContent(
            Utils.readFromFile(Utils.joinPaths(List(dir, orm + "_" + k + ".out"))),
            orm,
            k,
            new File(dir).getName.toInt,
            res
          )
        }
      }
    }
  }

  def apply(schema: String): Option[InspectRes] = {
    val dir = Utils.joinPaths(List(Utils.getReportDir, schema, "mismatches"))
    Utils.listFiles(dir, ext = "") match {
      case None          => None
      case Some(queries) => {
        Some(queries.foldLeft(InspectRes()) { (acc, x) =>
          inspectMismatch(Utils.joinPaths(List(dir, x)), acc)
        })
      }
    }
  }
}
