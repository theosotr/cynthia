/*
 * Copyright (c) 2020-2021 Thodoris Sotiropoulos, Stefanos Chaliasos
 *
 * This program is free software: you can redistribute it and/or modify  
 * it under the terms of the GNU General Public License as published by  
 * the Free Software Foundation, version 3.
 *
 * This program is distributed in the hope that it will be useful, but 
 * WITHOUT ANY WARRANTY; without even the implied warranty of 
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU 
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License 
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */

package cynthia

import java.io.File
import scala.io.Source

import cynthia.utils.Utils


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
  val quotedStrPattern = "[\"'`][a-z\\.]+[\"'`]"

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

  def apply(schema: String, mismatches: Seq[Int],
            workdir: String): Option[InspectRes] = {
    val dir = Utils.joinPaths(List(
      workdir, Utils.reportDir, schema))
    Utils.listFiles(dir, ext = "") match {
      case None          => None
      case Some(queries) => {
        val misQueries =
          // Find the queries where the output of differential testing
          // was a MISMATCH.
          queries filter(x => {
            val file = Utils.joinPaths(List(dir, x, "diff_test.out"))
            if (!Utils.exists(file)) false
            else Source.fromFile(file).mkString.equals("MISMATCH")
          })
        val q = mismatches match {
          case Seq() => misQueries
          case _     => misQueries map { _.toInt } filter { mismatches.contains(_) } map { _.toString }
        }
        Some(q.foldLeft(InspectRes()) { (acc, x) =>
          inspectMismatch(Utils.joinPaths(List(dir, x)), acc)
        })
      }
    }
  }
}
