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
import java.nio.file.{Paths, Files}
import scala.concurrent.{Await, Future, TimeoutException}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.{Try, Success, Failure}

import me.tongfei.progressbar.{ProgressBar, ProgressBarStyle, ProgressBarBuilder};

import cynthia.lang.Schema
import cynthia.targets.{DBSetup, Postgres, MySQL, TestRunnerCreator,
  TestRunner}
import cynthia.gen.SchemaGenerator
import cynthia.translators.SchemaTranslator
import cynthia.utils.Utils


object Controller {
  def _run(options: Options, s: Schema, pBar: Option[ProgressBar],
           x: TestRunner => Unit) =
    TestRunnerCreator(options, s, pBar) match {
      case Success(testRunner) => x(testRunner)
      case Failure(e)          => println(e.getMessage)
    }

  def createNSchemas(nSchemas: Int, nQueries: Int) =
    List.range(0, nSchemas) map(_ => SchemaGenerator()) map(s =>
        (s,
          new ProgressBarBuilder()
            .setTaskName("Session: " + s.name)
            .setInitialMax(nQueries)
            .setStyle(ProgressBarStyle.ASCII)
            .setUpdateIntervalMillis(50)
            .build
         )
    )

  def apply(options: Options) = {
    Utils.setWorkDir()
    val f =
      options.mode match {
        case Some("test") =>
          createNSchemas(options.schemas, options.nuqueries) map (out =>
              Future {
                Utils.writeToFile(out._1.getSchemaPath, SchemaTranslator(out._1))
                _run(options, out._1, Some(out._2), {_.start})
              })
        case Some("generate") =>
          createNSchemas(options.schemas, options.nuqueries) map(out =>
              Future {
                Utils.writeToFile(out._1.getSchemaPath, SchemaTranslator(out._1))
                _run(options, out._1, None, {_.generate})
              })
        case Some("run") =>
          List { Future {
            val sql = options.sql match {
              case Some(x) => x
              case None    => ""
            }
            val schema = options.schema match {
              case Some(x) => x
              case None    => ""
            }
            val dst = Utils.basenameWithoutExtension(sql)
            // If options.sql and dst are the same file then the direct copy
            // will fail
            Utils.copyFile(sql, "/tmp/cynthia_db")
            Utils.copyFile("/tmp/cynthia_db",
                           Utils.joinPaths(List(Utils.getSchemaDir, dst)))
            val s = Schema(dst, Map())
            _run(options, s, None, {_.start})
          }}
        case Some("replay") =>
          options.schema match {
            case Some(x) => {
              List { Future {
                val s = Schema(x, Map())
                _run(options, s, None, {_.start})
              }}
            }
            case None => {

            }
            Utils.getListOfFiles(
              Utils.joinPaths(List(options.dotCynthia, Utils.schemaDir))
            ) filter (!_.endsWith(".sql")) map (_.split('/').last) map { s =>
                Future {
                  val schema = Schema(s, Map())
                  _run(options, schema, None, {_.start})
              }}
          }
        case Some("inspect") => {
          val reportDir = List(options.dotCynthia, Utils.reportDir)
          val projects = options.schema match {
            case None    => Utils.listFiles(Utils.joinPaths(reportDir), ext = "")
            case Some(s) => Some(Array(Utils.joinPaths(reportDir :+ s)))
          }
          projects match {
            case None           => Nil
            case Some(projects) =>
              List(
                Future.sequence(
                  projects.toList map { x => new File(x).getName } map { x =>
                    Future { (x, Inspector(x, options.mismatches, options.dotCynthia)) }
                }) map { res => res map { case (project, res) => {
                    println(s"${Console.GREEN}Project: ${project}${Console.RESET}")
                    res match {
                      case None      => println(s"No mismatches found for project '${project}'")
                      case Some(res) => {
                        InspectRes.printCrashes(res, startIdent = 2)
                        InspectRes.printMismatches(res, startIdent = 2)
                        println("==================================")
                      }
                    }
                  } }
                })
          }
        }
        case Some("clean") => {
          val f = Future {
            println("Cleaning working directory .cynthia...")
            Utils.deleteRecursively(new File(".cynthia"))
          }
          if (options.onlyWorkDir) f :: Nil
          else f :: (
            List(
              Postgres(options.dbUser, options.dbPass, "postgres"),
              MySQL(options.dbUser, options.dbPass, "sys")
            ) map { x => Future {
              println("Cleaning backend " + x.getName + "...")
              DBSetup.clean(x)
              }
            })
        }
        case _ => ???
      }
    try {
      Await.result(
        Future.sequence(f) map { _ =>
          Thread.sleep(50)
          println(s"Command ${options.mode.get} finished successfully.")
        },
        options.timeout match {
          case None    => Duration.Inf
          case Some(t) => t seconds
        }
      )
    } catch {
      case e: TimeoutException => {
        println("Cynthia timed out")
        System.exit(0)
      }
    }
  }
}
