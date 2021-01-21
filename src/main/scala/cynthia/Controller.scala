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
import scala.concurrent.{Await, Future, TimeoutException}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.{Try, Success, Failure}

import com.typesafe.scalalogging.Logger
import me.tongfei.progressbar.ProgressBar;
import spray.json._

import cynthia.lang.Schema
import cynthia.lang.ModelJsonProtocol._
import cynthia.targets.{
  DBSetup,
  Postgres,
  MySQL,
  MSSQL,
  TestRunnerCreator,
  TestRunner
}
import cynthia.gen.SchemaGenerator
import cynthia.translators.SchemaTranslator
import cynthia.utils.{Utils, RUtils}

object Controller {
  def _run(
      options: Options,
      s: Schema,
      pBar: ProgressBar,
      logger: Logger,
      x: TestRunner => Unit
  ) =
    TestRunnerCreator(logger)(options, s, pBar) match {
      case Success(testRunner) => x(testRunner)
      case Failure(e) => {
        pBar.close()
        println(s"Cannot start testing session for ${s.name}: ${e.getMessage}")
      }
    }

  def createNSchemas(nSchemas: Int, nQueries: Int, prefix: String) =
    List.range(0, nSchemas) map (_ => SchemaGenerator()) map (s => {
      val schemaPath =
        Utils.joinPaths(List(Utils.getSchemaDir(), s"${s.name}.schema.json"))
      Utils.writeToFile(schemaPath, s.toJson.prettyPrint)
      (
        s,
        Utils.buildProgressBar(prefix + " " + s.name, Some(nQueries)),
        Logger("Session " + s.name)
      )
    })

  def apply(options: Options) = {
    Utils.setWorkDir()
    val f =
      options.mode match {
        case Some("test") =>
          createNSchemas(options.schemas, options.nuqueries, "Testing") map (
            out =>
              Future {
                RUtils.seed(Utils.encodeStr2Int(out._1.name))
                Utils.writeToFile(
                  out._1.getSchemaPath(),
                  SchemaTranslator(out._1)
                )
                _run(options, out._1, out._2, out._3, { _.start() })
              }
          )
        case Some("generate") =>
          createNSchemas(options.schemas, options.nuqueries, "Generating") map (
            out =>
              Future {
                RUtils.seed(Utils.encodeStr2Int(out._1.name))
                Utils.writeToFile(
                  out._1.getSchemaPath(),
                  SchemaTranslator(out._1)
                )
                _run(options, out._1, out._2, out._3, { _.generate() })
              }
          )
        case Some("run") =>
          List {
            Future {
              val sql = options.sql match {
                case Some(x) => x
                case None    => ""
              }
              val dst = Utils.basenameWithoutExtension(sql)
              // If options.sql and dst are the same file then the direct copy
              // will fail
              Utils.copyFile(sql, "/tmp/cynthia_db")
              Utils.copyFile(
                "/tmp/cynthia_db",
                Utils.joinPaths(List(Utils.getSchemaDir(), dst))
              )
              val s = Schema(dst, Map())
              _run(
                options,
                s,
                Utils.buildProgressBar("Running " + dst, None),
                Logger("Session " + s.name),
                { _.start() }
              )
            }
          }
        case Some("replay") =>
          options.schema match {
            case Some(x) => {
              List {
                Future {
                  val schemaPath = Utils.joinPaths(
                    List(Utils.getSchemaDir(), s"$x.schema.json")
                  )
                  if (new File(schemaPath).exists()) {
                    val s = Utils.loadSchema(schemaPath)
                    _run(
                      options,
                      s,
                      Utils.buildProgressBar("Replaying " + x, None),
                      Logger("Session " + s.name),
                      { _.start() }
                    )
                  } else {
                    println(
                      s"Cannot replay testing session ${x}: "
                        + s" The file ${schemaPath} not found."
                    )
                  }
                }
              }
            }
            case None =>
              {}
              val schemaFiles = Utils.getListOfFiles(
                Utils.joinPaths(List(options.dotCynthia, Utils.schemaDir))
              ) filter (x => !x.endsWith(".sql") && !x.endsWith(".json"))

              schemaFiles map (_.split('/').last) map (s =>
                (
                  s,
                  Utils.buildProgressBar("Replaying " + s, None),
                  Logger("Session " + s)
                )
              ) map (out =>
                Future {
                  val schemaPath = Utils.joinPaths(
                    List(Utils.getSchemaDir(), s"${out._1}.schema.json")
                  )
                  if (new File(schemaPath).exists()) {
                    val schema = Utils.loadSchema(schemaPath)
                    _run(options, schema, out._2, out._3, { _.start() })
                  } else {
                    out._2.setExtraMessage("Cannot replay, see cynthia.log.")
                    out._2.close()
                    out._3.error(
                      s"Cannot replay testing session: "
                        + s"File ${schemaPath} not found."
                    )
                  }
                }
              )
          }
        case Some("inspect") => {
          val sessionDir = List(options.dotCynthia, Utils.sessionDir)
          val sessions = options.schema match {
            case None    => Utils.listFiles(Utils.joinPaths(sessionDir), ext = "")
            case Some(s) => Some(Array(Utils.joinPaths(sessionDir :+ s)))
          }
          sessions match {
            case None => Nil
            case Some(sessions) =>
              List(Future.sequence(sessions.toList map { x =>
                new File(x).getName
              } map { x =>
                Future {
                  (x, Inspector(x, options.mismatches, options.dotCynthia))
                }
              }) map { res =>
                res map {
                  case (session, res) => {
                    println(
                      s"${Console.GREEN}Session: ${session}${Console.RESET}"
                    )
                    res match {
                      case None =>
                        println(s"No mismatches found for session '${session}'")
                      case Some(res) => {
                        InspectRes.printCrashes(res, startIdent = 2)
                        InspectRes.printMismatches(res, startIdent = 2)
                        println("==================================")
                      }
                    }
                  }
                }
              })
          }
        }
        case Some("clean") => {
          val f = Future {
            println("Cleaning working directory .cynthia...")
            Utils.deleteRecursively(new File(options.dotCynthia))
          }
          if (options.onlyWorkDir) f :: Nil
          else
            f :: (
              List(
                Postgres(options.dbUser, options.dbPass, "postgres"),
                MySQL(options.dbUser, options.dbPass, "sys"),
                MSSQL(options.dbUser, options.dbPass, "master")
              ) map (x =>
                Future {
                  println("Cleaning backend " + x.getName() + "...")
                  Try(DBSetup.clean(x)) match {
                    case Success(_) =>
                      println(s"Cleaned database ${x.getName()} successfully")
                    case Failure(e) => {
                      println(s"Unable to clean database ${x.getName()}...")
                    }
                  }
                }
              )
            )
        }
        case _ => ???
      }
    try {
      Await.result(
        Future.sequence(f) map { _ =>
          Thread.sleep(51)
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
