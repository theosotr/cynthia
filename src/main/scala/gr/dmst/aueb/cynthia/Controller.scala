package gr.dmst.aueb.cynthia

import java.io.File
import java.nio.file.{Paths, Files}
import scala.concurrent.{Await, Future, TimeoutException}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.{Try, Success, Failure}

import gr.dmst.aueb.cynthia.gen.SchemaGenerator
import gr.dmst.aueb.cynthia.translators.SchemaTranslator


object Controller {
  def _run(options: Options, s: Schema, x: TestRunner => Unit) =
    TestRunnerCreator(options, s) match {
      case Success(testRunner) => x(testRunner)
      case Failure(e)          => println(e.getMessage)
    }

  def apply(options: Options) = {
    Utils.setWorkDir()
    val f =
      options.mode match {
        case Some("test") =>
          List.range(0, options.schemas) map { _ => SchemaGenerator() } map { s => Future {
            Utils.writeToFile(s.getSchemaPath, SchemaTranslator(s))
            _run(options, s, {_.start})
          }}
        case Some("generate") =>
          List.range(0, options.schemas) map { _ => SchemaGenerator() } map { s => Future {
            Utils.writeToFile(s.getSchemaPath, SchemaTranslator(s))
            _run(options, s, {_.generate})
          }}
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
            Utils.copyFile("/tmp/cynthia_db", Utils.joinPaths(List(Utils.getSchemaDir, dst)))
            val s = Schema(dst, Map())
            _run(options, s, {_.start})
          }}
        case Some("replay") =>
          options.schema match {
            case Some(x) => {
              List { Future {
                val s = Schema(x, Map())
                _run(options, s, {_.start})
              }}
            }
            case None => {

            }
            Utils.getListOfFiles(
              Utils.joinPaths(List(options.dotCynthia, Utils.schemaDir))
            ) filter (!_.endsWith(".sql")) map (_.split('/').last) map { s =>
                Future {
                  val schema = Schema(s, Map())
                  _run(options, schema, {_.start})
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
