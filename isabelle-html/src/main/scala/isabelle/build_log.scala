/*  Title:      Pure/Admin/build_log.scala
    Author:     Makarius

Management of build log files and database storage.
*/

package isabelle

import java.io.{File => JFile}
import java.time.format.{DateTimeFormatter, DateTimeParseException}
import java.util.Locale


import scala.collection.immutable.SortedMap
import scala.collection.mutable
import scala.util.matching.Regex


object Build_Log
{
  def uncompress_errors(bytes: Bytes, cache: XZ.Cache): List[String] = ???

  def compress_errors(errors: List[String], cache: XZ.Cache): Bytes = ???

  def print_date(start_date: Date): String = "print_date"


  class Log_File private(val name: String, val lines: List[String]) {
    def parse_session_info(
            command_timings: Boolean = false,
            theory_timings: Boolean = false,
            ml_statistics: Boolean = false,
            task_statistics: Boolean = false): Session_Info = ???
  }

  case class Session_Info(
      session_timing: Properties.T,
      command_timings: List[Properties.T],
      theory_timings: List[Properties.T],
      ml_statistics: List[Properties.T],
      task_statistics: List[Properties.T],
      errors: List[String]) {
    def error(value: String): Session_Info = ???

  }

  private val empty: Log_File = Log_File("DUMMY", List())

  def Log_File(name: String, lines: List[String]): Log_File = empty

  object Settings {
    def show(): String = ""

  }

}
