/*  Title:      Pure/General/mercurial.scala
    Author:     Makarius

Support for Mercurial repositories, with local or remote repository clone
and working directory (via ssh connection).
*/

package isabelle

import java.io.{File => JFile}

import scala.annotation.tailrec
import scala.collection.mutable


object Mercurial
{
  def is_repository(path: Path): Boolean = false

  def check_files(source_files: List[Path]): (List[Path], List[Path]) =
    (List(), List())
}
