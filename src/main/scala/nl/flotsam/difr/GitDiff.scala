/*
 * Difr
 * Copyright (C) 2013  Wilfred Springer
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 */

package nl.flotsam.difr

import util.parsing.combinator._
import java.io.Reader

case class GitLog(commitHash: String, author: String, date: String, comment: String, diffs: List[GitDiff])

case class GitDiff(cmd: String, op: FileOperation, details: Option[GitDiffDetails])

case class GitDiffDetails(oldFile: String, newFile: String, chunks: List[ChangeChunk])

sealed trait FileOperation

case class NewFile(mode: Int) extends FileOperation

case class DeletedFile(mode: Int) extends FileOperation

case object UpdatedFile extends FileOperation

sealed trait LineChange {
  def line: String
}

case class ContextLine(line: String) extends LineChange

case class LineRemoved(line: String) extends LineChange

case class LineAdded(line: String) extends LineChange

case class RangeInformation(oldOffset: Int, oldLength: Int, newOffset: Int, newLength: Int)

case class ChangeChunk(rangeInformation: RangeInformation, changeLines: List[LineChange])

/**
 * Git diff parser, originally based on some code found on github, now butchered beyond recognition. (Solving a couple
 * of bugs, including the "No newline at end of file" input, the way it deals with filenames, restrictions on number of
 * characters for hashes, etc.)
 */
object GitDiffParser extends RegexParsers {

  override def skipWhitespace = false

  def gitLogs = rep1(gitLog <~ opt(newline))

  def gitLog = commitHash ~ merge ~ author ~ date ~ comment ~ allDiffs ^^ {
    case h ~ m ~ a ~ d ~ c ~ diffs => GitLog(h, a, d, c, diffs)
  }

  def commitHash: Parser[String] = "commit " ~> anythingWithoutNewLine <~ newline

  def merge: Parser[Option[String]] = opt("Merge: " ~> anythingWithoutNewLine <~ newline)

  def author: Parser[String] = "Author: " ~> anythingWithoutNewLine <~ newline

  def date: Parser[String] = "Date:   " ~> anythingWithoutNewLine <~ newline

  def comment: Parser[String] = newline ~> rep1(commentLine <~ newline) <~ newline ^^ {
    case commentLines => commentLines.mkString("\n")
  }

  def commentLine: Parser[String] = "    " ~> anythingWithoutNewLine

  def allDiffs: Parser[List[GitDiff]] = rep(gitDiff)

  def gitDiff: Parser[GitDiff] = diffHeader ~ fileOperation ~ (gitDiffDetails | gitDiffDetailsMissing) ^^ {
    case files ~ op ~ details => GitDiff(files, op, details)
  }

  def gitDiffDetails: Parser[Option[GitDiffDetails]] = oldFile ~ newFile ~ diffChunks ^^ {
    case of ~ nf ~ chunks => Some(GitDiffDetails(of, nf, chunks))
  }

  def gitDiffDetailsMissing: Parser[Option[GitDiffDetails]] =
    ("""Binary[^\n]*\n""".r | newline) ^^ {
      case _ => None
    }

  def diffHeader: Parser[String] =
    """diff --git[^\n]*""".r <~ newline

  def fileOperation: Parser[FileOperation] =
    opt(deletedFileMode | newFileMode) <~ index ^^ {
      _ getOrElse UpdatedFile
    }

  def index: Parser[Any] = ("index " ~ hash ~ ".." ~ hash) ~> opt(" " ~> mode) <~ newline

  def deletedFileMode: Parser[DeletedFile] = "deleted file mode " ~> mode <~ newline ^^ {
    m => DeletedFile(m)
  }

  def newFileMode: Parser[NewFile] = "new file mode " ~> mode <~ newline ^^ {
    m => NewFile(m)
  }

  def hash: Parser[String] = """[0-9a-f]{7,8}""".r

  def mode: Parser[Int] = """\d{6}""".r ^^ {
    _.toInt
  }

  def diffChunks: Parser[List[ChangeChunk]] = rep1(changeChunk)

  def oldFile: Parser[String] = "--- " ~> anythingWithoutNewLine <~ newline ^^ {
    s => s.dropWhile(_ != '/').drop(1)
  }

  def newFile: Parser[String] = "+++ " ~> anythingWithoutNewLine <~ newline ^^ {
    s => s.dropWhile(_ != '/').drop(1)
  }

  def filename: Parser[String] = """[\S]+""".r

  def changeChunk: Parser[ChangeChunk] = rangeInformation ~ opt(contextLine) ~ (opt(newline) ~> rep1(lineChange)) ^^ {
    case ri ~ opCtx ~ lines => ChangeChunk(ri, opCtx map (_ :: lines) getOrElse (lines))
  }

  def rangeInformation: Parser[RangeInformation] =
    ("@@ " ~> "-" ~> number) ~ opt("," ~> number) ~ (" +" ~> number) ~ opt("," ~> number) <~ " @@" ^^ {
      case a ~ b ~ c ~ d => RangeInformation(a, b getOrElse 0, c, d getOrElse 0)
    }

  def lineChange: Parser[LineChange] = (contextLine | addedLine | deletedLine) <~ opt("\\ No newline at end of file" ~ newline)

  def contextLine: Parser[ContextLine] = " " ~> """.*""".r <~ newline ^^ {
    l => ContextLine(l)
  }

  def addedLine: Parser[LineAdded] = "+" ~> """.*""".r <~ newline ^^ {
    l => LineAdded(l)
  }

  def deletedLine: Parser[LineRemoved] = "-" ~> """.*""".r <~ newline ^^ {
    l => LineRemoved(l)
  }

  def newline: Parser[String] = """\n""".r

  def number: Parser[Int] = """\d+""".r ^^ {
    _.toInt
  }

  def anythingWithoutNewLine: Parser[String] = """[^\n]*""".r

  def parse(str: String) = parseAll(allDiffs, str)

  def asGitLogs(reader: Reader): List[GitLog] = parseAll(gitLogs, reader) match {
    case Success(s, _) => s
    case NoSuccess(msg, next) => sys.error(msg + " at " + next.pos)
  }
}