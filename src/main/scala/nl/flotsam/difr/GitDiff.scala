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

case class GitDiff(cmd: String, op: FileOperation, details: Option[GitDiffDetails])

case class GitDiffDetails(oldFile: String, newFile: String, chunks: List[ChangeChunk])
  def gitDiff: Parser[GitDiff] = diffHeader ~ fileOperation ~ (gitDiffDetails | gitDiffDetailsMissing) ^^ {
    case files ~ op ~ details => GitDiff(files, op, details)
  def gitDiffDetails: Parser[Option[GitDiffDetails]] = oldFile ~ newFile ~ diffChunks ^^ {
    case of ~ nf ~ chunks => Some(GitDiffDetails(of, nf, chunks))
  }

  def gitDiffDetailsMissing: Parser[Option[GitDiffDetails]] =
    """Binary[^\n]*\n""".r ^^ {
      case _ => None
    }
