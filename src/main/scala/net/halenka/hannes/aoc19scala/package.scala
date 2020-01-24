package net.halenka.hannes

import net.halenka.hannes.aoc19scala.validation._

import scala.io.Source
import scala.util.{Failure, Success, Try, Using}

package object aoc19scala {
  type Result[+B] = Either[RuntimeError, B]

  /** Provides the content of a text file as a sequence of lines.
   *
   * @throws IllegalArgumentException if `resource` is blank
   */
  def loadTextFileResource(resource: String): Try[Vector[String]] = {
    val _resource = resource.requireNonBlank("`resource` must not be blank.")

    Using(Source.fromResource(_resource)) { r => r.getLines().toVector }
  }

  /**
   * @throws IllegalArgumentException if `resource` is blank.
   */
  def loadResourceAsIntSeq(resource: String): Try[IndexedSeq[Int]] = {
    val _resource = resource.requireNonBlank("`resource` must not be blank.")

    loadTextFileResource(_resource) match {
      case Success(lines) => Try(
        if (lines.nonEmpty) {
          lines.head.split(',').map(_.toInt).toIndexedSeq
        } else {
          IndexedSeq()
        }
      )
      case Failure(ex) => Failure(ex)
    }
  }

  implicit class IntOps(value: Int) {
    def toIndexedSeq: IndexedSeq[Int] = String.valueOf(value).map(_.toInt)
  }

  abstract class RuntimeError(private val _message: String = null) {
    def message: Option[String] = Option(_message)

    override def toString: String = s"${this.getClass.getSimpleName}(<${message.orNull}>)"
  }

}