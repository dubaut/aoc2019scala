package net.halenka.hannes.aoc19scala

import org.apache.commons.lang3.StringUtils

package object validation {

  abstract class SimpleValueContainer[T] protected(private val _value: T) {
    /** Returns the wrapped value. */
    final def value: T = _value

    override def toString: String = s"${this.getClass.getSimpleName}(${_value})"
  }

  implicit class StringValidator(string: String) {
    def requireNonBlank(message: String): NonBlankString = {
      if (StringUtils.isBlank(string)) throw new IllegalArgumentException(message)

      NonBlankString(string)
    }
  }

  implicit class SeqValidator[T](seq: Seq[T]) {
    def requireNonEmpty(message: String): NonEmptySeq[T] = {
      if (seq == null || seq.isEmpty) throw new IllegalArgumentException(message)

      NonEmptySeq(seq)
    }
  }

}