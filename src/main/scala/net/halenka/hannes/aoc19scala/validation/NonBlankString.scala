package net.halenka.hannes.aoc19scala.validation

import org.apache.commons.lang3.StringUtils

import scala.collection.StringOps

/** A container representing an non-blank _string value. */
final class NonBlankString private(private val string: String) extends SimpleValueContainer[String](string) {
  assert(StringUtils.isNotBlank(string))
}

object NonBlankString {
  /** Create a new container instance.
   *
   * @param string A non-blank _string value
   * @throws IllegalArgumentException if `string` is <null> or blank.
   */
  def apply(string: String): NonBlankString = {
    if (StringUtils.isBlank(string)) throw new IllegalArgumentException("`string` must not be <null> or blank.")

    new NonBlankString(string)
  }

  /** Implicit conversion from `NonBlankString` to `String`. */
  implicit def nonBlankString2String(nbs: NonBlankString): String = nbs.value

  /** Implicit conversion from `NonBlankString` to `StringOps`. */
  implicit def nonBlankString2StringOps(nbs: NonBlankString): StringOps = new StringOps(nbs.value)
}