package net.halenka.hannes.aoc19scala.validation

/** A container wrapping a `Seq` which is guaranteed not to be empty. */
final class NonEmptySeq[T] private(private val _value: Seq[T]) extends SimpleValueContainer[Seq[T]](_value) {
  assert(_value != null && _value.nonEmpty)
}

object NonEmptySeq {
  /** Create a new `NonEmptySeq` instance.
   *
   * @param seq The sequence to be wrapped by the container.
   * @return a `NonEmptySeq` wrapping the specified `Seq`
   * @throws IllegalArgumentException if `seq` is <null> or empty.
   */
  def apply[T](seq: Seq[T]): NonEmptySeq[T] = {
    if (seq == null || seq.isEmpty) throw new IllegalArgumentException("`seq` must not be <null> or empty.")

    new NonEmptySeq(seq)
  }

  implicit def nonEmptySeq2Seq[T](nes: NonEmptySeq[T]): Seq[T] = nes.value

  implicit class SeqOps[T](seq: Seq[T]) {
    def nes: NonEmptySeq[T] = NonEmptySeq(seq)
  }

}