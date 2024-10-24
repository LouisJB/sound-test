package Audio

object Pitches {
  import Math._
  case class Note(freq: Int)
  val A1 = Note(440)

  val semitoneRatio = pow(2, 1.0/12)
  val wholetomeRatio = pow(2, 1.0/6)
}
import Pitches._
trait Scale {
  def freq(note: Int): Int
}
case class ChromaticScale(baseFreq: Int = A1.freq) extends Scale {
  import Math._
  val ratio = semitoneRatio
  def freq(note: Int): Int =
    (baseFreq * pow(ratio, note-1)).toInt
}
