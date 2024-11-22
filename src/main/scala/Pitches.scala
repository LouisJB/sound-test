package Audio

object Pitches {
  import Math._
  case class NotePitch(freq: Double)
  val A1 = NotePitch(440.0)

  val semitoneRatio = pow(2, 1.0/12)
  val wholetomeRatio = pow(2, 1.0/6)
}
import Pitches._
trait Scale {
  def freq(note: Int): Double
}
case class ChromaticScale(baseFreq: Double = A1.freq) extends Scale {
  import Math._
  val ratio = semitoneRatio
  // have made this 1 based as tonic is note 1 in default scale
  def freq(note: Int): Double =
    (baseFreq * pow(ratio, note-1)).toInt
}

case class Durations(bpm: Double) {
  val beatMs = 60 * 1000 / bpm

  val whole = beatMs * 4.0
  val semi = beatMs * 2.0
  val quarter = beatMs
  val eigth = beatMs / 2.0
  val sixteeth = beatMs / 4.0
}

trait Notes
case class Note(freq: Double, durMs: Double) extends Notes
case class Rest(durMs: Double) extends Notes
