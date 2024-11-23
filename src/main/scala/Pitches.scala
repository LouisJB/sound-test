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

trait NoteDuration {
  def beats: Double
}
case object Whole extends NoteDuration { val beats = 4 }
case object Semi extends NoteDuration { val beats = 2 }
case object Quarter extends NoteDuration { val beats = 1 }
case object Eigth extends NoteDuration { val beats = 0.5 }
case object Sixteenth extends NoteDuration { val beats = 0.25 }

case class Durations(bpm: Double) {
  // take base BPM as the stantard quarter note (quaver)
  val beatMs = 60 * 1000 / bpm

  val whole = length(Whole)
  val semi = length(Semi)
  val quarter = length(Quarter)
  val eigth = length(Eigth)
  val sixteeth = length(Sixteenth)

  def length(duration: NoteDuration) =
    beatMs * duration.beats
}

trait Notes
case class Note(freq: Double, durMs: Double) extends Notes
case class Rest(durMs: Double) extends Notes
