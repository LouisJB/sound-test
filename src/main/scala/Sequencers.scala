
package Audio

case class Sequencers(as: AudioSynth) {
  import as._

  def playSeq(scale: Scale, noteSeq: Array[Int], durMs: Int, gapMs: Int = 0) =
    noteSeq.foreach(nn => { sine(scale.freq(nn).toInt, durMs); silence(gapMs) })

  def playSeqOpt(scale: Scale, noteSeq: Array[Option[Int]], durMs: Int, gapMs: Int = 0) =
    noteSeq.foreach( _ match {
      case Some(nn) =>
        sine(scale.freq(nn).toInt, durMs)
      case None =>
        silence(durMs)
      silence(gapMs)
  })

  def playSeqWs(noteSeq: Seq[Notes], playF: (Double, Double) => Array[Byte]) =
    noteSeq.foreach( _ match {
      case Note(f, durMs) =>
        play(playF(f, durMs))
      case Rest(durMs) =>
        silence(durMs.toInt)
  })

  def playSeq(noteSeq: Seq[Notes], playF: (Double, Double) => Unit) =
    noteSeq.foreach( _ match {
      case Note(f, durMs) =>
        playF(f, durMs)
      case Rest(durMs) =>
        silence(durMs.toInt)
  })

  // sequence the notes and rests into a sequence of wave clips
  def seqMap(noteSeq: Seq[Notes], playF: (Double, Double) => Array[Double]): Seq[Array[Double]] =
    noteSeq.map( _ match {
      case Note(f, durMs) =>
        playF(f, durMs)
      case Rest(durMs) =>
        ws.mkSilence(durMs.toInt)
  })
}
