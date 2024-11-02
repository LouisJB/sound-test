package Audio

// a simple tone partial with volume
case class Tone(waveBuffers: Array[Byte], volume: Double)

// basic tone mixer allowing for per tone volume
object SimpleMixer {
  def mix(tones: Array[Tone]): Array[Byte] = {
    def scale(tone: Tone) =
      tone.waveBuffers.map(_.toDouble * tone.volume)
    tones.reduce { case (a, b) =>
      Tone(scale(a).zipAll(scale(b), 0.0, 0.0).map { case (v1, v2) => ((v1 + v2)/2.0).toByte }, 1.0)
    }.waveBuffers
  }
}