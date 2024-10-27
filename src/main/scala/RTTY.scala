package Audio

case class RTTY(audioSynth: AudioSynth,
                baudRate: Double = defaultBaudRate,
                spacePitch: Int = defaultSpacePitch,
                markPitch: Int = defaulMarkPitch,
                stopBits: StopBits = defaultStopBits) {
  import BaudotCodes._
  println(s"RTTY v0.1 : Initialized - spacePitch: $spacePitch, markPitch: $markPitch, stopBits: $stopBits")

  val bitDurMs = (1000 / baudRate).toInt
  def play(msg: String): Unit = {
    msg.map( c =>
      if (c.isLetter)
        play(codeMap(c.toString))
    )
  }

  def play(bc: BaudotCode): Unit = {
    audioSynth.sine(spacePitch, bitDurMs) // start bit is a space
    bc.code.foreach( c =>
      if (c.equals("-"))
        audioSynth.sine(spacePitch, bitDurMs)
      else
        audioSynth.sine(markPitch, bitDurMs)
      
      audioSynth.sine(markPitch, (stopBits.stopBit * bitDurMs).toInt) // stop bit is a mark
    )
  }
  def replMode() = { 
  }
}

trait StopBits { def stopBit: Double }
case object OneStopBit extends StopBits { def stopBit = 1.0 }
case object OneAndHalfStopBit extends StopBits { def stopBit = 1.5 }
case object TwoStopBit extends StopBits { def stopBit = 2.0 }

object RTTY {
  val defaultBaudRate = 45.45
  val defaultSpacePitch = 2125 // Hz
  val defaulMarkPitch = 2295
  val defaultStopBits = OneStopBit
}

object RTTYDemo {
  val defaultSampleRate = 48000
  val defaintBitDepth = 8
  def main(args: Array[String]): Unit = {
    AudioSynth.withAudioSynth(defaultSampleRate, defaintBitDepth) { audioSynth =>
      val rtty = RTTY(audioSynth)
      if (args.length == 0)
        rtty.play("ABCDE")
      else if (args.length == 1 && args(0) == "-r")
        rtty.replMode()
      else
        rtty.play(args(0))
    }
    println("All done, 73!")
  }
}

case class BaudotCode(symbol: String, code: String)
object BaudotCodes {
  val A = BaudotCode("A", "+----")
  val B = BaudotCode("B", "--++-")
  val C = BaudotCode("C", "+-++-")
  val D = BaudotCode("D", "++++-")
  val E = BaudotCode("E", "-+---")
  val F = BaudotCode("F", "-+++-")
  val G = BaudotCode("G", "-+-+-")
  val H = BaudotCode("G", "++-+-")

  val codes = Seq(A, B, C, D, E)
  val codeMap = codes.map(ev => ev.symbol-> ev).toMap
}
