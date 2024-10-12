package Audio

case class DTMF(as: AudioSynth, lengthMs: Int = defaultLengthMs, digitGapMs: Int = defaultDigitGapMs) {
  import DTMF._
  println("DTMF dialertron v0.1")
  def play(digits : String) = {
    println(s"Playing autodialing at rate; toneLenghMs: $lengthMs, toneGapMs: $digitGapMs")
    digits.foreach( _ match {
      case ' ' => as.silence(lengthMs + digitGapMs)
      case 'S' => as.silence(4000)
      case 'R' => as.play(mkTones(keyMap('R'), 2000)) 
      case 'T' => as.play(mkTones(keyMap('T'), lengthMs * 10)) 
      case c =>
        val key = keyMap(c)
        as.play(mkTones(key))
        as.silence(digitGapMs)
      }
    )
  }

  def mkTones(key: Key, lenMs: Int = lengthMs) = {
    val (f1, f2) = key.freqs 
    val tone1 = as.mkSineWaveBuffer(f1, lenMs).toSeq
    val tone2 = as.mkSineWaveBuffer(f2, lenMs).toSeq
    val tones = tone1.zipAll(tone2, 0:Byte, 0:Byte).map { case (w1, w2) =>
      ((w1.toDouble + w2.toDouble) / 2).toByte
    }.toArray
    println(s"key: $key, ${tone1.length}, ${tone2.length}, ${tones.length}")
    tones
  }
}

object DTMF {
  // col and row frequencies
  val col1 = 1209
  val col2 = 1336
  val col3 = 1477
  val col4 = 1633

  val row1 = 697
  val row2 = 779
  val row3 = 852
  val row4 = 941

  case class Key(char: Char, freqs: (Int, Int)) {
    override def toString = s"$char : ${freqs._1}Hz, ${freqs._2}Hz"
  }

  val k1 = Key('1', (row1, col1))
  val k2 = Key('2', (row1, col2))
  val k3 = Key('3', (row1, col3))
  val kA = Key('A', (row1, col4))

  val k4 = Key('4', (row2, col1))
  val k5 = Key('5', (row2, col2))
  val k6 = Key('6', (row2, col3))
  val kB = Key('B', (row2, col4))
  
  val k7 = Key('7', (row3, col1))
  val k8 = Key('8', (row3, col2))
  val k9 = Key('9', (row3, col3))
  val kC = Key('C', (row3, col4))

  val kStar = Key('*', (row4, col1))
  val k0 = Key('0', (row4, col2))
  val kHash = Key('#', (row4, col3))
  val kD = Key('D', (row4, col4))

  val kDt = Key('T', (350, 440))
  val kDr = Key('R', (440, 480))

  val keys = Seq(k1, k2, k3, kA, k4, k5, k6, kB, k7, k8, k9, kC, kStar, k0, kHash, kD, kDt, kDr)
  val keyMap: Map[Char, Key] = keys.map(k => (k.char, k)).toMap

  val defaultLengthMs = 250
  val defaultDigitGapMs = 150  
}

object DTMFDemo {
  val defaultSampleRate = 48000
  val defaintBitDepth = 8
  def main(args: Array[String]): Unit = {
    AudioSynth.withAudioSynth(defaultSampleRate, defaintBitDepth) { audioSynth =>
      val dtmf = DTMF(audioSynth)
      if (args.length == 0) {
        dtmf.play("T  001 718 8675309 # RSRSR")
        audioSynth.silence(1000)
        (1 to 2).foreach { _ =>
          audioSynth.blip(1500, 1900, 10, 2000)
          audioSynth.silence(4000)
        }
      }
      else if (args.contains("-i")) {
        while (true) {
          println("type dial string and press enter (/ex to quit)")
          val text = Console.in.readLine()
          if (text == "/ex") {
            println("All done, 73")
            System.exit(0)
          }
          dtmf.play(text)
          println("dialed: " + text)
          audioSynth.silence(500)
        }
      }
      else
        dtmf.play(args(0))
    }
  }
}
