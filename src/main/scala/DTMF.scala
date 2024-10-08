package Audio

object DTMF {
  val col1 = 1209
  val col2 = 1336
  val col3 = 1477
  val col4 = 1633

  val row1 = 697
  val row2 = 779
  val row3 = 852
  val row4 = 941

  case class Key(char: Char, freqs: (Int, Int))

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

  val kDt = Key('T', (350, 440))

  val kStar = Key('*', (row4, col1))
  val k0 = Key('0', (row4, col2))
  val kHash = Key('#', (row4, col3))
  val kD = Key('D', (row4, col4))

  val keys = Seq(k1, k2, k3, kA, k4, k5, k6, kB, k7, k8, k9, kC, kStar, k0, kHash, kD, kDt)
  val keyMap: Map[Char, Key] = keys.map(k => (k.char, k)).toMap

  val defaultLengthMs = 250
  val defaultDigitGapMs = 150
}

case class DTMF(as: AudioSynth, lengthMs: Int = defaultLengthMs, digitGapMs: Int = defaultDigitGapMs) {
  import DTMF._
  def play(digits : String) = {
    digits.foreach( _ match {
      case ' ' => Thread.sleep(lengthMs + digitGapMs)
      case 'T' => as.play(mkTone(keyMap('T'), lengthMs * 10)) 
      case c =>
        val key = keyMap(c)
        as.play(mkTone(key))
        Thread.sleep(digitGapMs)
      }
    )
  }

  def mkTone(key: Key, lenMs: Int = lengthMs) = {
    val (f1, f2) = key.freqs
    val tone1 = as.createSineWaveBuffer(f1, lenMs).toSeq
    val tone2 = as.createSineWaveBuffer(f2, lenMs).toSeq
    val tone = tone1.zipAll(tone2, 0:Byte, 0:Byte).map { case (w1, w2) =>
      ((w1.toDouble + w2.toDouble) / 2).toByte
    }.toArray
    println(s"key: $key, ${tone1.length}, ${tone2.length}, ${tone.length}")
    tone
  }
}
