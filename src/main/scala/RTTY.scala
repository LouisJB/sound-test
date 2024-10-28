package Audio

import Math._

case class RTTY(audioSynth: AudioSynth,
                baudRate: Double = defaultBaudRate,
                spacePitch: Int = defaultSpacePitch,
                markPitch: Int = defaulMarkPitch,
                stopBits: StopBits = defaultStopBits) {
  import BaudotCodes._
  val spreadHz = abs(markPitch - spacePitch)
  println(s"RTTY v0.1 : Initialized - baudRate: $baudRate, spacePitch: $spacePitch, markPitch: $markPitch, stopBits: $stopBits, spreadHz: $spreadHz")

  val bitDurMs = (1000 / baudRate).toInt
  println(s"bit duration $bitDurMs ms")
  val noOfIdleSyncWords = 2
  val idleSyncWord = LTRS
  println(s"idle preamble: $noOfIdleSyncWords | $idleSyncWord")
  println()
  println("Ready |>")
  println()

  trait SymbolMode
  case object LtrsMode extends SymbolMode
  case object FigsMode extends SymbolMode
  private var currentMode: SymbolMode = LtrsMode
  private def setMode(newMode: SymbolMode) = {
    if (newMode != currentMode) {
      newMode match {
        case LtrsMode =>
          println("Changing mode to LTRS")
          play(LTRS)
        case FigsMode =>
          println("Changing mode to FIGS")
          play(FIGS)
      }
      currentMode = newMode
    }
  }
  def play(msg: String): Unit = {
    println(s"Message is : '$msg'")
    play(NULL)
    (1 to noOfIdleSyncWords).foreach( _ => play(idleSyncWord))
    msg.toUpperCase().map( c => // note rtty baudot is case insensitive
      if (c.isDigit) {
        setMode(FigsMode)
        play(codeMap(c.toString))
      }
      else if (c.isLetter || c.isSpaceChar) {
        setMode(LtrsMode)
        play(codeMap(c.toString))
      }
      else
        println(s"***** Unsupported character '$c' will be ignored *****")
    )
    play(CR) // in line mode for now so this is default eol
    println()
  }

  def play(bc: BaudotCode): Unit = {
    val timeMs = System.currentTimeMillis()
    print(s"$timeMs: output $bc |> ")
    audioSynth.sine(spacePitch, bitDurMs) // start bit is a space
    bc.code.foreach( c =>
      print(c)
      if (c.equals('-'))
        audioSynth.sine(spacePitch, bitDurMs)
      else
        audioSynth.sine(markPitch, bitDurMs) 
    )
    audioSynth.sine(markPitch, (stopBits.stopBit * bitDurMs).toInt) // stop bit is a mark
    println
  }
  def replMode() = { 
  }
}

trait StopBits { def stopBit: Double }
case object OneStopBit extends StopBits { def stopBit = 1.0 }
case object OneAndHalfStopBits extends StopBits { def stopBit = 1.5 }
case object TwoStopBits extends StopBits { def stopBit = 2.0 }

object RTTY {
  val defaultBaudRate = 45.45
  val defaultSpacePitch = 2125 // Hz
  val defaulMarkPitch = 2295
  val defaultStopBits = OneAndHalfStopBits

  def apply(audioSynth: AudioSynth,
            baudRate: Double,
            stopBits: StopBits,
            centerPitch: Int,
            offsetPitch: Int): RTTY = {
    val spacePitch = centerPitch - offsetPitch/2
    val markPitch = centerPitch + offsetPitch/2
    new RTTY(audioSynth, baudRate, spacePitch, markPitch, stopBits)
  }
}

object RTTYDemo {
  val defaultSampleRate = 48000
  val defaultBitDepth = 8
  def main(args: Array[String]): Unit = {
    val startMs = System.currentTimeMillis
    AudioSynth.withAudioSynth(defaultSampleRate, defaultBitDepth) { audioSynth =>
      val rtty = RTTY(audioSynth)
      if (args.length == 0) {
        rtty.play("abc def ghi jkl mno pqr stu vwx yz")
        rtty.play("hello this is a test")
        rtty.play("numbers follow 1234567890 1 2 3 and then text again")
        rtty.play("the quick brown fox jumps over the lazy dog")
      }
      else if (args.length == 1 && args(0) == "-r")
        rtty.replMode()
      else
        rtty.play(args(0))
    }
    val totalMs = System.currentTimeMillis - startMs
    println(s"Message tx duration was ${totalMs}ms")
    println()
    println("All done, 73!")
  }
}

case class BaudotCode(symbol: String, code: String) {
  override def toString = s"symbol: $symbol, code: $code"
}
object BaudotCodes {
  val NULL  = BaudotCode("<NULL>", "-----")
  val LF    = BaudotCode("<LF>", "---+-")
  val CR    = BaudotCode("<CR>", "-+---")

  val LTRS  = BaudotCode("<LTRS>", "+++++")
  val FIGS  = BaudotCode("<FIGS>", "++-++")

  val SPACE = BaudotCode(" ", "--+--")
  val A = BaudotCode("A", "++---")
  val B = BaudotCode("B", "+--++")
  val C = BaudotCode("C", "-+++-")
  val D = BaudotCode("D", "+--+-")
  val E = BaudotCode("E", "+----")
  val F = BaudotCode("F", "+-++-")
  val G = BaudotCode("G", "-+-++")
  val H = BaudotCode("H", "--+-+")
  val I = BaudotCode("I", "-++--")
  val J = BaudotCode("J", "++-+-")
  val K = BaudotCode("K", "++++-")
  val L = BaudotCode("L", "-+--+")
  val M = BaudotCode("M", "--+++")
  val N = BaudotCode("N", "--++-")
  val O = BaudotCode("O", "---++")
  val P = BaudotCode("P", "-++-+")
  val Q = BaudotCode("Q", "+++-+")
  val R = BaudotCode("R", "-+-+-")
  val S = BaudotCode("S", "+-+--")
  val T = BaudotCode("T", "----+")
  val U = BaudotCode("U", "+++--")
  val V = BaudotCode("V", "-++++")
  val W = BaudotCode("W", "++--+")
  val X = BaudotCode("X", "+-+++")
  val Y = BaudotCode("Y", "+-+-+")
  val Z = BaudotCode("Z", "+---+")

  val F1 = BaudotCode("1", "+++-+")
  val F2 = BaudotCode("2", "++--+")
  val F3 = BaudotCode("3", "+----")
  val F4 = BaudotCode("4", "+-+--")
  val F5 = BaudotCode("5", "----+")
  val F6 = BaudotCode("6", "+-+-+")
  val F7 = BaudotCode("7", "+++--")
  val F8 = BaudotCode("8", "-++--")
  val F9 = BaudotCode("9", "---++")
  val F0 = BaudotCode("0", "-++-+")

  val codeMap = {
    val letterCodes = Seq(
      A, B, C, D, E, F, G, H, I, J , K, L, M, N, O, P,
      Q, R, S, T, U, V, W, X, Y, Z,
      SPACE
    )
    val numberCodes = Seq(
     F1, F2, F3, F4, F5, F6, F7, F8, F9, F0
    )
    val controlCodes = Seq(
      NULL, LF, CR, LTRS, FIGS
    )
    def checkCodes(cs: Seq[BaudotCode]): Unit = {
      assert(cs.forall(_.code.size == 5))
      assert(cs.size == cs.map(_.code).toSet.size)
    }
    checkCodes(letterCodes)
    checkCodes(numberCodes)
    checkCodes(controlCodes)

    (letterCodes ++ numberCodes ++ controlCodes).map(ev => ev.symbol-> ev).toMap
  }
}
