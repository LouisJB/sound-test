package Audio

case class Morse(as: AudioSynth, pitch: Int = defaultPitch, wpm: Int = defaultWpm, maybeWpmFarns: Option[Int] = None) {
  import Morse._
  import Math._
  val wpmFarns = maybeWpmFarns.getOrElse(wpm)
  println(s"Morser v0.1 : Initialized - pitchFreq: $pitch Hz, speed WPM: $wpm, farnsworth Speed WPM: $wpmFarns")
  val tMs = 60000/(50 * wpm)
  val tFarnsMs = ((60000 / wpmFarns) - (31 * tMs)) / 19
  val tDitMs = 1 * tMs
  val tDahMs = 3 * tMs
  val tGapMs = 1 * tMs
  val tLetterSpaceMs = 3 * max(tMs, tFarnsMs)
  val tWordSpaceMs = 7 * max(tMs, tFarnsMs)

  println(s"Time Unit: $tMs ms, Time Unit Farnsworth: $tFarnsMs ms\n")

  def play(text: String, echo: Boolean = true) = {
    if (echo)
      println(s"Morse: Playing text string:\n$text")

    val morse = text.map { _ match {
      case c if c != ' ' =>
        print(c)
        val digits = map(c)
        digits.foreach { d => d match { 
          case '-' => 
            as.sine(pitch, tDahMs)
          case '.' =>
            as.sine(pitch, tDitMs)
          }
          as.silence(tGapMs)
        }
        as.silence(tLetterSpaceMs)
        digits + " "
      case ' ' =>
        print(' ')
        as.silence(tWordSpaceMs)
        " / "
      }
    }.flatten
    println
    println(morse.mkString)
  }

  def replMode(): Unit = {
    println("Repl mode, please type your words and enter to play, /ex to exit")
    while (true) {
      val text = Console.in.readLine()
      if (text == "/ex") {
        println("All done, 73")
        System.exit(0)
      }
      play(text, false)
    }
  }
}

object Morse {
  private val defaultPitch = 700 // Hz
  val defaultWpm = 10

  val dit = '.'
  val dah = '-'

  val a = ".-"
  val b = "-..."
  val c = "-.-."
  val d = "-.."
  val e = "."
  val f = "..-."
  val g = "--."
  val h = "...."
  val i = ".."
  val j = ""
  val k = "-.-"
  val l = ".-.."
  val m = "--"
  val n = "-."
  val o = "---"
  val p = ".--."
  val q = "--.-"
  val r = ".-."
  val s = "..."
  val t = "-"
  val u = ".._"
  val v = "..._"
  val w = ".--"
  val x = "-..-"
  val y = "-.--"
  val z = "--.."

  val one   = ".----"
  val two   = "..---"
  val three = "...--"
  val four  = "....-"
  val five  = "....."
  val six   = "-...."
  val seven = "--..."
  val eight = "---.."
  val nine  = "----."
  val zero  = "-----"

  val period = ".-.-.-"

  val map = Map(
    'a' -> a,
    'b' -> b,
    'c' -> c,
    'd' -> d,
    'e' -> e,
    'f' -> f,
    'g' -> g,
    'h' -> h,
    'i' -> i,
    'j' -> j,
    'k' -> k,
    'l' -> l,
    'm' -> m,
    'n' -> n,
    'o' -> o,
    'p' -> p,
    'q' -> q,
    'r' -> r,
    's' -> s,
    't' -> t,
    'u' -> u,
    'v' -> v,
    'w' -> w,
    'x' -> x,
    'y' -> y,
    'z' -> z,

    '1' -> one,
    '2' -> two,
    '3' -> three,
    '4' -> four,
    '5' -> five,
    '6' -> six,
    '7' -> seven,
    '8' -> eight,
    '9' -> nine,
    '0' -> zero,

    '.' -> period
  )
}

object MorseDemo {
  val defaultSampleRate = 48000
  val defaintBitDepth = 8
  def main(args: Array[String]): Unit = {
    AudioSynth.withAudioSynth(defaultSampleRate, defaintBitDepth) { audioSynth =>
      val morse = Morse(audioSynth, wpm = 15, maybeWpmFarns = Some(10))
      if (args.length == 0)
        morse.play("cq kd2yck pse k.")
      else if (args.length == 1 && args(0) == "-r")
        morse.replMode()
      else
        morse.play(args(0))
    }
    println("All done, 73!")
  }
}
