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

  def play(text: String, showWordFirst: Boolean = true, echo: Boolean = true) = {
    if (showWordFirst)
      println(s"Morse: Playing text string:\n$text")

    val morse = text.map { _ match {
      case c if c != ' ' =>
        if (echo) print(c)
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
        if (echo) print(' ')
        as.silence(tWordSpaceMs)
        " / "
      case ex =>
        println(s"Unexpected char, ignoring, was $ex with char code " + ex.toByte.toShort)
        " ? "
      }
    }.flatten

    if (!echo) println
    if (!showWordFirst)
      println(text)

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

  def playWords(fileName: String) = {
    val wordFile = new java.io.File(fileName)
    println("Playing words from file " + wordFile.getAbsolutePath)
    if (wordFile.exists) {
      val rand = new scala.util.Random
      val words: Seq[String] = io.Source.fromFile(wordFile).getLines().toSeq
      while (true) {
        val idx = rand.nextInt(words.length)
        play(words(idx).stripLineEnd, showWordFirst = false, echo = false)
        as.silence(5000)
      }
    }
  }

  def showSymbols() = {
    println("Letters:")
    letters.foreach( (s, m) => println(s"$s = $m"))
    println()
    println("Numbers:")
    numbers.foreach( (s, m) => println(s"$s = $m"))
    println()
    println("Symbols:")
    symbols.foreach( (s, m) => println(s"$s = $m"))
  }
}

object Morse {
  private val defaultPitch = 700 // Hz
  private val defaultTrainingWordGapMs = 5000
  private val defaultWpm = 10

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
  val j = ".---"
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
  val u = "..-"
  val v = "...-"
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
  val comma = "--..--"
  val questionMark = "..--.."
  val appostraphe = ".---."
  val exclamation = "-.-.--"
  val slash = "-..-."
  val openparens = "-.--."
  val closeparens = "-.--.-"

  val ampersand = ".-..."
  val colon = "---..."
  val semicolon = "-.-.-."
  val equals = "-...-"
  val plus = ".-.-."
  val minus = "-....-"
  val underscore = "..--.-"
  val quotation = ".-..-."
  val dollar = "...-..-"
  val at = ".--.-."

  val letters = Seq(
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
    'z' -> z
  )

  val numbers = Seq(
    '1' -> one,
    '2' -> two,
    '3' -> three,
    '4' -> four,
    '5' -> five,
    '6' -> six,
    '7' -> seven,
    '8' -> eight,
    '9' -> nine,
    '0' -> zero
  )

  val symbols = Seq(
    '.' -> period,
    ',' -> comma,
    '?' -> questionMark,
    '\'' -> appostraphe,
    '!' -> exclamation,
    '/' -> slash,
    '(' -> openparens,
    ')' -> closeparens,

    '&' -> ampersand,
    ':' -> colon,
    ';' -> semicolon,
    '=' -> equals,
    '+' -> plus,
    '-' -> minus,
    '_' -> underscore,
    '"' -> quotation,
    '$' -> dollar,
    '@' -> at
  )

  val allSymbols = letters ++ numbers ++ symbols
  val map = {
    require(allSymbols.length == allSymbols.map(_._1).toSet.size, "non unique symbol")
    require(allSymbols.length == allSymbols.map(_._2).toSet.size, "non unique morse code sequence")
    val morseSymbolSet = allSymbols.flatMap(_._2).toSet
    require(morseSymbolSet == Set('.', '-'), "unexpected morse symbols found: " + morseSymbolSet)
    allSymbols.toMap
  }
}

object MorseApp {
  val defaultSampleRate = 48000
  val defaintBitDepth = 8

  def main(args: Array[String]): Unit = {
    clrScr()
    print(Console.YELLOW + Console.REVERSED)
    println("Morse utils")
    print(Console.RESET)
    AudioSynth.withAudioSynth(defaultSampleRate, defaintBitDepth) { audioSynth =>
      val morse = Morse(audioSynth, wpm = 15, maybeWpmFarns = Some(10))
      if (args.length == 0)
        morse.play("cq kd2yck pse k.")
      else if (args.length == 1 && args(0) == "-r")
        morse.replMode()
      else if (args.length >= 2 && args(0) == "-w")
        morse.playWords(args(1))
      else if (args.length == 1 && args(0) == "-p")
        morse.showSymbols()
      else
        morse.play(args(0))
    }
    println("\nAll done, 73!")
  }

  def clrScr() = print("\u001b[2J")
}
