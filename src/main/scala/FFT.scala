package FFT

import scala.math._

// largely wholesale borrowed from rosettacode as a starting point
case class Complex(re: Double, im: Double) {
  infix def eq(x: Complex): Boolean = re == x.re && im == x.im
  infix def approx(x: Complex, ep: Double = 1E-12): Boolean = abs(re - x.re) <= ep && abs(im - x.im) <= ep
  infix def +(x: Complex): Complex = Complex(re + x.re, im + x.im)
  infix def -(x: Complex): Complex = Complex(re - x.re, im - x.im)
  infix def *(x: Double):  Complex = Complex(re * x, im * x)
  infix def *(x: Complex): Complex = Complex(re * x.re - im * x.im, re * x.im + im * x.re)
  infix def /(x: Double):  Complex = Complex(re / x, im / x)

  override def toString(): String = {
    val a = "%1.3f" format re
    val b = "%1.3f" format abs(im)
    (a, b) match {
      case (_, "0.000") => a
      case ("0.000", _) => b + "i"
      case (_, _) if im > 0 => a + " + " + b + "i"
      case (_, _) => a + " - " + b + "i"
    }
  }
}
object Complex {
  def apply(re: Int, im: Int): Complex =
    Complex(re.toDouble, im.toDouble)
  def exp(c: Complex): Complex = {
    val r = (cosh(c.re) + sinh(c.re))
    Complex(cos(c.im), sin(c.im)) * r
  }
}
case class FTTBin(freq: Double, value: Complex)
object FFT {
  import Complex._
  def _fft(cs: Seq[Complex], direction: Complex, scalar: Int): Seq[Complex] = {
    if (cs.length == 1)
      cs
    else {
      val n = cs.length
      assume(n % 2 == 0, s"The Cooley-Tukey FFT algorithm only works when the length of the input is even. $n")

      val evenOddPairs = cs.grouped(2).toSeq
      val evens = _fft(evenOddPairs map (_(0)), direction, scalar)
      val odds  = _fft(evenOddPairs map (_(1)), direction, scalar)
      def leftRightPair(k: Int): (Complex, Complex) = {
        val base = evens(k) / scalar
        val offset = exp(direction * (Pi * k / n)) * odds(k) / scalar
        (base + offset, base - offset)
      }

      val pairs = (0 until n/2) map leftRightPair
      val left  = pairs map (_._1)
      val right = pairs map (_._2)
      left ++ right
    }
  }

  def fft(cSeq: Seq[Complex]): Seq[Complex] =
    _fft(cSeq, Complex(0,  2), 1)
  def rfft(cSeq: Seq[Complex]): Seq[Complex] =
    _fft(cSeq, Complex(0, -2), 2)
}

case class FFT(sampleRate: Int, windowLen: Int) {
  import Audio._
  val ws = WaveSynth(sampleRate, 8)
  import ws._
  val windowLenMs = 1000 * windowLen / sampleRate
  val binSizeHz = (sampleRate/2.0) / (windowLen/2)
  println(s"FFT binSizeHz: $binSizeHz Hz")
  lazy val defaultWindowEs = EnvelopeSpecPercent(5.0, 0.0, 1.0, 5.0) // unsure what this should be, but this is a start

  def binFreq(binIdx: Int) =
    binIdx * binSizeHz
  def windowed(rawWf: Array[Byte], windowEnvSpec: EnvelopeSpecPercent = defaultWindowEs): Array[Byte] = {
    val wf = rawWf.take(windowLen)
    ws.mult(wf, ws.basicEg.mkEg(windowEnvSpec, windowLenMs))
  }
  def complexify(wf: Array[Byte]): Seq[Complex] =
    wf.toSeq.map(re => Complex(re, 0))
  // will return the non-imaged freq bins with the frequency they represent in _1
  def doFft(wf: Seq[Complex]) =
    FFT.fft(wf).take(windowLen/2).zipWithIndex.map { case (c, i) => FTTBin(binFreq(i), c) }
  def maxAmplitudeFreq(freqBins: Seq[FTTBin]) = {
    freqBins.reduce { case (a @ FTTBin(f1, Complex(re1, _)), b @ FTTBin(f2, Complex(re2, _))) =>
      if (abs(re1) > abs(re2)) a else b
    }
  }
}

object FFTTest {
  import FFT._
  import Audio._
  import Math._
  @main def run() = {

    // basic checks
    val data = Seq(
                (1,0),
                (1,0),
                (1,0),
                (1,0),
                (0,0),
                (0,2),
                (0,0),
                (0,0)).map(Complex.apply.tupled)

    println(fft(data))
    val timeDomainSignal = rfft(fft(data))
    println(timeDomainSignal)
    assert(timeDomainSignal.zip(data).forall { case (a, b) => a approx b })

    // sine wave tests, note this needs better windowing to be more accurate. Todo
    val sampleRate = 40000
    val ws = WaveSynth(sampleRate, 8)
    import ws._
    val windowLen = 2048
    val ffter = FFT(sampleRate, windowLen)
    import ffter._

    val windowLenMs = 1000 * windowLen / sampleRate
    println(s"windowLenMs: $windowLenMs Ms")

    val trailFreqs = Seq(1100.0, 700.0, 1400.0, 1600, 250.0, 925.0)
    trailFreqs.map { freq =>
      val rawWf = ws.mkSineWave(freq, windowLenMs * 2)
      println(s"freq: $freq, wave sample len = " + rawWf.length)
      val maxFreqVal = maxAmplitudeFreq(doFft(complexify(windowed(rawWf))))
      val diff = freq - maxFreqVal.freq
      println(s"freq: ${freq}Hz, max: ${maxFreqVal.freq}Hz, difference: ${diff}Hz, binSize: ${binSizeHz}Hz")
      maxFreqVal
    }

    val dcWf = Array.tabulate(windowLen)(_ => 255.toByte)
    val maxFreqVal = maxAmplitudeFreq(doFft(complexify(windowed(dcWf))))
    println(s"DC signal found max: ${maxFreqVal.freq}Hz")
  }
}
