package dsp.fft

import scala.math._

// simple CT radix-2 FFT
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

  def complexify(wf: Array[Double]): Seq[Complex] =
    wf.toSeq.map(re => Complex(re, 0))
  def complexify(wf: Array[Int]): Seq[Complex] =
    complexify(wf.map(_.toDouble))
  def complexify(wf: Array[(Double, Double)]): Seq[Complex] =
    wf.toSeq.map { case (re, im) => Complex(re, im) }
}

case class FFT(sampleRate: Int, windowLen: Int) {
  import Audio._
  val ws = WaveGen(sampleRate, 8)
  import ws._
  val windowLenMs = 1000 * windowLen / sampleRate
  val binSizeHz = (sampleRate/2.0) / (windowLen/2)
  println(s"FFT binSizeHz: $binSizeHz Hz")
  lazy val defaultWindowEs = EnvelopeSpecPercent(5.0, 0.0, 1.0, 5.0) // unsure what this should be, but this is a start

  def binFreq(binIdx: Int) =
    binIdx * binSizeHz
  def windowed(rawWf: Array[Double], windowEnvSpec: EnvelopeSpecPercent = defaultWindowEs): Array[Double] = {
    val wf = rawWf.take(windowLen)
    ws.mult(wf, ws.basicEg.mkEg(windowEnvSpec, windowLenMs))
  }
  // will return the non-imaged freq bins with the frequency they represent in _1
  def doFft(wf: Seq[Complex]) =
    FFT.fft(wf).take(windowLen/2).zipWithIndex.map { case (c, i) => FTTBin(binFreq(i), c) }
  def maxAmplitudeFreq(freqBins: Seq[FTTBin]) = {
    freqBins.reduce { case (a @ FTTBin(f1, c1), b @ FTTBin(f2, c2)) =>
      if (c1 > c2) a else b
    }
  }
}

case class FTTBin(freq: Double, value: Complex) {
  override def toString = s"frequency: $freq, amplitude value: $value"
}

case class Complex(re: Double, im: Double) {
  import Complex._
  infix def eq(x: Complex): Boolean = re == x.re && im == x.im
  infix def approx(x: Complex, ep: Double = epsilon): Boolean = abs(re - x.re) <= ep && abs(im - x.im) <= ep
  infix def +(x: Complex): Complex = Complex(re + x.re, im + x.im)
  infix def -(x: Complex): Complex = Complex(re - x.re, im - x.im)
  infix def *(x: Double): Complex = Complex(re * x, im * x)
  infix def *(x: Complex): Complex = Complex(re * x.re - im * x.im, re * x.im + im * x.re)
  infix def /(x: Double): Complex = Complex(re / x, im / x)
  infix def >(other: Complex): Boolean = this.mag > other.mag

  def mag = sqrt(re * re + im * im)

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
  val epsilon = 1E-12
  def apply(re: Int, im: Int): Complex =
    Complex(re.toDouble, im.toDouble)
  def exp(c: Complex): Complex = {
    val r = (cosh(c.re) + sinh(c.re))
    Complex(cos(c.im), sin(c.im)) * r
  }
}

object BasicAutoCorrelation {
  def autoCorrelation(xs: Array[Double]): Array[Double] = {
    val n = xs.length
    (0 until n).toArray.map { j =>
      (0 until n).map(i => xs(i) * xs((n + i - j) % n)).sum
    }
  }

  // simple peak finder
  def findFirstPeak(xs: Array[Double]): Option[(Double, Int)] = {
     xs.toList.zipWithIndex.sliding(3, 1).toList.find {
      case List[(Double, Int)](a, b, c) => a._1 < b._1 && c._1 < b._1
      case _ => false
    }
  }.map(_.drop(1).head)
}

object ACFTest {
  @main def runAcf() = {
    import BasicAutoCorrelation._
    val sampleRate = 1000 // pretend rate
    val testData1 = Array(1.0, -81.0, 2.0, -15.0, 8.0, 2.0, -9.0, 0.0)
    val ac1 = BasicAutoCorrelation.autoCorrelation(testData1)
    println(s"ac1: " + ac1.mkString(", "))

    val testData2 = (1 to 5).flatMap(_ => Array(-2.0, -1.0, 0.0, 1.0, 2.0, 3.0, 2.0, 1.0, 0.0, -1.0, -2.0, -3.0)).toArray
    val ac2 = BasicAutoCorrelation.autoCorrelation(testData2)
    println(s"ac2: " + ac2.mkString(", "))
    val maxAc = findFirstPeak(ac2).get
    println(s"max AC $maxAc")
    val freq0 = sampleRate.toDouble / maxAc._2.toDouble
    println(s"freq0: $freq0")
  }
}
