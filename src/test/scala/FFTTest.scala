package dsp.fft

import dsp.fft.FFT._
import Audio._

import Math._
import org.scalatest._
import org.scalatest.wordspec.AnyWordSpec
import matchers._

class ExampleSpec extends AnyWordSpec with should.Matchers {
  def approxEq(as: Seq[Complex], bs: Seq[Complex]) =
    as.zip(bs).forall { case (a, b) => a approx b }

  "FFT" should {
    "create basic freq analysis" in {
      Seq(
        complexify(Array(0, 0, 0, 0)) -> complexify(Array(0.0, 0.0, 0.0, 0.0)), // nil case
        complexify(Array(1, 1, 1, 1)) -> complexify(Array(4.0, 0.0, 0.0, 0.0)), // d.c case
        complexify(Array(1, 0, -1, 0)) -> complexify(Array(0.0, 2.0, 0.0, 2.0)),
        complexify(Array(-1, 0, 1, 0)) -> complexify(Array(0.0, -2.0, 0.0, -2.0)),
        complexify(Array(0, 1, 0, -1)) -> complexify(Array((0.0, 0.0), (0.0, 2.0), (0.0, 0.0), (0.0, -2.0))),
        complexify(Array(1, -1, 1, -1)) -> complexify(Array((0.0, 0.0), (0.0, 0.0), (4.0, 0.0), (0.0, 0.0))),   // nyquist cos re phase
        complexify(Array(-1, 1, -1, 1)) -> complexify(Array((0.0, 0.0), (0.0, 0.0), (-4.0, 0.0), (0.0, 0.0))),  // nyquist cos re phase
        complexify(Array(1, 1, 0, 0)) -> complexify(Array((2.0, 0.0), (1.0, 1.0), (0.0, 0.0), (1.0, -1.0))),
        complexify(Array(1, 1, -1, -1)) -> complexify(Array((0.0, 0.0), (2.0, 2.0), (0.0, 0.0), (2.0, -2.0))),
        complexify(Array(1, -1, -1, 1)) -> complexify(Array((0.0, 0.0), (2.0, -2.0), (0.0, 0.0), (2.0, +2.0))),
        complexify(Array(1, 1, -1, -1, 1, 1, -1, -1)) -> complexify(Array((0.0, 0.0), (0.0, 0.0), (4.0, 4.0), (0.0, 0.0), (0.0, 0.0), (0.0, 0.0), (4.0, -4.0), (0.0, 0.0))),
      ).zipWithIndex.foreach { case ((xns, expectedXks), idx) =>
        val xks = fft(xns)
        println(xks.mkString(", "))
        withClue (s"$idx: fft($xns) was:\n${xks.mkString(",")} not expected:\n${expectedXks.mkString(",")}") {
          approxEq(xks, expectedXks) should be (true)
        }
      }
    }

    "be invertable" in {
      val xks = Seq(
        (1,0),
        (1,0),
        (1,0),
        (1,0),
        (0,0),
        (0,2),
        (0,0),
        (0,0)
      ).map(Complex.apply.tupled)

      println(fft(xks))
      val tds = rfft(fft(xks))
      println(tds)
      approxEq(xks, tds) should be (true)
    }

    "calculate correct dominant frequency" in {
      // sine wave tests, note this needs better windowing to be more accurate. Todo
      val sampleRate = 40000
      val bitDepth = 8
      val ws = WaveSynth(sampleRate, bitDepth)
      import ws._
      val windowLen = 2048
      val ffter = FFT(sampleRate, windowLen)
      import ffter._

      val windowLenMs = 1000 * windowLen / sampleRate
      println(s"windowLenMs: $windowLenMs Ms")

      val trailFreqs = (10 to 3000).by(100).map(_.toDouble)
      trailFreqs.map { freq =>
        val rawWf = ws.mkSineWave(freq, windowLenMs * 2)
        println(s"freq: $freq, wave sample len = " + rawWf.length)
        val maxFreqVal = maxAmplitudeFreq(doFft(complexify(windowed(rawWf))))
        val diff = freq - maxFreqVal.freq
        println(s"freq: ${freq}Hz, max: ${maxFreqVal.freq}Hz, difference: ${diff}Hz, binSize: ${binSizeHz}Hz")
        assert(diff <= binSizeHz)

        import BasicAutoCorrelation._
        val acs = autoCorrelation(rawWf)
        val maxAc = acs.toSeq.zipWithIndex.drop(1).maxBy(_._1)
        println(s"maxAc: $maxAc")
        maxFreqVal
      }

      val dcWf = Array.tabulate(windowLen)(_ => pow(2.0, bitDepth) - 1.0)
      val maxFreqVal = maxAmplitudeFreq(doFft(complexify(windowed(dcWf))))
      println(s"DC signal found max: ${maxFreqVal.freq}Hz")
      assert(maxFreqVal.freq == 0.0)
    }
  }

  "acf" should {
    "calculate correct fundamental frequency" in {
      val sampleRate = 1000
      val bitDepth = 8
      val ws = WaveSynth(sampleRate, bitDepth)
      import ws._
      val freq = sampleRate / 20
      val sampleLenMs = 1000 / freq
      val rawWf = ws.mkSineWave(freq, sampleLenMs * 3)
      import BasicAutoCorrelation._
      val acs = autoCorrelation(rawWf)
      val maxAc = findFirstPeak(acs)
      //println(acs.mkString(", "))
      println(s"max AC $maxAc")
      val freq0 = sampleRate.toDouble / maxAc.get._2.toDouble
      println(s"freq0: $freq0")
    }
  }
}
