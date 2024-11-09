package dsp.fft

import dsp.fft.FFT._

import Math._
import org.scalatest._
import flatspec.AnyFlatSpec
import matchers._

class ExampleSpec extends AnyFlatSpec with should.Matchers {
  
  "FFT" should "be invertable" in {
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
    timeDomainSignal.zip(data).forall { case (a, b) => a approx b } should be (true)
  }

  import Audio._

  "FFT" should "calculate correct dominant frequency" in {
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
