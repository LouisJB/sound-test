
package Audio

object AudioConsts {
  val defaultSampleRate = 96 * 1000
  val defaultBitDepth = 8
  val defaultBufferSize = 4096
}
object Debug {
  val enabled = false
  def debug(s: String) = if (enabled)
    println(s)
}
// creates waveforms in sampled PCM format
case class WaveSynth(sampleRate: Int, bitDepth: Int) {
  import Math._
  import Debug._
  val PiPi = Math.PI * 2.0
  val maxVol = pow(2.0, bitDepth - 1) - 1
  println(s"Initialized wave synth to sample rate : $sampleRate, bit depth: $bitDepth, max vol: $maxVol")
  def sampleLength(freq: Double, lenMs: Int): Int = {
    debug("Feq: " + freq)
    val periodMs = 1 / freq * 1000 // ms
    debug("periodMs: " + periodMs)
    val waveCount = (lenMs / periodMs).toInt
    debug("waveCount: " + waveCount)
    val noOfSamples = (waveCount * periodMs * sampleRate / 1000).toInt
    debug("numberOfSamples: " + noOfSamples)
    noOfSamples
  }  
  def sine(phaseAngle: Double) : Double = sin(phaseAngle) * maxVol
  def sine3(phaseAngle : Double) : Double = {
    val f1 = sin(phaseAngle)
    val f3 = sin(phaseAngle * 3)
    (f1 * maxVol * 0.7) + (f3 * maxVol * 0.3)
  }
  def mkSilence(lenMs: Int) = {
    val noOfSamples = lenMs * sampleRate / 1000
    (0 to noOfSamples).map(_ => 0.0).toArray
  }
  def mkSineWave(freq: Double, lenMs: Int) = {
    val noOfSamples = sampleLength(freq, lenMs)
    val waveBuffer = (0 to noOfSamples).map { i =>
      val phaseAngle = PiPi * i * freq / sampleRate
      sine(phaseAngle)
    }.toArray
    debug(waveBuffer.toSeq.take(100).mkString(", "))
    debug(waveBuffer.toSeq.drop(noOfSamples - 50).mkString(", "))
    waveBuffer
  }
  // simple symetrical attack/decay rise/fall times
  def mkSineWavePulse(freq: Double, lenMs: Int, rampTimeMs: Int = 100) = {
    val waveBufferRaw = mkSineWave(freq, lenMs)
    val noOfSamples = waveBufferRaw.length
    val rampLength = sampleRate * rampTimeMs / 1000.0
    val waveBuffer = waveBufferRaw.zipWithIndex.map { case (s, i) =>
      (s * min(
        min(1.0, min(i, rampLength)/rampLength),
        min(1.0, min(noOfSamples - i, rampLength)/rampLength)
      ))
    }
    debug(waveBuffer.toSeq.take(100).mkString(", "))
    debug(waveBuffer.toSeq.drop(noOfSamples - 50).mkString(", "))
    waveBuffer
  }
  def mkWave(freq: Double, lenMs: Int, waveFn : Double => Double = sine3) = {
    val period = sampleRate / freq
    val noOfSamples = sampleLength(freq, lenMs)
    (0 to noOfSamples).map { i =>
      val phaseAngle = PiPi * i / period
      waveFn(phaseAngle)
    }.toArray
  }
  def mkNoiseWave(lenMs: Int) = {
    val rand = new scala.util.Random
    val noOfSamples = (lenMs * sampleRate) / 1000
    (0 to noOfSamples).map { i =>
      (rand.nextDouble() * maxVol)
    }.toArray
  }
  def mkSquareWave(freq: Double, lenMs: Int) = {
    val noOfSamples = sampleLength(freq, lenMs)
    (0 to noOfSamples).map { i =>
      val phaseAngle = PiPi * i * freq / sampleRate
      sine(phaseAngle).sign * maxVol
    }.toArray
  }
  def mkSawWave(freq: Double, lenMs: Int) = {
    val noOfSamples = sampleLength(freq, lenMs)
    (0 to noOfSamples).map { i =>
      val phaseAngle = (PiPi * i * freq / sampleRate) % PiPi
      ((2.0 * phaseAngle / PiPi) - 1.0) * maxVol
    }.toArray
  }
  def mkTriWave(freq: Double, lenMs: Int) = {
    val noOfSamples = sampleLength(freq, lenMs)
    (0 to noOfSamples).map { i =>
      val phaseAngle = (PiPi * i * freq / sampleRate) % PiPi
      if (phaseAngle < PiPi / 2)
        ((4.0 * phaseAngle / PiPi) - 1.0) * maxVol
      else
        ((4.0 * (PiPi / 2 - phaseAngle) / PiPi) + 1.0) * maxVol
    }.toArray
  }
  // pwnVal is a percentage 1-99 value (0 and 100 produce DC and are out of bounds)
  def mkPwmWave(freq: Double, pwmVal: Int, lenMs: Int) = {
    val pwm = Math.min(99, max(1, pwmVal))
    val samplesPerWave = sampleRate / freq
    val pwmLength = ((pwm / 100.0) * samplesPerWave)
    (0 to sampleLength(freq, lenMs)).map { i =>
      if (i % samplesPerWave < pwmLength) maxVol else -1.0 * maxVol
    }.toArray
  }

  def mult(as: Array[Double], bs: Array[Double]) =
    as.zipAll(bs, 0.0, 1.0).map { (a, b) => a * b }

  def applyEg(eq: Array[Double])(ws: Array[Double]) =
    mult(ws, eq)

  // rescale wave as a 0.0 .. 1.0 modulation signal
  def scale(as: Array[Double]): Array[Double] =
    as.map(a => (a - (pow(2.0, bitDepth - 1)) - 1.0) / (pow(2.0, bitDepth) - 1))

  def modulate(as: Array[Double], mod: Array[Double]) =
    mult(as, scale(mod))

  def basicEg = EG(sampleRate)
}
