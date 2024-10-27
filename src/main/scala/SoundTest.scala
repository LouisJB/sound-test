package Audio

import javax.sound.sampled.AudioFormat
import javax.sound.sampled.AudioSystem
import javax.sound.sampled.LineUnavailableException
import javax.sound.sampled.SourceDataLine
import javax.sound.sampled.AudioFileFormat
import javax.sound.sampled.AudioInputStream
import javax.sound.sampled.TargetDataLine
import javax.sound.sampled.DataLine
import java.io.ByteArrayInputStream
import java.io.File

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
  val maxVol = Math.pow(2.0, bitDepth - 1) - 1
  println(s"Initialized audio synth to sample rate : $sampleRate, bit depth: $bitDepth, max vol: $maxVol")
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
    (0 to noOfSamples).map(_ => (0).toByte).toArray
  }
  def mkSineWave(freq: Double, lenMs: Int) = {
    val noOfSamples = sampleLength(freq, lenMs)
    val waveBuffer = (0 to noOfSamples).map { i =>
      val phaseAngle = PiPi * i * freq / sampleRate
      sine(phaseAngle).toByte
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
      )).toByte
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
      waveFn(phaseAngle).toByte
    }.toArray
  }
  def mkNoiseWave(lenMs: Int) = {
    val rand = new scala.util.Random
    val noOfSamples = (lenMs * sampleRate) / 1000
    (0 to noOfSamples).map { i =>
      (rand.nextDouble() * maxVol).toByte
    }.toArray
  }
  def mkSquareWave(freq: Double, lenMs: Int) = {
    val noOfSamples = sampleLength(freq, lenMs)
    (0 to noOfSamples).map { i =>
      val phaseAngle = PiPi * i * freq / sampleRate
      sine(phaseAngle).sign * maxVol
    }.map(_.toByte).toArray
  }
  def mkSawWave(freq: Double, lenMs: Int) = {
    val noOfSamples = sampleLength(freq, lenMs)
    (0 to noOfSamples).map { i =>
      val phaseAngle = (PiPi * i * freq / sampleRate) % PiPi
      ((2.0 * phaseAngle / PiPi) - 1.0) * maxVol
    }.map(_.toByte).toArray
  }
  def mkTriWave(freq: Double, lenMs: Int) = {
    val noOfSamples = sampleLength(freq, lenMs)
    (0 to noOfSamples).map { i =>
      val phaseAngle = (PiPi * i * freq / sampleRate) % PiPi
      if (phaseAngle < PiPi / 2)
        ((4.0 * phaseAngle / PiPi) - 1.0) * maxVol
      else
        ((4.0 * (PiPi / 2 - phaseAngle) / PiPi) + 1.0) * maxVol
    }.map(_.toByte).toArray
  }
  // pwnVal is a percentage 1-99 value (0 and 100 produce DC and are out of bounds)
  def mkPwmWave(freq: Double, pwmVal: Int, lenMs: Int) = {
    val pwm = Math.min(99, max(1, pwmVal))
    val samplesPerWave = sampleRate / freq
    val pwmLength = ((pwm / 100.0) * samplesPerWave)
    (0 to sampleLength(freq, lenMs)).map { i =>
      if (i % samplesPerWave < pwmLength) maxVol else -1.0 * maxVol
    }.map(_.toByte).toArray
  }

  def mult(as: Array[Byte], bs: Array[Byte]) =
    as.zipAll(bs, 0.toByte, 0.toByte).map { (a, b) => (a.toDouble * b.toDouble).toByte }

  def mult(as: Array[Byte], bs: Array[Double]) =
    as.zipAll(bs, 0.toByte, 1.toDouble).map { (a, b) => (a.toDouble * b).toByte }

  // rescale byte wave as a 0.0 .. 1.0 modulation signal
  def scale(as: Array[Byte]): Array[Double] =
    as.map(a => (a.toDouble - Byte.MinValue) / 255.0)

  def modulate(as: Array[Byte], mod: Array[Byte]) =
    mult(as, scale(mod))
}
// sythesizer source that can play to supplied audio line
case class AudioSynth(line: SourceDataLine, sampleRate: Int, bitDepth: Int) {
  val ws = WaveSynth(sampleRate, bitDepth)
  import ws._
  def play(ab: Array[Byte]) = line.write(ab, 0, ab.length)
  def save(path: String, ab : Array[Byte]) = {
    // preset to mono wav for now
    val outputFile = new File(path).getAbsoluteFile()
    outputFile.getParentFile.mkdirs()
    val audioFormat = new AudioFormat(sampleRate.toFloat, bitDepth, 1, true, true)
    val outStream = new AudioInputStream(
      new ByteArrayInputStream(ab),
      audioFormat,
      ab.length)
    AudioSystem.write(outStream, AudioFileFormat.Type.WAVE, outputFile)
  }
  def drain(): Unit = line.drain()
  def silence(lenMs: Int): Unit =
    play(mkSilence(lenMs))
  def sine(freq: Int, lenMs: Int): Unit =
    play(mkSineWave(freq, lenMs))
  def pulse(freq: Int, lenMs: Int, rampTimeMs: Int = 100) =
    play(mkSineWavePulse(freq, lenMs, rampTimeMs))
  def tone3(freq: Int, lenMs: Int): Unit =
    play(mkWave(freq, lenMs))
  def square(freq: Int, lenMs: Int): Unit =
    play(mkSquareWave(freq, lenMs))
  def saw(freq: Int, lenMs: Int): Unit =
    play(mkSawWave(freq, lenMs))
  def tri(freq: Int, lenMs: Int): Unit =
    play(mkTriWave(freq, lenMs))
  def pwm(freq: Int, pwm: Int, lenMs: Int): Unit =
    play(mkPwmWave(freq, pwm, lenMs))
  def sweep(f1: Int, f2: Int, steps: Int, lenMs: Int) : Unit = {
    val dur = lenMs / ((f2 - f1) / steps)
    (f1 to f2).by(steps).foreach { freq =>
      play(mkSineWave(freq, dur))
    }
  }
  def noise(lenMs: Int) : Unit =
    play(mkNoiseWave(lenMs))
  def blip(f1: Int, f2: Int, steps: Int, lenMs: Int): Unit = {
    val durMs = lenMs / steps / 2
    val audioBuffer1 = mkSineWave(f1, durMs)
    val audioBuffer2 = mkSineWave(f2, durMs)
    (1 to steps).foreach { _ =>
      play(audioBuffer1)
      play(audioBuffer2)
    }
  }
  def blipSweep(f1: Int, f2: Int, f3: Int, steps: Int, substeps: Int, lenMs: Int) : Unit = {
    val dur = lenMs / ((f2 - f1) / steps)
    (f1 to f2).by(steps).foreach { freq =>
      blip(freq - f3/2, freq + f3/2, substeps, dur)
    }
  }
  def randomTones(f1: Int, f2: Int, steps: Int, lenMs: Int) : Unit = {
    val dur = lenMs / steps
    val rand = new scala.util.Random
    (1 to steps).foreach { _ =>
      val freq = rand.nextDouble() * (f2-f1) + f1
      play(mkSineWave(freq.toInt, dur))
    }
  }
  def chromaticSweep(minNote: Int, maxNote: Int, step: Int, durMs: Int) = {
    val cs = ChromaticScale()
    (minNote to maxNote).by(step).foreach( n => sine(cs.freq(n).toInt, durMs))
  }
  def chromaticSweepUpDown(minNote: Int, maxNote: Int, step: Int, durMs: Int) = {
    chromaticSweep(minNote, maxNote, step, durMs)
    chromaticSweep(maxNote, minNote, step * -1, durMs)
  }
  def stop() = AudioSynth.stopDataLine(line)
}
case class Player(as: AudioSynth) {
  import as._
  def playSeq(scale: Scale, noteSeq: Array[Int], durMs: Int, gapMs: Int = 0) =
    noteSeq.foreach(nn => { sine(scale.freq(nn).toInt, durMs); silence(gapMs) })
  def playSeqOpt(scale: Scale, noteSeq: Array[Option[Int]], durMs: Int, gapMs: Int = 0) =
    noteSeq.foreach( _ match {
      case Some(nn) =>
        sine(scale.freq(nn).toInt, durMs)
      case None =>
        silence(durMs)
      silence(gapMs)
  })
  def playSeq(noteSeq: Seq[Notes], playF: (Double, Double) => Array[Byte]) =
    noteSeq.foreach( _ match {
      case Note(f, durMs) =>
        playF(f.toInt, durMs.toInt)
      case Rest(durMs) =>
        silence(durMs.toInt)
  })
}
object AudioSynth {
  println("AudioSynthi v0.1")
  def mkDataLine(sampleRate: Int, bitDepth: Int, bufferSize: Int = AudioConsts.defaultBufferSize): SourceDataLine  = {
    val af : AudioFormat = new AudioFormat(sampleRate.toFloat, bitDepth, 1, true, true)
    val line : SourceDataLine = AudioSystem.getSourceDataLine(af)
    line.open(af, bufferSize)
    line.start()
    println(line.getLineInfo())
    println(line.getFormat())
    line
  }
  def stopDataLine(line: SourceDataLine) = {
    line.drain()
    line.stop()
    line.close()
  }
  def mkAudioSynth(sampleRate: Int, bitDepth: Int) =
    AudioSynth(mkDataLine(sampleRate, bitDepth), sampleRate, bitDepth)
  def withDataLine(sampleRate: Int, bitDepth: Int)(fn: SourceDataLine => Unit): Unit = {
    val line = mkDataLine(sampleRate, bitDepth)
    fn(line)
    stopDataLine(line)
  }
  def withAudioSynth(sampleRate: Int = AudioConsts.defaultSampleRate,
                     bitDepth: Int = AudioConsts.defaultBitDepth)(fn: AudioSynth => Unit): Unit = {
    withDataLine(sampleRate, bitDepth) { line =>
      val as = AudioSynth(line, sampleRate, bitDepth)
      fn(as)
    }
  }
}
