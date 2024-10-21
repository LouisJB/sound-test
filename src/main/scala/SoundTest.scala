package Audio

import javax.sound.sampled.AudioFormat
import javax.sound.sampled.AudioSystem
import javax.sound.sampled.LineUnavailableException
import javax.sound.sampled.SourceDataLine
import javax.sound.sampled.AudioFileFormat
import java.io.File
import javax.sound.sampled.AudioInputStream
import javax.sound.sampled.TargetDataLine
import javax.sound.sampled.DataLine
import java.io.ByteArrayInputStream

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
case class AudioSynth(line: SourceDataLine, sampleRate: Int, bitDepth: Int) {
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
  def mkSilenceBuffer(lenMs: Int) = {
    val noOfSamples = lenMs * sampleRate / 1000
    (0 to noOfSamples).map(_ => (0).toByte).toArray
  }
  def mkSineWaveBuffer(freq: Double, lenMs: Int) = {
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
  def mkSineWavePulseBuffer(freq: Double, lenMs: Int, rampTimeMs: Int = 100) = {
    val waveBufferRaw = mkSineWaveBuffer(freq, lenMs)
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
  def mkWaveBuffer(freq: Double, lenMs: Int, sineF : Double => Double = sine3) = {
    val period = sampleRate / freq
    val noOfSamples = sampleLength(freq, lenMs)
    (0 to noOfSamples).map { i =>
      val phaseAngle = PiPi * i / period
      sineF(phaseAngle).toByte
    }.toArray
  }
  def mkNoiseBuffer(lenMs: Int) = {
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
    play(mkSilenceBuffer(lenMs))
  def sine(freq: Int, lenMs: Int): Unit =
    play(mkSineWaveBuffer(freq, lenMs))
  def pulse(freq: Int, lenMs: Int, rampTimeMs: Int = 100) =
    play(mkSineWavePulseBuffer(freq, lenMs, rampTimeMs))
  def tone3(freq: Int, lenMs: Int): Unit =
    play(mkWaveBuffer(freq, lenMs))
  def square(freq: Int, lenMs: Int): Unit =
    play(mkSquareWave(freq, lenMs))
  def saw(freq: Int, lenMs: Int): Unit =
    play(mkSawWave(freq, lenMs))
  def tri(freq: Int, lenMs: Int): Unit =
    play(mkTriWave(freq, lenMs))
  def sweep(f1: Int, f2: Int, steps: Int, lenMs: Int) : Unit = {
    val dur = lenMs / ((f2 - f1) / steps)
    (f1 to f2).by(steps).foreach { freq =>
      play(mkSineWaveBuffer(freq, dur))
    }
  }
  def noise(lenMs: Int) : Unit =
    play(mkNoiseBuffer(lenMs))
  def blip(f1: Int, f2: Int, steps: Int, lenMs: Int): Unit = {
    val durMs = lenMs / steps / 2
    val audioBuffer1 = mkSineWaveBuffer(f1, durMs)
    val audioBuffer2 = mkSineWaveBuffer(f2, durMs)
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
      val audioBuffer = mkSineWaveBuffer(freq.toInt, dur)
      play(audioBuffer)
    }
  }
  def chromaticSweep(minNote: Int, maxNote: Int, step: Int, durMs: Int) = {
    val cs = ChromaticScale()
    (minNote to maxNote).by(step).foreach( n => sine(cs.freq(n), durMs))
  }
  def chromaticSweepUpDown(minNote: Int, maxNote: Int, step: Int, durMs: Int) = {
    chromaticSweep(minNote, maxNote, step, durMs)
    chromaticSweep(maxNote, minNote, step * -1, durMs)
  }
  def playSeq(scale: Scale, noteSeq: Array[Int], durMs: Int, gapMs: Int = 0) =
    noteSeq.foreach(nn => { sine(scale.freq(nn), durMs); silence(gapMs) })
  def playSeqOpt(scale: Scale, noteSeq: Array[Option[Int]], durMs: Int, gapMs: Int = 0) =
    noteSeq.foreach( _ match {
      case Some(nn) =>
        sine(scale.freq(nn), durMs)
      case None =>
        silence(durMs)
      silence(gapMs)
  })
  def stop() = AudioSynth.stopDataLine(line)
}

object SynthDemo {
  import AudioConsts._
  def main(args: Array[String]): Unit = {
    AudioSynth.withAudioSynth(defaultSampleRate, defaultBitDepth) { audioSynth =>
      if (args.contains("-r")) {
        println("type cmd and press enter (/ex to quit)")
        while (true) {
          val line = Console.in.readLine()
          if (line == "/ex")
            System.exit(0)
          else line match {
            case s"sine $freq $duration" =>
              audioSynth.sine(freq.toInt, duration.toInt)
            case s"tri $freq $duration" =>
              audioSynth.tri(freq.toInt, duration.toInt)
            case s"saw $freq $duration" =>
              audioSynth.saw(freq.toInt, duration.toInt)
            case s"sqr $freq $duration" =>
              audioSynth.square(freq.toInt, duration.toInt)
            case s"noise $duration" =>
              audioSynth.noise(duration.toInt)  
            case s"sweep $f1 $f2 $steps $lenMs" =>
              audioSynth.sweep(f1.toInt, f2.toInt, steps.toInt, lenMs.toInt)
            case s"blip $f1 $f2 $steps $lenMs" =>
              audioSynth.blip(f1.toInt, f2.toInt, steps.toInt, lenMs.toInt)
            case s"rnd $f1 $f2 $steps $lenMs" =>
              audioSynth.randomTones(f1.toInt, f2.toInt, steps.toInt, lenMs.toInt)
            case s"blipSweep $f1 $f2 $f3 $steps $subSteps $lenMs" =>
              audioSynth.blipSweep(f1.toInt, f2.toInt, f3.toInt, steps.toInt, subSteps.toInt, lenMs.toInt)
            case _ => println("syntax error")
          }
          audioSynth.silence(250)
        }
      }
      else {
        import audioSynth._
        val sineAb = mkSineWaveBuffer(1000, 1000)
        save("wav/sine-1kHz.wav", sineAb)
        val triAb = mkTriWave(1000, 1000)
        save("wav/tri-1kHz.wav", triAb)

        val cs = ChromaticScale()
        playSeqOpt(cs,
          Array(
            Some(1), None, None, Some(3), Some(5), None, None, Some(3), Some(5), None, Some(1), None, Some(5), None, None, None,
            Some(3), None, None, Some(5), Some(6), Some(6), Some(5), Some(3), Some(6), Some(6), Some(6), None, None, None, None
        ),
        200, 50)

        playSeq(cs, Array(1, 3, 2, 4, 5, 6, 3), 200)
        chromaticSweepUpDown(-13, 13, 1, 100)
        chromaticSweep(1, 13, 1, 100)
        chromaticSweep(1, -13, -1, 100)
        chromaticSweep(-26, 26, 1, 50)

        pulse(1000, 5000, 2000)
        sine(500, 1000)
        square(500, 1000)
        saw(500, 1000)
        tri(500, 1000)
        // check is relatively free of zc noise
        (1 to 50).foreach { i =>
          sine(950+i, 50-i/3)
        }
        blipSweep(500, 2500, 200, 100, 4, 5000)
        sine(100, 1000)
        square(1000, 1000)
        square(400, 1000)
        sine(1000, 1000)
        sine(750, 1000)
        sine(700, 900)
        sine(500, 500)
        tone3(1000, 1000)
        blip(500, 1000, 100, 4000)
        sweep(400, 1600, 5, 4000)
        blip(200, 800, 25, 5000)
        sweep(1600, 200, -3, 4000)
        (1 to 10).foreach { i => 
          noise(1000/i)
          drain()
          Thread.sleep(1000/(11-i))
        }
        randomTones(200, 1000, 20, 5000)
        sweep(3200, 300, -10, 5000)

        DTMF(audioSynth).play("T  001 718 8675309 # RSRSR")
        Thread.sleep(1000)
        (1 to 2).foreach { _ =>
          blip(1500, 1900, 10, 2000)
          Thread.sleep(4000)
        }
      }
    }
  }
}

object Pitches {
  import Math._
  case class Note(freq: Int)
  val A1 = Note(440)

  val semitoneRatio = pow(2, 1.0/12)
  val wholetomeRatio = pow(2, 1.0/6)
}
import Pitches._
trait Scale {
  def freq(note: Int): Int
}
case class ChromaticScale(baseFreq: Int = A1.freq) extends Scale {
  import Math._
  val ratio = semitoneRatio
  def freq(note: Int): Int =
    (baseFreq * pow(ratio, note-1)).toInt
}
