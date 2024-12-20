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

// sythesizer source that can play to supplied audio line
case class AudioSynth(output: UnbufferedOutputSink, sampleRate: Int, bitDepth: Int) {
  println(s"Initialized audio synth to sample rate : $sampleRate, bit depth: $bitDepth")

  import Utils._
  val wg = WaveGen(sampleRate, bitDepth)
  import wg._

  def play(ab: Array[Double]) = output.write(toByte(ab))
  def play(ab: Array[Byte]) = output.write(ab)
  def save(path: String, ab : Array[Double]): Int = save(path, toByte(ab))
  def save(path: String, ab : Array[Byte]): Int = {
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
  def drain(): Unit = output.drain()
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
  def blipSweep(f1: Int, f2: Int, f3: Int, steps: Int, substeps: Int, lenMs: Int): Unit = {
    val dur = lenMs / ((f2 - f1) / steps)
    (f1 to f2).by(steps).foreach { freq =>
      blip(freq - f3/2, freq + f3/2, substeps, dur)
    }
  }
  def randomTones(f1: Int, f2: Int, steps: Int, lenMs: Int): Unit = {
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
  def stop() = output.stop()
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
    AudioSynth(UnbufferedOutputSink(mkDataLine(sampleRate, bitDepth)), sampleRate, bitDepth)
  def withDataLine(sampleRate: Int, bitDepth: Int)(fn: SourceDataLine => Unit): Unit = {
    val line = mkDataLine(sampleRate, bitDepth)
    fn(line)
    stopDataLine(line)
  }
  def withAudioSynth(sampleRate: Int = AudioConsts.defaultSampleRate,
                     bitDepth: Int = AudioConsts.defaultBitDepth)(fn: AudioSynth => Unit): Unit = {
    withDataLine(sampleRate, bitDepth) { line =>
      val output = UnbufferedOutputSink(line)
      val as = AudioSynth(output, sampleRate, bitDepth)
      fn(as)
    }
  }
}
