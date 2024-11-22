package Audio

import java.util.ArrayDeque
import javax.sound.sampled.SourceDataLine

// a simple tone partial with volume
case class Tone(waveBuffers: Array[Double], volume: Double)

// basic tone mixer allowing for per tone volume
object SimpleWaveMixer {
  def mix(tones: Array[Tone]): Array[Double] = {
    def scale(tone: Tone) =
      tone.waveBuffers.map(_ * tone.volume)
    tones.reduce { case (a, b) =>
      Tone(scale(a).zipAll(scale(b), 0.0, 0.0).map { case (v1, v2) => (v1 + v2)/2.0 }, 1.0)
    }.waveBuffers
  }
}

trait QueueBuffer[T] {
  def enqueue(data: T): Boolean
  def dequeue(): Option[T]
}

case class BasicBuffer[T]() extends QueueBuffer[T] {
  private val queue = new ArrayDeque[T]()

  def enqueue(data: T) = 
    queue.add(data)

  def dequeue(): Option[T] = 
    Option(queue.poll())
}

case class Channel[T <: Double](channelNo: Int, channelName: String, volume: Byte) {
  require(volume >= 0)
  val audioBuffer = BasicBuffer[T]()
  def write(data: T): Boolean = audioBuffer.enqueue(data)
  def read = audioBuffer.dequeue()
  def readWithVolume = read.map(_ * volume / 127.0)
  def write(ds: Array[T]): Boolean =
    ds.map(data => write(data)).forall(_ == true)

  override def toString() = s"Channel: Number: $channelNo, Name: $channelName"
}

// basic tone mixer allowing for per tone volume
// this is a very simple mono channel mixer
// warning not thread safe, do not modify once running...
class ChannelMixer[T <: Double](line: SourceDataLine) {
  private var channels = Set[Channel[T]]()
  private var running = false
  private def toByte(ds: Array[Double]): Array[Byte] = ds.map(_.toByte)

  private def runMixer(stopOnEmpty: Boolean = false) = {
    running = true
    val bufferSize = line.getBufferSize()
    val thread = new Thread {
      override def run = {
        println(s"starting mixer, stopOnEmpty: $stopOnEmpty")
        val channelSize = channels.size
        println("channel size " + channelSize)
        while (running) {
          val bufferVals = (1 to bufferSize).toArray.map { _ =>
            channels.flatMap(c => c.readWithVolume)
          }
          if (bufferVals.forall(_.isEmpty)) {
            if (stopOnEmpty) {
              running = false
              println("stopping because empty")
            }
            else {
               Thread.sleep(1) // should use a signal later...
            }
          }
          else {
            // scale down then add
            val data = bufferVals.map(_.map(_ / channelSize).sum)
            line.write(toByte(data), 0, data.size)
          }
        }
        println("mixer ended")
      }
    }
    thread.start
    thread
  }

  // caller needs to figure how to wait and end
  def run = {
    runMixer(false)
    true
  }

  // ends when all channel data runs out
  def runAndWait(stopOnEmpty: Boolean = false): Unit =
    runMixer(stopOnEmpty).join()

  def step() =
    running = false

  def addChannel(channel: Channel[T]) = {
    require(running == false)
    println(s"adding channel: $channel")
    channels += channel
  }

  def addChannel(chans: Seq[Channel[T]]) = {
    require(running == false)
    println(s"adding ${chans.length} channels")

    chans.foreach(println)
    channels ++= chans
  }

  def removeChannel(channel: Channel[T]) = {
    require(running == false)
    println(s"removing channel: $channel")
    channels -= channel
  }
}

object MixerTest {
  val sampleRate = 48000
  val bitDepth = 8

  def main(args: Array[String]): Unit = {
    AudioSynth.withDataLine(sampleRate, bitDepth) { sdl =>
      // set up a simple mixer
      val mixer = ChannelMixer(sdl)
      val channel1 = Channel[Double](1, "Sine 1", 127)
      val channel2 = Channel[Double](2, "Tri 1", 75)
      val channel3 = Channel[Double](3, "Noise", 75)
      val channel4 = Channel[Double](4, "Square 1", 50)
      mixer.addChannel(Seq(channel1, channel2, channel3, channel4))

      val ws = WaveSynth(sampleRate, bitDepth)
      channel1.write(ws.mkSineWavePulse(500, 3000))
      channel2.write(ws.mkTriWave(2000, 1000))
      channel3.write(ws.mkNoiseWave(1500))
      channel4.write(ws.mkSquareWave(1000, 2000))

      mixer.runAndWait(true)
    }
  }
}
