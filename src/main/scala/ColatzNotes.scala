package Audio

object ColatzDemo {
  import AudioConsts._
  def main(args: Array[String]): Unit = {
    AudioSynth.withAudioSynth(defaultSampleRate, defaultBitDepth) { audioSynth =>
      import audioSynth._
        val scale = ChromaticScale()
        val cs1 = colatz(100)
        println(cs1.mkString(", "))
        cs1.map(nn => {
          sine(scale.freq(nn).toInt, 200)
        })
    }
  }

  def colatz(n: Int): List[Int] = {
    if (n == 1)
        List(1)
    else {
      if (n % 2 == 0) 
          n :: colatz(n/2)
      else 
        n :: colatz(3*n + 1)
    }
  }
}
