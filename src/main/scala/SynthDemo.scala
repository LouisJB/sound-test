package Audio

object SynthDemo {
  import AudioConsts._
  def main(args: Array[String]): Unit = {
    AudioSynth.withAudioSynth(defaultSampleRate, defaultBitDepth) { audioSynth =>
      import audioSynth._
      if (args.contains("-r")) {
        println("type cmd and press enter (/ex to quit)")
        while (true) {
          val line = Console.in.readLine()
          if (line == "/ex")
            System.exit(0)
          else line match {
            case s"sine $freq $duration" =>
              sine(freq.toInt, duration.toInt)
            case s"tri $freq $duration" =>
              tri(freq.toInt, duration.toInt)
            case s"saw $freq $duration" =>
              saw(freq.toInt, duration.toInt)
            case s"sqr $freq $duration" =>
              square(freq.toInt, duration.toInt)
            case s"noise $duration" =>
              noise(duration.toInt)  
            case s"sweep $f1 $f2 $steps $lenMs" =>
              sweep(f1.toInt, f2.toInt, steps.toInt, lenMs.toInt)
            case s"blip $f1 $f2 $steps $lenMs" =>
              blip(f1.toInt, f2.toInt, steps.toInt, lenMs.toInt)
            case s"rnd $f1 $f2 $steps $lenMs" =>
              randomTones(f1.toInt, f2.toInt, steps.toInt, lenMs.toInt)
            case s"pwm $freq $pwmVal $lenMs" =>
              pwm(freq.toInt, pwmVal.toInt, lenMs.toInt)
            case s"blipSweep $f1 $f2 $f3 $steps $subSteps $lenMs" =>
              blipSweep(f1.toInt, f2.toInt, f3.toInt, steps.toInt, subSteps.toInt, lenMs.toInt)
            case _ => println("syntax error")
          }
          audioSynth.silence(250)
        }
      }
      else {
        (1 to 99).foreach ( p => 
          pwm(1000, p, 100)
        )

        val sineAb = ws.mkSineWaveBuffer(1000, 1000)
        save("wav/sine-1kHz.wav", sineAb)
        val triAb = ws.mkTriWave(1000, 1000)
        save("wav/tri-1kHz.wav", triAb)
        val pwm50Ab = ws.mkPwmWave(1000, 50, 1000)
        save("wav/pwm-50-1kHz.wav", pwm50Ab)

        System.exit(0)

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
