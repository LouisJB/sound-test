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
        val eg = ws.basicEg

        play {
          val lenMs = 2500
          val es = EnvelopeSpec(1000.0, 500.0, .4, 500.0)
          val egSignal = eg.mkEg(es, lenMs)
          ws.applyEg(egSignal)(ws.mkSineWave(1200, lenMs))
        }
        silence(1000)

        // example simple mixing
        play {
          val lenMs = 2000
          val applyEg = ws.applyEg(eg.mkEg(EnvelopeSpec(500.0, 250.0, .7, 250.0), lenMs))
          applyEg(SimpleWaveMixer.mix(Array(
            Tone(ws.mkSineWave(500, lenMs), 1.0),
            Tone(ws.mkSineWave(1000, lenMs), 1.0),
            Tone(ws.mkSineWave(2000, lenMs), 1.0)
          )))
        }
        silence(1000)

        // example multi-tone partial mixing with EG amplitude moduation
        play {
          val lenMs = 5000
          val applyEg = ws.applyEg(eg.mkEg(EnvelopeSpec(750.0, 250.0, .7, 250.0), lenMs))
          applyEg(SimpleWaveMixer.mix(Array(
            Tone(ws.modulate(ws.mkTriWave(1000, lenMs), ws.mkSineWave(5, lenMs)), 0.7),
            Tone(ws.mkSquareWave(2000, lenMs), 0.5),
            Tone(ws.mkSineWave(500, lenMs), 1.0),
            Tone(ws.mkPwmWave(4000, 10, lenMs), 0.5),
            Tone(ws.mult(ws.mkNoiseWave(lenMs), eg.mkEg(EnvelopeSpec(lenMs/2, lenMs/2, .0, .0), lenMs)), 0.6)
          )))
        }
        silence(1000)

        // EG example
        val lenMs = 2500
        play {
          val es = EnvelopeSpec(1000.0, 500.0, .4, 500.0)
          ws.applyEg(eg.mkEg(es, lenMs))(ws.mkSineWave(1200, lenMs))
        }
        play {
          val es = EnvelopeSpecPercent(30.0, 20.0, .4, 20.0)
          ws.applyEg(eg.mkEg(es, lenMs))(ws.mkSineWave(1000, lenMs))
        }

        // example modulate 1 wave by another
        play(ws.modulate(ws.mkTriWave(1000, 2000), ws.mkSineWave(5, 2000)))

        // examples of PWM sweeping
        (1 to 99).foreach ( p =>
          pwm(1000, p, 100)
        )

        (1 to 99).foreach ( p =>
          play(ws.mkPwmWave(440, p, 100).zip(
            ws.mkPwmWave(880 * 1.5, (p + 50) % 99, 100)).map { case (a, b) => (a + b) / 2.0 }
          )
        )

        // wave gen, wave file generation and saving
        val sineAb = ws.mkSineWave(1000, 1000)
        save("wav/sine-1kHz.wav", sineAb)
        val triAb = ws.mkTriWave(1000, 1000)
        save("wav/tri-1kHz.wav", triAb)
        val pwm50Ab = ws.mkPwmWave(1000, 50, 1000)
        save("wav/pwm-50-1kHz.wav", pwm50Ab)

        // play chromatic scale notes
        chromaticSweepUpDown(-13, 13, 1, 100)
        chromaticSweep(1, 13, 1, 100)
        chromaticSweep(1, -13, -1, 100)
        chromaticSweep(-26, 26, 1, 50)

        // selection of tone/wave demos
        pulse(1000, 5000, 2000)
        sine(500, 1000)
        square(500, 1000)
        saw(500, 1000)
        tri(500, 1000)
        // pitch shift, check is relatively free of zc noise
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

        // DTMF demo, see DTMF for full example
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

object BasicSequencerDemos {
  import Utils._
  import AudioConsts._
  def main(args: Array[String]): Unit = {
    AudioSynth.withAudioSynth(defaultSampleRate, defaultBitDepth) { audioSynth =>
      import audioSynth._
      val eg = ws.basicEg

      // some simple scala sequence players
      val player = Sequencers(audioSynth)
      import player._

      // set up note generators
      val bpm = Durations(120)
      val cs = ChromaticScale()
      println("Node 1 is " + cs.freq(1))
      val ng = NoteGen(cs, bpm)
      import ng._

      // construct a wave generator based synth with amplitude Eg
      val wsp = WaveSynthPlayer(audioSynth.output, defaultSampleRate, defaultBitDepth)
      wsp.ws.setEnv(EnvelopeSpec(20.0, 50.0, .7, 100.0))
      wsp.ws.addTgs(Seq(
        (f: Double, lenMs: Double) => Tone(wsp.ws.wg.mkSineWave(f/2.0, lenMs.toInt), 1.0),
        (f: Double, lenMs: Double) => Tone(wsp.ws.wg.mkSineWave(f*2, lenMs.toInt), 0.2),
        (f: Double, lenMs: Double) => Tone(wsp.ws.wg.mkSawWave(f, lenMs.toInt), 0.7),
        (f: Double, lenMs: Double) => Tone(wsp.ws.wg.mkNoiseWave(lenMs.toInt), 0.02)
      ))
      wsp.play(2000, 1000)
      Thread.sleep(1000)

      // Jingle Bells sequence for Christmas...
      val notes1 = quarters(Seq(
        Some(8), Some(8), Some(8), None,
        Some(8), Some(8), Some(8), None,
        Some(8), Some(11), Some(4), Some(6)
      )) ++ Seq(
        note(8, bpm.whole)
      ) ++ quarters(Seq(
        Some(9), Some(9), Some(9), Some(9),
        Some(9), Some(8), Some(8)
      )) ++ mkNotes(Seq(
        Some(8), Some(8)), Eigth)
       ++ quarters(Seq(
        Some(11), Some(11), Some(9),
        Some(6)
      )) ++ Seq(
        note(4, bpm.whole)
      )

      play(seqMap(notes1, (f, lenMs) => {
        println(s"Freq: $f, durMs: $lenMs")
        wsp.ws.play(f, lenMs)
      }).flatten.toArray)
      Thread.sleep(1000)

      val notes2 = Seq(
        Some(1), None, None, Some(3), Some(5), None, None, Some(3), Some(5), None, Some(1), None, Some(5), None, None, None,
        Some(3), None, None, Some(5), Some(6), Some(6), Some(5), Some(3), Some(6), Some(6), Some(6), None, None, None, None
      ).map {
        case Some(nn) =>
          Note(cs.freq(nn), bpm.quarter)
        case None => Rest(bpm.quarter)
      }
      playSeq(notes2, wsp.play)
      Thread.sleep(1000)

      // these play using default built-in sine tone
      playSeqOpt(cs,
        Array(
          Some(1), None, None, Some(3), Some(5), None, None, Some(3), Some(5), None, Some(1), None, Some(5), None, None, None,
          Some(3), None, None, Some(5), Some(6), Some(6), Some(5), Some(3), Some(6), Some(6), Some(6), None, None, None, None
      ),
      200, 50)
      Thread.sleep(1000)

      playSeq(cs, Array(1, 3, 2, 4, 5, 6, 3), 200)
    }
  }
}

case class NoteGen(cs: ChromaticScale, bpm: Durations) {
  def note(nn: Int, dur: Double): Notes = Note(cs.freq(nn), dur)
  def quarters(notes: Seq[Option[Int]]) = mkNotes(notes, Quarter)

  def mkNotes(notes: Seq[Option[Int]], duration: NoteDuration) = notes.map( _ match {
    case Some(nn) =>
      note(nn, bpm.length(duration))
    case None => Rest(bpm.length(duration))
  })
}
