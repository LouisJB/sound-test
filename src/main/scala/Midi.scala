package Audio

import java.io.File
import javax.sound.midi.MidiEvent
import javax.sound.midi.MidiMessage
import javax.sound.midi.MidiSystem
import javax.sound.midi.Sequence
import javax.sound.midi.ShortMessage
import javax.sound.midi.Track
import ShortMessage._
import javax.sound.midi.Instrument
import javax.sound.midi.MidiChannel
import javax.sound.midi.Synthesizer

object MidiDemo {
  def main(args: Array[String]): Unit = {
    val synth = MidiSystem.getSynthesizer()
    synth.open()

    println("Max Poly: " + synth.getMaxPolyphony())

    println("Voice Status:")
    synth.getVoiceStatus().foreach((println))

    val instruments = synth.getDefaultSoundbank().getInstruments()
    println("\nAll Instruments:")
    instruments.foreach(println)

    showChannels(synth)

    val instr1 = instruments(90)    // Pad 3 (polysynth) bank #0 preset #90
    val instr2 = instruments(99)    // Instrument: FX 4 (atmosphere) bank #0 preset #99
    val instr3 = instruments(102)   // FX 7 (echoes) bank #0 preset #102
    val instr4 = instruments(116)   // Instrument: Taiko Drum bank #0 preset #116

    synth.loadInstrument(instr1)
    synth.loadInstrument(instr2)
    synth.loadInstrument(instr3)
    synth.loadInstrument(instr4)

    val midiChanels = synth.getChannels()
    programChange(midiChanels(0), instr1)
    programChange(midiChanels(1), instr2)
    programChange(midiChanels(2), instr3)
    programChange(midiChanels(3), instr4)

    showChannels(synth)

    play(midiChanels(0), 60, 1000)
    play(midiChanels(1), 62, 1000)
    play(midiChanels(2), 65, 1000)
    play(midiChanels(3), 60, 1000)

    // play standard drum/perc sounds on GM midi chan 10
    (35 to 70).foreach { nn =>
      play(midiChanels(9), nn, 250)
    }

    allOff(midiChanels)
  }

  def showChannels(synth: Synthesizer) = {
    val midiChanels = synth.getChannels()
    println(s"\nMIDI Channels (${midiChanels.length}):")
    midiChanels.zipWithIndex.foreach { case (c, i) =>
      println(s"channel: $i, program: ${c.getProgram()}")
    }
  }

  def programChange(channel: MidiChannel, instr: Instrument) =
    channel.programChange(instr.getPatch().getBank(), instr.getPatch().getProgram())

  def play(midiChanel: MidiChannel, noteNo: Int, durMs: Int, vel: Int = 600) = {
    midiChanel.noteOn(noteNo, vel)
    Thread.sleep((durMs))
    midiChanel.noteOff(noteNo)
  }

  def allOff(midiChanels: Array[MidiChannel]) = {
    midiChanels.foreach { mc =>
      mc.allNotesOff()
      mc.allSoundOff()
    }
  }

  def loadMidi = {
    val noteNamesList = Array("C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B")

    val sequence = MidiSystem.getSequence(new File("test.mid"))
    val tracks = sequence.getTracks().zipWithIndex
    tracks.foreach { case (track, trackIdx) =>
      println("Track " + trackIdx + ": size = " + track.size())
      (0 to track.size).foreach { i =>
        val event = track.get(i)
        print("@" + event.getTick() + " ")
        val message = event.getMessage()
        if (message.isInstanceOf[ShortMessage]) {
          val sm = message.asInstanceOf[ShortMessage]
          print("Channel: " + sm.getChannel() + " ")
          if (sm.getCommand() == NOTE_ON) {
            val key = sm.getData1()
            val octave = (key / 12)-1
            val note = key % 12
            val noteName = noteNamesList(note)
            val velocity = sm.getData2()
            println("Note on, " + noteName + octave + " key=" + key + " velocity: " + velocity)
          } else if (sm.getCommand() == NOTE_OFF) {
            val key = sm.getData1()
            val octave = (key / 12)-1
            val note = key % 12
            val noteName = noteNamesList(note)
            val velocity = sm.getData2()
            println("Note off, " + noteName + octave + " key=" + key + " velocity: " + velocity)
          } else {
            println("Command:" + sm.getCommand())
          }
        }
        else {
          println("Other message: " + message.getClass())
        }
      }
    }
  }
}

// example main to play midi files
object SimpleMidiPlayer {
  // arg 1 is midi file to play, example
  // sbt "runMain Audio.SimpleMidiPlayer Queen-BohemianRhapsody.mid"
  def main(args: Array[String]) = {
    if (args.length == 1) {
      val file = args(0)
      val midiFile = new File(file)
      println("Loading from: " + midiFile.getAbsolutePath())
      if (midiFile.exists() && !midiFile.isDirectory() && midiFile.canRead()) {
        val sequencer = MidiSystem.getSequencer()
        sequencer.setSequence(MidiSystem.getSequence(midiFile))
        sequencer.open()
        println("Playing sequence")
        sequencer.start()
        while (sequencer.isRunning()) {
          print('.')
          Thread.sleep(5000)
        }
        sequencer.stop()
        sequencer.close()
        println("Sequence ended")
      }
      else {
        println("failed to find / open midi file")
        showHelp()
      }
    }
    else showHelp()
  }

  def showHelp() =
    System.out.println("Usage: java SimpleMidiPlayer midifile.mid")
}
