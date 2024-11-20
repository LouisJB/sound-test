package Audio

import java.io.File
import javax.sound.midi.MidiEvent
import javax.sound.midi.MidiMessage
import javax.sound.midi.MidiSystem
import javax.sound.midi.Sequence
import javax.sound.midi.ShortMessage
import javax.sound.midi.Track
import ShortMessage._

object MidiDemo {
  def main(args: Array[String]): Unit = {
    val synth = MidiSystem.getSynthesizer()
    synth.open()

    println("Max Poly: " + synth.getMaxPolyphony())

    println("Voice Status:")
    synth.getVoiceStatus().foreach((println))

    val midiChanels = synth.getChannels()
    println("\nMIDI channels:")
    midiChanels.foreach(println)
    
    val instruments = synth.getDefaultSoundbank().getInstruments()
    
    println("\nInstruments:")
    instruments.foreach(println)
    synth.loadInstrument(instruments(90))
    synth.loadInstrument(instruments(91))

    midiChanels(1).noteOn(60, 600)
    Thread.sleep((1000))
    midiChanels(1).noteOff(60)
    midiChanels(2).noteOn(62, 600)
    Thread.sleep((1000))
    midiChanels(1).noteOff(62)
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
