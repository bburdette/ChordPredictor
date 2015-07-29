// Henry Cooney <hacoo36@gmail.com>
// Chord Predictor -- https://github.com/bburdette/ChordPredictor/
//
// Melody Generator
//
// Melody generation module. Learns 'melody' from underlying chords and
// generates new melodies. Chords may be overlayed onto this.
package MelodyGenerator

//import com.cra.figaro.language._
import musicProcessing._
import musicProcessing.Chord

object MelodyGenerator_Test extends MelodyGenerator ("/home/henry/courses/ppaml/chordpredictor/ChordPredictor/TempSongs/") {
   def main(args: Array[String]) {
     
     println("Hello World!")
     println("Loaded " + dataSet.songList.length + " songs.")
     //printTransitionMatrix
     println()
     println()
   }   
}

class MelodyGenerator (filepath: String){
 
  // Melody generator class.
  // Loads a csv file, then 'learns' melody patterns.
  // Outputs a new csv file containing a generated melody.

  val dataSet = new musicDataSet(filepath)
  //val dataSet = new musicDataSet("/home/henry/courses/ppaml/chordpredictor/ChordPredictor/TempSongs/")
  //val dataSet = new musicDataSet("/home/henry/courses/ppaml/chordpredictor/ChordPredictor/ClassicalMidiCSV/")
  val songs = dataSet.songList
  val transitions = Array.fill(12, 12)(0)
  var totaltransistions = 0

  def learnMelodyTransistion(first: Chord, second: Chord) {
    transitions(first.getRootFreq) (second.getRootFreq) += 1
    totaltransistions += 1
  }

  
}
