// Henry Cooney <hacoo36@gmail.com>
// Chord Predictor -- https://github.com/bburdette/ChordPredictor/
//
// Melody Generator
//
// Melody generation module. Learns 'melody' from underlying chords and
// generates new melodies. Chords may be overlayed onto this.
package ChordGenerator

import musicProcessing._
import MelodyGenerator._


object ChordGenerator_Test extends ChordGenerator ("/home/henry/courses/ppaml/chordpredictor/ChordPredictor/TempSongs/"){
    def main(args: Array[String]) {
      println("Hello World!")
      println("Loaded " + dataSet.songList.length + " songs.")
      //printTransitionMatrix
     for (key <- chordTransitions.keys) {
        println(key)
      }
      println()
      println()
      for (key <- chordLookup.keys) {
        for(x <- chordLookup(key))
          print(x + ",")
        println()
      }

      println("Number of unique chords: " + numberUniqueChords)
      //printTransitionMatrix
    }
}

class ChordGenerator (filepath: String) extends MelodyGenerator(filepath) {

  // Chord generator class
  // Learns both melody and chord patterns from a directory
  // of MIDI files converted to CSV.

  
  var chordTransitions :Map[String, Int] = Map()
  var chordLookup :Map[Int, Array[Int]] = Map()
  var numberUniqueChords = 0 
  var totalChordTransitions = 0
  val chordLength = 2 // Chords will be capped at this length

  for (song <- songs) {
    println("Getting unique chords: " + song.getName)
    getUniqueChords(song.getChordList)
  }
  val chordTransitionMatrix = Array.fill(numberUniqueChords, numberUniqueChords)(0)

  for (song <- songs) {
    learnChords(song.getChordList)
  }

  val normalizedtransitions = transitions.map(_.map((x: Int) => x.toDouble/totaltransistions))

  val normalizedChordTransitionMatrix = chordTransitionMatrix.map(_.map((x: Int) => x.toDouble/totalChordTransitions))


  override def learnChords(s: List[Chord]) {
    // Populate the melody transition matrix with each chord transition found.
    if(s.length > 1) {
      val first = s.head
      val second = s.tail.head
      learnMelodyTransistion(first, second)
      learnChordTransition(first, second)
      learnChords(s.tail)
      }
   }

  def learnChordTransition(first: Chord, second: Chord) {
    // Learn the chord transitions in all songs;
    // add them to the chord transition matrix.
    val firstID = makeChordIdentifier(first.notes, chordLength)
    
    val i = chordTransitions(makeChordIdentifier(first.notes, chordLength))
    val j = chordTransitions(makeChordIdentifier(second.notes, chordLength))
    totalChordTransitions += 1
    chordTransitionMatrix(i)(j) += 1
  }
  
  def getUniqueChords(s: List[Chord]) {
    // Examine each chord in s. If it is new, add it
    // to the unique chords dictionary.
    if(s.length > 1) {
      val identifier = makeChordIdentifier(s.head.notes, chordLength)
      if(!(chordTransitions contains identifier)) {
        chordTransitions = chordTransitions + 
          (identifier -> numberUniqueChords)
        chordLookup = chordLookup + 
          (numberUniqueChords -> getChordArray(s.head.notes, chordLength))
        numberUniqueChords += 1
      }
      getUniqueChords(s.tail)
    }
  }

  def makeChordIdentifier(c: Array[Int], length: Int): String = {
    // Examines first elements of chord c, up to length LENGTH.
    // Returns a string uniquely describing the chord's transition.
    // For example: the chord [1, 3, 5] returns the string '2,4', since
    // it contains one note 2 steps above base, and one note 4 steps above base

    if(c.length <= 1) ""
    else makeChordIdentifier(c.tail, length, c.head)
  }


  def makeChordIdentifier(c: Array[Int], length: Int, base: Int): String = {
    // If this is the last requested note, or we are at the end of the chord,
    // return. Else, add the difference between base and the current note
    // to the identifier string.
    if (c.length < 1 || length < 1) ""
    else (c.head).toString + "," +
    (makeChordIdentifier(c.tail, length-1, base))
  }

  def getChordArray(c: Array[Int], length: Int): Array[Int] = {
    // Trims a chord to contain the first LENGTH offsets.
    c.splitAt(length+1)._1.tail
  }
}
