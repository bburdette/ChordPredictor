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
import java.io._
import scala.collection.mutable.ArrayBuffer

object ChordGenerator_Test extends ChordGenerator {
    def main(args: Array[String]) {
      println("Hello World!")
     // println("Loaded " + dataSet.songList.length + " songs.")
      val filepath = "/home/henry/courses/ppaml/chordpredictor/ChordPredictor/TempSongs/"
      val dataSet = new musicDataSet(filepath)
      val songs = dataSet.songList
      loadSongs(filepath)
      printTransitionMatrix
      saveTransitionMatrix("/home/henry/courses/ppaml/chordpredictor/testChordMatrixSave.csv")
      loadTransitionMatrix("/home/henry/courses/ppaml/chordpredictor/testChordMatrixSave.csv")
      printTransitionMatrix

    }
}

class ChordGenerator {

  // Chord generator class
  // Learns both melody and chord patterns from a directory
  // of MIDI files converted to CSV.

  
  var chordTransitions :Map[String, Int] = Map()
  var chordLookup :Map[Int, Array[Int]] = Map()
  var numberUniqueChords = 0 
  var totalChordTransitions = 0

  val chordLength = 2 // Chords will be capped at this length

  var chordTransitionMatrix = Array.fill(numberUniqueChords, numberUniqueChords)(0)
  var normalizedChordTransitionMatrix = Array.fill(numberUniqueChords, numberUniqueChords)(0.0)

  def getTransitionMatrix: Array[Array[Double]] = {
    normalizedChordTransitionMatrix
  }

  def loadSongs(filepath: String) {
    val dataSet = new musicDataSet(filepath)
    val songs = dataSet.songList
    for (song <- songs)
      getUniqueChords(song.chordList)
    chordTransitionMatrix = Array.fill(numberUniqueChords, numberUniqueChords)(0)
    for (song <- songs)
      learnChords(song.chordList)
    normalizeTransistionMatrix
  }


  def learnChords(s: List[Chord]) {
    // Populate the melody transition matrix with each chord transition found.
    if(s.length > 1) {
      val first = s.head
      val second = s.tail.head
      //learnMelodyTransistion(first, second)
      learnChordTransition(first, second)
      learnChords(s.tail)
      }
   }

  def normalizeTransistionMatrix {

    normalizedChordTransitionMatrix = 
      Array.fill(numberUniqueChords, numberUniqueChords)(0.0)

    if(numberUniqueChords > 0) {
      for(i <- 0 to numberUniqueChords-1) {
        for(j <- 0 to numberUniqueChords-1)
          normalizedChordTransitionMatrix(i)(j) =
            chordTransitionMatrix(i)(j).toDouble/totalChordTransitions
      }
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

  def saveTransitionMatrix(fileName: String) {
    val file = new File(fileName)
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(totalChordTransitions.toString + "\n")
    for(n<-chordTransitionMatrix){
       bw.write(n.mkString(",")+"\n")
    }
    bw.close()  
  }

  def loadTransitionMatrix(filepath: String) {
    totalChordTransitions = 0
    val f = new java.io.File(filepath)
    val csv = ArrayBuffer[Array[Int]]()
    var rows =0
    var cols =0
    val name =f.getName.toString

    val bufferedSource = io.Source.fromFile(f)
    val lines = bufferedSource.getLines
    totalChordTransitions = lines.next.toInt
    //println(totalChordTransitions)
    for (line <- lines) {
      ///println(line)
      rows +=1
      csv += line.split(",").map(_.trim).map(_.toInt)
      if(rows==1){cols=csv(0).length}
      totalChordTransitions += 1
    }
    bufferedSource.close()
    chordTransitionMatrix = Array.fill(numberUniqueChords, numberUniqueChords)(0)
    //for (row <- csv) {println (row.mkString)}
    if(totalChordTransitions > 0) {
      for(i <- 0 to numberUniqueChords-1) {
        for(j <- 0 to numberUniqueChords-1)
          chordTransitionMatrix(i)(j) =
            csv(i)(j)
      }
    }
    normalizeTransistionMatrix
  } 

  def saveChordLookupTable(fileName: String){
    val file = new File(fileName)
    val bw = new BufferedWriter(new FileWriter(file))
    for (key <- chordLookup.keys) {
      bw.write(chordLookup(key).mkString(",") + "\n")
    }
    bw.close()
  }


  def printTransitionMatrix {

    println("RAW CHORD TRANSITION MATRIX: ")

    for(row <- chordTransitionMatrix) {
    for(col <- row)
      print(col + ",")
    println()
    }
    
    println()
    println("NORMALIZED CHORD TRANSISTION MATRIX: ")
    for(row <- normalizedChordTransitionMatrix) {
    for(col <- row)
      print(col + ",")
    println()
    }

    println()
    println("TOTAL CHORD TRANSISTIONS: " + totalChordTransitions)
  }
}
