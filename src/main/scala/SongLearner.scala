// Henry Cooney <hacoo36@gmail.com>
// Chord Predictor -- https://github.com/bburdette/ChordPredictor/
//
// Song Learner
//
// Interface class for the learner modules. Learns patterns from midi files
// converted to CSV, then creates transition matrices represeting state
// transitions probabilities. 
//
// Initialize it to a directory containing CSV files you want to load.

package SongLearner

import java.io._
import ChordGenerator._
import TimingGenerator._

object SongLearner_Test extends SongLearner ("/home/henry/courses/ppaml/chordpredictor/ChordPredictor/TempSongs/"){
    def main(args: Array[String]) {
      println("Hello World!")
      println("Loaded " + dataSet.songList.length + " songs.")
      printTransitionMatrix
      for (key <- chordLookup.keys) {
        for(x <- getChordFromIndex(key))
          print(x+ ",")
        println()
      }
      saveChordTransitionMatrix("/home/henry/courses/ppaml/chordpredictor/testTransitionMatrixSave.csv")
      saveChordLookupTable("/home/henry/courses/ppaml/chordpredictor/testChordLookupSave.csv")
    }

}


class SongLearner(filepath: String) extends ChordGenerator(filepath: String) {

  val timinggenerator = new TimingGenerator
  timinggenerator.loadSongs(filepath)

  def getMelodyTransistionMatrix: Array[Array[Double]] = { normalizedtransitions }
  def getChordTransistionMatrix: Array[Array[Double]] = { normalizedChordTransitionMatrix }
  def getTimingTransitionMatrix: Array[Array[Double]] = { timinggenerator.getTransitionMatrix }

  def getChordFromIndex(i: Int): Array[Int] = { chordLookup(i) }
 
  def saveMelodyTransitionMatrix(fileName: String) {
    val file = new File(fileName)
    val bw = new BufferedWriter(new FileWriter(file))
    for(n<-normalizedtransitions){
       bw.write(n.mkString(",")+"\n")
    }
    bw.close()  
  }

  def saveChordTransitionMatrix(fileName: String){
    val file = new File(fileName)
    val bw = new BufferedWriter(new FileWriter(file))
    for(n<-normalizedChordTransitionMatrix){
       bw.write(n.mkString(",")+"\n")
    }
    bw.close()  
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

    println("RAW MELODY TRANSITION MATRIX: ")

    for(row <- transitions) {
    for(col <- row)
      print(col + ",")
    println()
    }

    println()
    println("NORMALIZED MELODY TRANSISTION MATRIX: ")
    for(row <- normalizedtransitions) {
    for(col <- row)
      print(col + ",")
    println()
    }

    println()
    println("TOTAL MELODY TRANSISTIONS: " + totaltransistions)


    println("RAW CHORD TRANSITION MATRIX: ")
    for(row <- chordTransitionMatrix) {
      for(col <- row)
        print(col + ",")
      println()
    }
    println()

    println("NORMALIZED CHORD TRANSITION MATRIX: ")
    for(row <- normalizedChordTransitionMatrix) {
      for(col <- row)
        print(col + ",")
      println()
    }
    println()
    
    println("TOTAL CHORD TRANSITIONS: " + totalChordTransitions)

  }

}

