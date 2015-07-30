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
import MelodyGenerator._

object SongLearner_Test extends SongLearner {
    def main(args: Array[String]) {
      println("Hello World!")
      loadDirectory("/home/henry/courses/ppaml/chordpredictor/ChordPredictor/TempSongs/")
      chordgenerator.printTransitionMatrix
      saveChordTransitionMatrix("/home/henry/courses/ppaml/chordpredictor/testTransitionMatrixSave.csv")
      saveChordLookupTable("/home/henry/courses/ppaml/chordpredictor/testChordLookupSave.csv")
      loadChordTransitionMatrix("/home/henry/courses/ppaml/chordpredictor/testTransitionMatrixSave.csv")
      chordgenerator.printTransitionMatrix
      melodygenerator.printTransitionMatrix
      timinggenerator.printTransitionMatrix
    }

}


class SongLearner {

  val timinggenerator = new TimingGenerator
  val chordgenerator = new ChordGenerator
  val melodygenerator = new MelodyGenerator

  def loadDirectory(filepath: String) {
    timinggenerator.loadSongs(filepath)
    chordgenerator.loadSongs(filepath)
    melodygenerator.loadSongs(filepath)
  }

  
  //def getMelodyTransistionMatrix: Array[Array[Double]] = { normalizedtransitions }
  def getChordTransistionMatrix: Array[Array[Double]] = { chordgenerator.getTransitionMatrix }
  def getTimingTransitionMatrix: Array[Array[Double]] = { timinggenerator.getTransitionMatrix }
  def getMelodyTransitionMatrix: Array[Array[Double]] = { melodygenerator.getTransitionMatrix }


  def getChordFromIndex(i: Int): Array[Int] = { chordgenerator.chordLookup(i) }
  def saveChordTransitionMatrix(fileName: String){
    chordgenerator.saveTransitionMatrix(fileName)
  }

  def saveTimingTransistionMatrix(fileName: String) {
    timinggenerator.saveTransitionMatrix(fileName)
  }  
  def saveMelodyTransistionMatrix(fileName: String) {
    melodygenerator.saveTransitionMatrix(fileName)
  }  


  def loadTimingTransistionMatrix(fileName: String) {
    timinggenerator.loadTransitionMatrix(fileName)
  }

  def loadChordTransitionMatrix(fileName:String) {
    chordgenerator.loadTransitionMatrix(fileName)
  }

  def loadMelodyTransitionMatrix(fileName:String) {
    melodygenerator.loadTransitionMatrix(fileName)
  }


  def saveChordLookupTable(fileName: String){
    chordgenerator.saveChordLookupTable(fileName)
  }

}

