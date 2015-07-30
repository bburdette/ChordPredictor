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
import java.io._
import scala.collection.mutable.ArrayBuffer

object MelodyGenerator_Test extends MelodyGenerator {
   def main(args: Array[String]) {
     
     println("Hello World!")
     val filepath = "/home/henry/courses/ppaml/chordpredictor/ChordPredictor/TempSongs/"
     val dataSet = new musicDataSet(filepath)
     val songs = dataSet.songList
     loadSongs(filepath)
     printTransitionMatrix
     saveTransitionMatrix("/home/henry/courses/ppaml/chordpredictor/testMelodyMatrixSave.csv")
     loadTransitionMatrix("/home/henry/courses/ppaml/chordpredictor/testMelodyMatrixSave.csv")
     printTransitionMatrix
   }   
}

class MelodyGenerator {

  var totalTransitions = 0
  var transitionMatrix = Array.fill(12, 12)(0)
  var normalizedTransitionMatrix = Array.fill(12, 12)(0.0)

  def getTransitionMatrix: Array[Array[Double]] = {
    normalizedTransitionMatrix
  }

  def loadSongs(filepath: String) {
    val dataSet = new musicDataSet(filepath)
    val songs = dataSet.songList
    transitionMatrix = Array.fill(12, 12)(0)
    for (song <- songs)
      learnChords(song.chordList)
    normalizeTransistionMatrix
  }


  def learnChords(s: List[Chord]) {
    // Populate the melody transition matrix with each chord transition found.
    if(s.length > 1) {
      val first = s.head
      val second = s.tail.head
      learnMelodyTransistion(first, second)
      learnChords(s.tail)
      }
   }

  def learnMelodyTransistion(first: Chord, second: Chord) {
    transitionMatrix(first.getRootFreq)(second.getRootFreq) += 1
    totalTransitions += 1
  }

  def normalizeTransistionMatrix {

    normalizedTransitionMatrix = 
      Array.fill(12, 12)(0.0)

    if(totalTransitions > 0) {
      for(i <- 0 to 11) {
        for(j <- 0 to 11)
          normalizedTransitionMatrix(i)(j) =
            transitionMatrix(i)(j).toDouble/totalTransitions
      }
    }
  }



  def saveTransitionMatrix(fileName: String) {
    val file = new File(fileName)
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(totalTransitions.toString + "\n")
    for(n<-transitionMatrix){
       bw.write(n.mkString(",")+"\n")
    }
    bw.close()  
  }

  def loadTransitionMatrix(filepath: String) {
    totalTransitions = 0
    val f = new java.io.File(filepath)
    val csv = ArrayBuffer[Array[Int]]()
    var rows =0
    var cols =0
    val name =f.getName.toString

    val bufferedSource = io.Source.fromFile(f)
    val lines = bufferedSource.getLines
    totalTransitions = lines.next.toInt
    //println(totalChordTransitions)
    for (line <- lines) {
      ///println(line)
      rows +=1
      csv += line.split(",").map(_.trim).map(_.toInt)
      if(rows==1){cols=csv(0).length}
      totalTransitions += 1
    }
    bufferedSource.close()
    transitionMatrix = Array.fill(12, 12)(0)
    //for (row <- csv) {println (row.mkString)}
    if(totalTransitions > 0) {
      for(i <- 0 to 11) {
        for(j <- 0 to 11)
          transitionMatrix(i)(j) =
            csv(i)(j)
      }
    }
    normalizeTransistionMatrix
  } 



  def printTransitionMatrix {

    println("RAW MELODY TRANSITION MATRIX: ")

    for(row <- transitionMatrix) {
    for(col <- row)
      print(col + ",")
    println()
    }
    
    println()
    println("NORMALIZED MELODY TRANSISTION MATRIX: ")
    for(row <- normalizedTransitionMatrix) {
    for(col <- row)
      print(col + ",")
    println()
    }

    println()
    println("TOTAL MELODYTRANSISTIONS: " + totalTransitions)
  }
  
}
