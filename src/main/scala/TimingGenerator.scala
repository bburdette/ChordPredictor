package TimingGenerator

import musicProcessing._
import MelodyGenerator._
import java.io._
import scala.collection.mutable.ArrayBuffer


object TimingGenerator_Test extends TimingGenerator {
    def main(args: Array[String]) {
      println("Hello World!")
     // println("Loaded " + dataSet.songList.length + " songs.")
      val filepath = "/home/henry/courses/ppaml/chordpredictor/ChordPredictor/TempSongs/"
      val dataSet = new musicDataSet(filepath)
      val songs = dataSet.songList
      loadSongs(filepath)
      printTransitionMatrix
      saveTransitionMatrix("/home/henry/courses/ppaml/chordpredictor/testTimingMatrixSave.csv")
      loadTransitionMatrix("/home/henry/courses/ppaml/chordpredictor/testTimingMatrixSave.csv")
      printTransitionMatrix

    }
}


class TimingGenerator {

  val binVals = Array[Int](125, 250, 500, 750, 1000, 1250, 1500, 1750, 2000, 2250)
  val numBins = binVals.length
  val timingTransitionMatrix = Array.fill(numBins,numBins)(0)
  var totalTimingTransitions = 0
  val normalizedTimingMatrix = Array.fill(numBins,numBins)(0.0)

  def getTransitionMatrix: Array[Array[Double]] = { normalizedTimingMatrix }

  def printTransitionMatrix {

    println("RAW TIMING TRANSITION MATRIX: ")

    for(row <- timingTransitionMatrix) {
    for(col <- row)
      print(col + ",")
    println()
    }
    
    println()
    println("NORMALIZED TIMING TRANSISTION MATRIX: ")
    for(row <- normalizedTimingMatrix) {
    for(col <- row)
      print(col + ",")
    println()
    }

    println()
    println("TOTAL TIMING TRANSISTIONS: " + totalTimingTransitions)
  
  }

  def loadSongs(filepath: String) {
    val dataSet = new musicDataSet(filepath)
    val songs = dataSet.songList

    for (song <- songs)
      learnTiming(song.chordList)

  }

  def learnTiming(s: List[Chord]) {
    if(s.length > 1) {
      val first = s.head
      val second = s.tail.head
      learnTimingTransition(first.duration, second.duration)
      learnTiming(s.tail)
      normalizeTransistionMatrix
    }
  }

  def learnTimingTransition(first: Int, second: Int) {
    val bin1= getBin(first)
    val bin2 = getBin(second)
    if (bin1 < numBins && bin2 < numBins) {
      // Drop transitions between chords that are above the max bin
      timingTransitionMatrix(getBin(first))(getBin(second)) += 1
      totalTimingTransitions += 1
    }
  }

  def getBin(dur: Int): Int = {
    // Assigns an integer duration to the appropriate bin
    getBin(dur, 0, binVals)
  }

  def getBin(dur: Int, count: Int, bins: Array[Int]): Int = {
    if(bins.length < 1) count
    else if(dur < bins.head) count
    else getBin(dur, count+1, bins.tail)
  }
  



  def getLongestChordLength(s: List[Chord]): Int = {
      getLongestChordLength_helper(s, 0)
    }

  def getLongestChordLength_helper(s: List[Chord], max: Int): Int = {
    if(s.isEmpty)
      max
    else
      getLongestChordLength_helper(s.tail, if (s.head.duration > max )
        s.head.duration else max)
    }

  def normalizeTransistionMatrix {
    for(i <- 0 to numBins-1) {
      for(j <- 0 to numBins-1)
        normalizedTimingMatrix(i)(j) = 
          timingTransitionMatrix(i)(j).toDouble/totalTimingTransitions
    }
  }

  def saveTransitionMatrix(fileName: String) {
    val file = new File(fileName)
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(totalTimingTransitions.toString + "\n")
    for(n<-timingTransitionMatrix){
       bw.write(n.mkString(",")+"\n")
    }
    bw.close()  
  }

  def loadTransitionMatrix(filepath: String) {
    totalTimingTransitions = 0
    val f = new java.io.File(filepath)
    val csv = ArrayBuffer[Array[Int]]()
    var rows =0
    var cols =0
    val name =f.getName.toString

    val bufferedSource = io.Source.fromFile(f)
    val lines = bufferedSource.getLines
    totalTimingTransitions = lines.next.toInt
    println(totalTimingTransitions)
    for (line <- lines) {
      println(line)
      rows +=1
      csv += line.split(",").map(_.trim).map(_.toInt)
      if(rows==1){cols=csv(0).length}
      totalTimingTransitions += 1
    }
    bufferedSource.close()
    for (row <- csv) {println (row.mkString)}
    
    for(i <- 0 to numBins-1) {
      for(j <- 0 to numBins-1)
        timingTransitionMatrix(i)(j) =
          csv(i)(j)
    }    
    normalizeTransistionMatrix
  } 
}


