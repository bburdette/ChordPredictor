//package musicProcessing

import scala.util._
import com.cra.figaro.language._
import com.cra.figaro.algorithm._
import com.cra.figaro.library.atomic.discrete.Uniform  
import MelodyGenerator._
/**
 * @author cLennon
 */
object figModel {
  
  val numIterations=100
  val noteHistory: Array[String]=Array.fill(numIterations)("")
  val intervalHistory: Array[String]=Array.fill(numIterations)("")
  val initialNote = Uniform(probFile.noteStates)
  val initialInterval="none"
  val MelodyGenerator_ = new MelodyGenerator("/home/henry/courses/ppaml/chordpredictor/ChordPredictor/TempSongs/")
  
  noteHistory(0)=initialNote.toString
  
  object probFile  {
   val Name: String ="given File Name" 
   val rows: Int =0
   val cols: Int =0
   val noteStates=List("0","1","2","3","4","5","6","7","8","9","10","11")
  }
  
  for(songTime<- 1 until numIterations){
    val noteIntPair=transitionNotesIntervals(noteHistory(songTime-1), intervalHistory(songTime-1))
    noteHistory(songTime)=noteIntPair._1
    intervalHistory(songTime)=noteIntPair._2
  }
  
  val preOutputArray =noteHistory.zipWithIndex
  val outputArray = preOutputArray.map(a=>(a._2,a._1))
  
   
  
  
  
  
  
  
  
  
  
  // read probabilities as type Array of Arrays with known parameters
  def loadProbArray(): Array[Array[Double]]={
      val probArray= Array.ofDim[Double](probFile.rows, probFile.cols)
      val bufferedSource = io.Source.fromFile(probFile.Name)
      for ((line, count) <- bufferedSource.getLines.zipWithIndex) {
          probArray(count) = line.split(",").map(_.trim).map(x => Try(x.toDouble).getOrElse(0.0)) //warning.. will convert string to double 
          //or will replace by 0 (in case on Na or "" or other)
        }
        bufferedSource.close
        return probArray
      }
    //probability Array read 
    
  
  
  val noteTransProb: Array[Array[Double]]=MelodyGenerator_.getTransitionMatrix
   
  
  def transitionNotesIntervals(note: String, interval:String): (String, String )={
    val transProbs = noteTransProb(note.toInt).toList
    val choice: Element[String]=Select(transProbs,probFile.noteStates)
    val newNoteState=choice.toString
    val newIntervalState=interval
    (newNoteState, newIntervalState)
  }

}
