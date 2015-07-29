package musicProcessing

import scala.util._
import com.cra.figaro.algorithm.sampling.OneTimeMetropolisHastings
import com.cra.figaro.language._
import com.cra.figaro.algorithm._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.library.atomic.discrete.Uniform
import com.cra.figaro.library.collection._
import MelodyGenerator._
import java.io._
import MelodyGenerator.MelodyGenerator
import com.cra.figaro.library.compound.CPD
/**
 * @author cLennon
 */
object figModel {
   def main(args: Array[String]) {
    val transitionMatrixFileName="C:/Users/cLennon/Documents/BenMusicProject/tranition.csv"
    val testFiles="C:/Users/cLennon/Documents/BenMusicProject/ClassicalMidiCSV"
    val genMelody=false
    if(genMelody){
    val melodyGen=new MelodyGenerator(testFiles)
    val matrix = melodyGen.getTransitionMatrix
      melodyGen.saveTransitionMatrix(transitionMatrixFileName)}
    
  
   object probFile {
   val Name: String =  "C:/Users/cLennon/Documents/BenMusicProject/tranition.csv"
   val rows: Int =12
   val cols: Int =12
   val noteStates=List("0","1","2","3","4","5","6","7","8","9","10","11")
  }
  
  val numIterations=10
  val noteHistory: Array[Element[String]]=Array.fill(numIterations)(Constant("0"))
  val intervalHistory: Array[Element[String]]=Array.fill(numIterations)(Constant(""))
  val initialNote = Uniform("0","1","2","3","4","5","6","7","8","9","10","11")
  val initialInterval=Constant("none")
  val initString =initialNote //.toString
  noteHistory(0)=initString
  val newSongFileName="C:/Users/cLennon/Documents/BenMusicProject/newSong.csv"
  //val MelodyGenerator_ = new MelodyGenerator("C:/Users/cLennon/Documents/BenMusicProject/ClassicalMidiCSV")
  
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
  
  
   val noteTransProb: Array[Array[Double]]=loadProbArray() //readTransitionMatrix(probFile.Name)
 
    def saveNewSong(text: List[(Int,String)]){
    
    val ntext=text.map { case (int, str) => s"$int,$str\n"}
    val file = new File(newSongFileName)
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(ntext.mkString(""))
    bw.close()
  }
  
  
  def transitionNotesIntervals(note: Element[String], interval:Element[String]): (Element[String], Element[String] )={
    val newNoteState = noteTrans(note)
    val newIntervalState=interval
    (newNoteState, newIntervalState)  
  }
  def noteTrans(oldNote: Element[String]):Element[String]={
   // print("trans")
  //  print(oldNote)
    val newNote=CPD(oldNote,
    "0"->Select(noteTransProb(0).toList,probFile.noteStates) ,   
    "1"->Select(noteTransProb(1).toList,probFile.noteStates) ,
    "2"->Select(noteTransProb(2).toList,probFile.noteStates) ,
    "3"->Select(noteTransProb(3).toList,probFile.noteStates) ,
    "4"->Select(noteTransProb(4).toList,probFile.noteStates) ,
    "5"->Select(noteTransProb(5).toList,probFile.noteStates) ,
    "6"->Select(noteTransProb(6).toList,probFile.noteStates) ,
    "7"->Select(noteTransProb(7).toList,probFile.noteStates) ,
    "9"->Select(noteTransProb(8).toList,probFile.noteStates) ,
    "9"->Select(noteTransProb(9).toList,probFile.noteStates) ,
    "10"->Select(noteTransProb(10).toList,probFile.noteStates), 
    "11"->Select(noteTransProb(11).toList,probFile.noteStates))
   // print(newNote)
    return newNote
  }
  
      
  for(songTime<- 1 until numIterations){
    val noteIntPair=transitionNotesIntervals(noteHistory(songTime-1), intervalHistory(songTime-1))
    noteHistory(songTime)=noteIntPair._1
    intervalHistory(songTime)=noteIntPair._2
  }
  print("loop done")
  val noteHistoryInject=Inject(noteHistory:_*)
  print("inject done ")
  val alg =MetropolisHastings( 1000,ProposalScheme.default,1,1,noteHistoryInject)
  
  alg.start()
  print("alg start")
  val songSample=alg.sample() 
  print("sampled")
 // val x=alg.mostLikelyValue(noteHistoryInject)
  alg.stop()
  print(songSample)
 
//  val preOutputArray =???.zipWithIndex
  //val outputList = preOutputArray.map(a=>(a._2,a._1)).toList
  println("Here is the song")
 // println( outputList)

  
  
  // read probabilities as type Array of Arrays with known parameters

    //probability Array read 
    
//     def readTransitionMatrix(fileName: String): Array[Array[Double]] = {
//       val matrix: Array[Array[Double]]= Array()    
//       val bufferedSource = io.Source.fromFile(fileName)
//          var count = 0
//          for (line <- bufferedSource.getLines) {
//            matrix(count) = line.split(",").map(_.trim).map(_.toDouble)
//            count += 1
//        }
//      bufferedSource.close
//      return matrix
//       
//     }
 
    
   
//  def saveTransitionMatrix(){
//    val file = new File(transitionMatrixFileName)
//    val bw = new BufferedWriter(new FileWriter(file))
//    for(n<-noteTransProb){
//       bw.write(n.mkString(",")+"\n")
//    }
//    bw.close()  
//  }
// 
  
 
} 
}
