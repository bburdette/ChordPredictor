package musicProcessing

import scala.collection.mutable.ListBuffer
import scala.util._
import com.cra.figaro.language._
import com.cra.figaro.algorithm._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.library.atomic.discrete.Uniform
import com.cra.figaro.library.collection._
import MelodyGenerator._
import java.io._
import SongLearner.SongLearner
import com.cra.figaro.library.compound.CPD

/**
 * @author cLennon
 */
object figModel {
   def main(args: Array[String]) {
     val numIterations=100
     
     
    val melodyMatrixFileName=fileNames.melodyMatrixFileName
    val chordMatrixFileName=fileNames.chordMatrixFileName
    val testFiles=fileNames.testFileDirectory   

    val useSavedData:Boolean=false
//    if(useSavedData){
//      val noteTransProb: Array[Array[Double]]=modelFunction.loadProbArray(12,12,melodyMatrixFileName) 
//      val chordTransProb: Array[Array[Double]]=modelFunction.loadProbArray(1,1,chordMatrixFileName) }
//      
   
    val songLearner=new SongLearner(testFiles)
    songLearner.saveChordTransitionMatrix(fileNames.chordMatrixFileName)
    songLearner.saveMelodyTransitionMatrix(fileNames.melodyMatrixFileName)
    val noteTransProb=songLearner.getMelodyTransistionMatrix
    val chordTransProb=songLearner.getChordTransistionMatrix
   
    

   object probFile {
   val Name: String = melodyMatrixFileName
   val rows: Int =12
   val cols: Int =12
   val noteStates=List("0","1","2","3","4","5","6","7","8","9","10","11")
  }
  
  
  val noteHistory: Array[Element[String]]=Array.fill(numIterations)(Constant("0"))
  val intervalHistory: Array[Element[String]]=Array.fill(numIterations)(Constant(""))
  val initialNote = Uniform("0","1","2","3","4","5","6","7","8","9","10","11")
  val initialInterval=Constant("none")
  val initString =initialNote 
  noteHistory(0)=initString
  val newSongFileName=fileNames.newSongFileName
  //val MelodyGenerator_ = new MelodyGenerator("C:/Users/cLennon/Documents/BenMusicProject/ClassicalMidiCSV")
  
  
   //
 

      
  for(songTime<- 1 until numIterations){
    val noteIntPair=modelFunction.transitionNotesIntervals(noteHistory(songTime-1), intervalHistory(songTime-1),
         probFile.noteStates,noteTransProb)
    noteHistory(songTime)=noteIntPair._1
    intervalHistory(songTime)=noteIntPair._2
  }
 
  val noteHistoryInject=Inject(noteHistory:_*)
  val alg =Importance(noteHistoryInject)

  println("alg start")
  val songSample=alg.sample()._2
  val wantString=songSample(noteHistoryInject)
   val newList:  ListBuffer[String]=ListBuffer()
 
 
  
  modelFunction.writeSong(wantString,newList)
  
  
 
  val preOutputArray =newList.zipWithIndex
  val outputList = preOutputArray.map(a=>(a._2,a._1)).toList
  println("Here is the song")
 println( outputList)
   modelFunction.saveNewSong(newSongFileName,outputList)
   println("song saved to "+newSongFileName)

 
} 
}
