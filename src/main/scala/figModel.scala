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

    val songLearner=new SongLearner()
 
     //songLearner.loadMelodyTransitionMatrix(melodyMatrixFileName)
  
   
    
    songLearner.loadDirectory(testFiles)
    
    songLearner.saveChordTransitionMatrix(fileNames.chordMatrixFileName)
    songLearner.saveMelodyTransistionMatrix(fileNames.melodyMatrixFileName)
    songLearner.saveTimingTransistionMatrix(fileNames.timeMatrixFileName)  
    
    
    val timeTransProb=songLearner.getTimingTransitionMatrix
    val noteTransProb=songLearner.getMelodyTransitionMatrix
    val chordTransProb=songLearner.getChordTransistionMatrix
    val noteStates=List("0","1","2","3","4","5","6","7","8","9","10","11")
    val prechordStates=(0 until chordTransProb.length)
    val chordStates=prechordStates.toList.map(_.toString)
    val preTimeStates=(0 until timeTransProb.length)
    val timeStates=preTimeStates.toList.map(_.toString)

   object probFile {
   val Name: String = melodyMatrixFileName
   val rows: Int =12
   val cols: Int =12
  }
  
  
  val noteHistory: Array[Element[String]]=Array.fill(numIterations)(Constant("0"))
  val intervalHistory: Array[Element[String]]=Array.fill(numIterations)(Constant("0"))
  val timeHistory:Array[Element[String]]=Array.fill(numIterations)(Constant("0"))
  val initialNote = Uniform("0","1","2","3","4","5","6","7","8","9","10","11")
  val initialInterval=Constant("0")
  val InitialTime=Constant("0")
  val initString =initialNote 
  noteHistory(0)=initString
  val newSongFileName=fileNames.newSongFileName
  
  
  

      
  for(songTime<- 1 until numIterations){
    val noteIntPair=modelFunction.transitionNotesIntervals(noteHistory(songTime-1), intervalHistory(songTime-1),
         noteStates,noteTransProb,chordStates,chordTransProb)
    noteHistory(songTime)=noteIntPair._1
    intervalHistory(songTime)=noteIntPair._2
   timeHistory(songTime)=modelFunction.timeTransition(timeHistory(songTime-1),timeStates,timeTransProb)
   
  }
 

  
  val noteHistoryInject=Inject(noteHistory:_*)
  val timeHistoryInject=Inject(timeHistory:_*)
  val intervalHistoryInject=Inject(intervalHistory:_*)
  val alg =Importance(noteHistoryInject,intervalHistoryInject,timeHistoryInject  )

  println("alg start")
  val songSample=alg.sample()._2
  val noteString=songSample(noteHistoryInject)
  val intervalString=songSample(intervalHistoryInject)
  val timeString=songSample(timeHistoryInject)
  val noteList:  ListBuffer[String]=ListBuffer()
  val intervalList:  ListBuffer[String]=ListBuffer()
  val timeList: ListBuffer[String]=ListBuffer()
   
  modelFunction.writeSong(noteString,noteList)
  modelFunction.writeSong(intervalString,intervalList)
  modelFunction.writeSong(timeString,timeList)
  val chordList = intervalList.map( s=> songLearner.getChordFromIndex(s.toInt).map(_.toString ))
  val bins=songLearner.timinggenerator.binVals  
  //println("bins")
  bins.foreach(x=>print(x.toString+" "))
 // println(timeList)
  val times=timeList.map(s=>s.toInt).map(x=>bins(x)).map(s=>s.toString)
 // println(times)
 // println("Here is the melody")
 // println(noteList)
  //println("here are the intervals")

  modelFunction.saveNewSong(newSongFileName,noteList,chordList,times)
   println("song saved to "+newSongFileName)

 
} 
}
