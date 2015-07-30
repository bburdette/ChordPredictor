package musicProcessing
import scala.collection.mutable.ListBuffer
import scala.util._
import com.cra.figaro.language._
import com.cra.figaro.algorithm._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.library.atomic.discrete.Uniform
import com.cra.figaro.library.atomic.continuous.Dirichlet
import com.cra.figaro.library.collection._
import MelodyGenerator._
import java.io._
import MelodyGenerator.MelodyGenerator
import com.cra.figaro.library.compound.{CPD}
//import scala.collection.immutable.Seq
/**
 * @author cLennon
 */


object modelFunction {
  
 
     def loadProbArray(rws:Int,cols:Int,fName:String): Array[Array[Double]]={
      val probArray= Array.ofDim[Double](rws,cols)
      val bufferedSource = io.Source.fromFile(fName)
      for ((line, count) <- bufferedSource.getLines.zipWithIndex) {
          probArray(count) = line.split(",").map(_.trim).map(x => Try(x.toDouble).getOrElse(0.0)) //warning.. will convert string to double 
          //or will replace by 0 (in case on Na or "" or other)
        }
        bufferedSource.close
        return probArray
      }
  
     
     


 
    def saveNewSong(songName:String,noteList: ListBuffer[String],chordList:ListBuffer[Array[String]],
        timeList: ListBuffer[String] ){
        val tupList=(noteList,chordList,timeList).zipped.toList
        val ntext=tupList.map{case (x:String,y:Array[String],z:String) => z+","+x+","+y.mkString(",")+"\n"}
        val file = new File(songName)
        val bw = new BufferedWriter(new FileWriter(file))
        bw.write(ntext.mkString(""))
        bw.close()
  }
  
    def writeSong(song2Be: Any,newList:ListBuffer[String]){
   
        song2Be.asInstanceOf[List[String]].foreach(newList+=_)
  }
    
  def transitionNotesIntervals(note: Element[String], interval:Element[String],
      noteStates:List[String],noteTransProb: Array[Array[Double]],intStates:List[String],chordTransMtx: Array[Array[Double]]):
      (Element[String], Element[String] )={
    val newNoteState = noteTrans(note, noteStates,noteTransProb)
    val  newIntervalState=chordTrans(interval,chordTransMtx,intStates)
    (newNoteState, newIntervalState)  
  } 
  
  
  def noteTrans(oldNote: Element[String],noteStates:List[String],noteTransProb: Array[Array[Double]]):Element[String]={
    val newNote=CPD(oldNote,
    ("0")->Select(noteTransProb(0).toList,noteStates) ,   
    ("1")->Select(noteTransProb(1).toList,noteStates) ,
    ("2")->Select(noteTransProb(2).toList,noteStates) ,
    ("3")->Select(noteTransProb(3).toList,noteStates) ,
    ("4")->Select(noteTransProb(4).toList,noteStates) ,
    ("5")->Select(noteTransProb(5).toList,noteStates) ,
    ("6")->Select(noteTransProb(6).toList,noteStates) ,
    ("7")->Select(noteTransProb(7).toList,noteStates) ,
    ("8")->Select(noteTransProb(8).toList,noteStates) ,
    ("9")->Select(noteTransProb(9).toList,noteStates) ,
    ("10")->Select(noteTransProb(10).toList,noteStates), 
    ("11")->Select(noteTransProb(11).toList,noteStates))
    return newNote
  }
  
  
//  def genChordTransSeq(chordTransMtx: Array[Array[Double]],States:List[String]):
//  Seq[(String,Element[String])]={
//   val cat=chordTransMtx.map(rw=>Select(rw.toList,States)).toSeq
//   val thing1=cat.zipWithIndex
//   val thing2=thing1.map(pair=>(pair._2.toString(),pair._1) )
//    thing2
//  }
  
  
  def chordTrans(oldChord: Element[String],chordTransMtx: Array[Array[Double]],States:List[String]): Element[String]={
    Chain(oldChord,(rw:String)=>Select(chordTransMtx(rw.toInt).toList,States))
  }
  
//  def rhythmTrans(last0: Int,last1:Int){
//    assert(last0<3&last0>0&last1<3&last1>0,"Presently note lengths are 1 short or 2 long")
//    CPD(last0,last1,
//     (1,1,1)->Select(Dirichlet(),stateslist   
//    
//    )
//    
//  val par = Dirichlet(2.0,2.0,2.0)
//  
//      val parList=List(par)
//      val outcomes =List("a","b","c")
//      val s=Select(par,outcomes:_*)
//      s.observe("a")
//      val s1=Select(par,outcomes:_*)
//      s1.observe("a")
//      val alg=MPEVariableElimination()
//      alg.start()
//      parameterList.foreach(p=> filewriter.println(p.name+ " "+ alg.mostLikelyValue(p))
//    
//  }
  
  //val d= Dirichlet(,)
  
}