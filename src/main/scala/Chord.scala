

/**
 * @author cLennon
 */
class Chord(timeNotes: Array[Int]) {
  import util.Sorting.quickSort
  import scala.collection.mutable.ListBuffer
   import scala.collection.immutable.Map
  val intervals = List( ("major",Array(0,3,7)),("minor",Array(0,4,7)))
 // println(timeNotes.mkString(" "))
 // println(timeNotes.length)
  assert(timeNotes.length>2,"There was only 1 column of the file.")
  val duration = timeNotes(0)
  val notes = timeNotes.tail
  val modNotes = timeNotes.map(_ % 12)
  val ordModNotes=modNotes
  val defaultRoot=ordModNotes(0)
  quickSort(ordModNotes)
  
  val intervalsPresent =checkIntervals().filter(_._2==true)
  
  def getRootFreq(): Int={
    if(intervalsPresent.isEmpty){return defaultRoot}
    else{
      val bases= intervalsPresent.unzip3._3.toArray
      quickSort(bases)
      bases(0)
    }
   }
  
  def getInterval(): String={
    if(intervalsPresent.isEmpty){return "Other"}
    else{
      return intervalsPresent(0)._1
    }
  }
  
  
  def checkIntervals() = {testAllChords(ordModNotes,intervals)} //.filter(_._2==true) }
  
  def testAllChords(ordModNotes: Array[Int],intervals: List[(String,Array[Int])]): List[(String,Boolean,Int)]={
   // println("in test All Chords")
   // println(ordModNotes) 
    ordModNotes.flatMap(base =>testBase(base,ordModNotes,intervals)).toList
  }
  
  def testBase(base: Int,ordModNotes: Array[Int],intervals: List[(String,Array[Int])]):List[(String,Boolean,Int)]={
   // println("in testBase")
    val shiftModNotes = ordModNotes.map(_-base)
    val ordShift = shiftModNotes
      quickSort(ordShift)
     // println(ordShift)
    val tests= testAllIntervals(ordShift,intervals)
    for(t<-tests) yield (t._1,t._2,base)
    
  
  }
  
  def testAllIntervals(ordShift: Array[Int],intervals:List[(String,Array[Int])]): List[(String,Boolean)]={
   // println("in testAllIntervals")
    for(inter<-intervals) yield {(inter._1,checkInterval(ordShift,inter._2))}
  }
  
  def checkInterval(ordShift: Array[Int],interval: Array[Int]): Boolean={
    //println("checkInterval")
   // println(ordShift)
    val L=interval.length
    if(L>ordShift.length){return false}
    else {
     // println("short")
      
      val shortOrd=ordShift.slice(0,L)
     // println(shortOrd)
     // println("interval")
     // println(interval)
       shortOrd==interval
    }
  }
  
  
}