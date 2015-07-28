package musicDataSet

import scala.collection.immutable.List
import scala.collection.mutable.Map
import scala.collection.mutable.ArrayBuffer
import java.io.File



/**
 * @author cLennon
 */

class Song(name: String, csv: ArrayBuffer[Array[Int]], private val rows: Int, private val cols: Int) {
    
  def getCSV()={
    this.csv
  }
  
    def getName()={
       this.name
    } 
    
    def toArray()={
      val temp=Array.ofDim[Int](rows,cols)
      for(j <- 0 until csv.length) {
        for(i <-0 until csv(j).length){
          temp(j)(i)=csv(j)(i)      
        } 
      }
      temp
    } 
  }


class musicDataSet(directory: String)  {

  def printCSV(){
    for(s<-songList) println(s.getCSV.map(_.mkString(" ")).mkString("\n"))
  }
  
  

  val  songList: List[Song]=getDirectory(directory).toList;
  
  override def toString() ={
    val nameArray=songList.map(_.getName)
    nameArray.mkString("\n") 
  }
  
  
  // duration, rootNote, intervals
  
  
      def getSong(f: java.io.File): Song ={
        val csv = ArrayBuffer[Array[Int]]()
        var rows =0
        var cols =0
        val name =f.getName.toString
    val bufferedSource = io.Source.fromFile(f)         
    for (line <- bufferedSource.getLines) {
      rows +=1
      csv += line.split(",").map(_.trim).map(_.toInt)
      if(rows==1){cols=csv(0).length}
    }
    bufferedSource.close()
    new Song(name, csv, rows,cols)
  }
      
    def getAllSongs(fileList: Array[java.io.File]): Array[Song]={
      for(f<-fileList) yield getSong(f)
      }

    
    def getDirectory(dirName: String): Array[Song]={
       val fileNamesList=new java.io.File(dirName).listFiles.filter(_.getName.endsWith(".csv"))
       getAllSongs(fileNamesList)
    }

}


    
    
    
