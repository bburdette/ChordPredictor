package musicProcessing

/**
 * @author cLennon
 */
object main {
  def main(args:Array[String]){ 
    val dirName="C:/Users/cLennon/Documents/GitHub/ChordPredictor/midi2csv"
  //  println(dirName)
   val  musicSet= new musicDataSet(dirName)
//   print(musicSet)
   musicSet.printCSV
   for( s <- musicSet.songList ){ 
     val csv=s.getCSV().toList
     for(row<-csv){
   
    // println("csv row value")
   //  println(row.mkString(" "))
     val chord=new Chord(row)
     val intervalsPresent=chord.checkIntervals()
     println("Intervals Present")
      println(intervalsPresent)
   }}    
   
  
  }
   
}
