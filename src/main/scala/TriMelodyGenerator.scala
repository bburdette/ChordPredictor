// Henry Cooney <hacoo36@gmail.com>
// Ben Burdette <bburdette@gmail.com>
// Chord Predictor -- https://github.com/bburdette/ChordPredictor/
//
// Melody Generator
//
// Melody generation module. Learns 'melody' from underlying chords and
// generates new melodies. Chords may be overlayed onto this.
package TriMelodyGenerator

//import com.cra.figaro.language._
import musicProcessing._
import musicProcessing.Chord
import java.io._

object TriMelodyGenerator_Test{ 
  def main(args: Array[String]) {
    
    var test = new TriMelodyCounter
    test.addChord(new Chord(Array(100,6)))
    test.addChord(new Chord(Array(100,7)))
    test.addChord(new Chord(Array(100,6)))
    test.addChord(new Chord(Array(100,8)))
    test.addChord(new Chord(Array(100,6)))
    test.addChord(new Chord(Array(100,4)))
    test.addChord(new Chord(Array(100,6)))
    test.addChord(new Chord(Array(100,1)))
    test.addChord(new Chord(Array(100,8)))
    test.addChord(new Chord(Array(100,11)))
    test.addChord(new Chord(Array(100,9)))
    test.addChord(new Chord(Array(100,7)))
    test.addChord(new Chord(Array(100,2)))
    
    test.saveToFile("test.txt")

    var test2 = new TriMelodyCounter
    // test.txt and test2.txt should end up being the same.
    test2.readFromFile("test.txt")
    test2.saveToFile("test2.txt")

    println()
    println()
  }   
}

class TriMelodyCounter (){
 
  // If the current note is at '0', this transition
  // matrix goes from prev to next 

  val transitions = Array.fill(12, 12)(0)
  var totaltransitions = 0

  var currentChord = new Chord(Array.fill(2)(0))
  var hasCurrent = false
  var prevChord = new Chord(Array.fill(2)(0))
  var hasPrev = false

  def addChord(nextChord: Chord) {
    if (hasCurrent == false) {
      currentChord = nextChord
      hasCurrent = true
      return;
    }
    if (hasPrev == true) {
      // Ok, now compute the prev and the next, considering current to be zero.
      var prev = (prevChord.getRoot - currentChord.getRoot) % 12
      if (prev < 0) {
        prev = prev + 12
      }
      var next = (nextChord.getRoot - currentChord.getRoot) % 12
      if (next < 0) {
        next = next + 12
      }

      println("prev: " ++ prev.toString)

      transitions(prev)(next) += 1
      totaltransitions += 1
    }
    else
    {
      // we'll update the prevChord below.
      hasPrev = true
    }

    // move the current chord to prev,
    // and the next chord to current.
    prevChord = currentChord
    
    currentChord = nextChord
  }

  def saveToFile(fileName: String){
    val file = new File(fileName)
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(totaltransitions.toString + "\n")
    for(n<-transitions){
       bw.write(n.mkString(",")+"\n")
    }
    bw.close()  
  }

  def readFromFile(fName:String){
    
    val bufferedSource = io.Source.fromFile(fName)

    val lines = bufferedSource.getLines
    val meh = lines.next
    // totaltransitions = Try(meh.toInt).getOrElse(0.0)
    totaltransitions = meh.toInt

    // lines.next
    for ((line, count) <- lines.zipWithIndex) {
        transitions(count) = line.split(",").map(_.trim).map(x => x.toInt) 
        //warning.. will convert string to int? 
        //or will replace by 0 (in case on Na or "" or other)
      }
    bufferedSource.close
  }
}

class TriMelodyGenerator(tmc: TriMelodyCounter) {
  val normalizedtransitions = tmc.transitions.map(_.map((x: Int) => x.toDouble/tmc.totaltransitions))
  
  def computeNextNote(prev: Int, current: Int) {
  }
  
}
