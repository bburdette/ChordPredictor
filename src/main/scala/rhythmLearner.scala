//package musicProcessing
//
//import scala.collection.breakOut
//import com.cra.figaro.algorithm._
//import com.cra.figaro.algorithm.factored.MPEVariableElimination
//import com.cra.figaro.algorithm.factored.MPEVariableElimination
//import com.cra.figaro.language._
//import com.cra.figaro.language.Universe
//import com.cra.figaro.library.atomic.continuous.AtomicDirichlet
//import com.cra.figaro.library.atomic.continuous.Dirichlet
//import com.cra.figaro.algorithm.learning.EMWithVE
//import com.cra.figaro.algorithm.learning.EMWithBP
//
///**
// * @author cLennon
// */
//object rhythmLearner {
//  
//  def main(args: Array[String]) {
//  // val dir11=Dirichlet(2.0)
//
//  val statePairs=List(("1","2"),("1","1"),("2","2"),("2","1"))
//  val param=List.fill(4)(Dirichlet(2.0, 2.0)("d", Universe.universe))
//  val pmap:Map[Tuple2[String,String],Element[Array[Double]]]=(statePairs zip param)(breakOut)
//  val states=List("1","2")
//  val observations=List("1","2","2","2","2","1","2","1","2","1","2","2","2","1","2","1","2","1","2","1")
// println("before any obs")
// 
//  
////  def rhythmTrans(last0: Element[String],last1:Element[String],pmap:
////  Map[Tuple2[String,String],Element[Array[Double]]], states: List[String]){
////    Chain(^^(last0,last1),(pair:(String,String))=>{Select(pmap(pair).asInstanceOf[AtomicDirichlet],states:_*)})
//// }
//  def makeObserve(obsStates:List[String],pmap:  Map[Tuple2[String,String],Element[Array[Double]]], 
//      states: List[String]){
//    val L=obsStates.length-1
//    for(i<- 1 until L){
//      val last0=obsStates(i-1)
//      val last1=obsStates(i)
//      val current=obsStates(i+1)
//      println(last0+" "+last1+" "+current)
//      val param = pmap((last0,last1) ).asInstanceOf[AtomicDirichlet]
//      println("parameter name: " + param.name.string)
//      println("states: " + states)
//      val x=Select(pmap((last0,last1) ).asInstanceOf[AtomicDirichlet],states:_*)
//      x.observe(current)
//    }
//  }
// 
// 
// 
// makeObserve(observations,pmap,states)
// println("after obs before est")
//
// // parameterList.foreach(p=> filewriter.println(p.name+ " "+ alg.mostLikelyValue(p))
//        
// val d1 = Dirichlet(2.0,2.0)
// val s1 = Select(d1, "1", "2")
// s1.observe("1")
//  val algMap=EMWithBP(2,5,param:_*)
//   algMap.start()
//  // parameterList.foreach(p=> filewriter.println(p.name+ " "+ alg.mostLikelyValue(p))
////    
//   println("after alg")
////   pmap.map{ case (k,v) => algMap.mostLikelyValue(v).foreach(x => print(x + " "))
////   println()  
//   param.foreach(x => {x.MAPValue.foreach(a => print(a + " ")); println()})
//   
//   //}
//  // modelFunction.printParams(pmap)
//  }
//  
//  
//}