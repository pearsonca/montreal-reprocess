package edu.cap10.montreal

import scala.io.Source
import scala.annotation.tailrec
import scala.util.parsing.combinator.RegexParsers
import scala.collection.immutable.Stream.Empty

//object shared {  
//  type Time = Long
//  type UserID = Int
//  type LocID = Int
//  case class Observation(user:UserID, loc:LocID, start:Time, end:Time, reason:String = "background") {
//    def contained: Observation => Boolean = (other:Observation) => inGroup(other) && (other match {
//      case Observation(_,_,oStart,oEnd,_) => start <= oStart & oEnd <= end
//    })
//    def inGroup: Observation => Boolean = {
//      case Observation(this.user,this.loc,_,_,_) => true
//      case _ => false
//    }
//    // assert: only applied to list of elements where start <= other.start
//    def overlapping: Observation => Boolean = {
//      case Observation(_, _, start, _,_) => start <= end
//    }
//    def upEnd(newEnd:Time) = Observation(user, loc, start, newEnd)
//  }
//  
//  object ObservationParser extends RegexParsers {
//    def uid: Parser[UserID] = """\d+""".r ^^ { _.toInt }
//    def lid: Parser[LocID] = """\d+""".r ^^ { _.toInt }
//    def time: Parser[Time] = """\d+""".r ^^ { _.toLong }
//    def obs: Parser[Observation] = 
//      (uid~lid~time~time) ^^ { case u~l~s~e => Observation(u,l,s,e) }
//    def apply(input:String) : Observation = 
//      parseAll(obs, input) match {
//      case Success(obs,_) => obs
//      case _ => Observation(-1,-1,-1,-1)
//    }  
//  }
//  
//  def parseFile(path:String) : Iterator[Observation] = 
//    Source.fromFile(path).getLines.map( ObservationParser(_) )
//  
//  case class PairObs(userA:UserID, userB:UserID, start:Time, end: Time)
// 
//  def intersect(one:Observation, two:Observation) : PairObs = 
//    PairObs(one.user, two.user, Math.max(one.start, two.start), Math.min(one.end, two.end))
//  
//}


/**
 * @author carlpearson
 */
object Reprocess extends App {
  // plan:
//  have merged.o
//  have series of sim outputs
//  have paired.o on base data
//  want paired.cc (covert-covert) for each sim output
//  want paired.cu (covert-regular) for each sim output
//  so:
//   make merged.o -> series of observations (once) - binarize?
//   supercomputer over sim types
//   for each type, specify root
  
  if (args.length == 1) { // reprocess original data
//    import shared._
    // TODO move the rest of that commented code here
    // get the paired.o version working
    
  } else {
    val reproRoot = args(0)
    val timeOffset = args(1).toInt
    val tardir = args(2)
    implicit val window = if (args.length == 4) args(3).toInt else 0
  
    import java.io._
  
    def recorder(pw:PrintWriter) = {
      (l:List[PairObservation]) => {
        l foreach {
          pw println _
        }
      }
    }
  
    for {
      file <- new File(reproRoot).listFiles.filter({ f => {
        val nm = f.getName
        !(nm.endsWith("cc.csv") || nm.endsWith("cu.csv"))
      }  }).toIterator if file.isFile
    } {
      println(file)
      val sampleStream = fileToObsStream(Source.fromFile(file)).map({
        obs => Observation(-obs.user, locMap(obs.loc.toInt-1), obs.start + timeOffset, obs.end + timeOffset, obs.reason)
      })
      println(file.toString.replace(".csv", f"-$window-cc.csv").replaceAll(".+/", tardir))
      val cc_printer = new PrintWriter(new File(file.toString.replace(".csv", f"-$window-cc.csv").replaceAll(".+/", tardir)))
      val cu_printer = new PrintWriter(new File(file.toString.replace(".csv", f"-$window-cu.csv").replaceAll(".+/", tardir)))
      val cc_rec = recorder(cc_printer)
      val cu_rec = recorder(cu_printer)
  
      // now intersect sampleStream w/ self -> cc pairs
      // intersect sampleStream w/ ref -> cu pairs
  
      parse(sampleStream.head, sampleStream.tail)(cc_rec)
      cc_printer.flush()
      cc_printer.close()
  
      sampleStream.foldLeft(refObservations)({
        (remainingstream, cov) => parseOne(cov, refObservations)(cu_rec)
      })
  
      cu_printer.flush()
      cu_printer.close()
  
  
    }
  }

}

//package edu.cap10.cora.demo
//
//import scala.annotation.tailrec
//import scala.io.Source
//import scala.util.parsing.combinator.RegexParsers
//import scala.collection.immutable.Stream.Empty
//
//import shared._
//
//object MontrealReprocess {
//
//
//
//  def parseFile(path:String) : Iterator[Observation] = for (
//      line <- Source.fromFile(path).getLines
//    ) yield ObservationParser(line)
//
//  def merge(baseStream:Stream[Observation], path2:String): Stream[Observation] = {
//    val s2 = parseFile(path2).toStream
//
//    def mergeStreams(s1: Stream[Observation], s2: Stream[Observation]): Stream[Observation] = {
//      if (s1.isEmpty) s2
//      else if (s2.isEmpty) s1
//      else if (s1.head.start < s2.head.start) s1.head #:: mergeStreams(s1.tail, s2)
//      else s2.head #:: mergeStreams(s1, s2.tail)
//    }
//
//    mergeStreams(baseStream, s2)
//  }
//
//  import java.io.{File, FilenameFilter}
//  
////  def main(args:Array[String]) : Unit = {
////    val baseStream = parseFile("/Users/carlpearson/Dropbox/montreal/merged.o").toStream // args(0)
////    val mergeSrc = "/Users/carlpearson/scala-commsim/simdata" // args(1)
////    val dir = new File(mergeSrc)
////    for (src <- dir.list().filter( f => f.endsWith(".sim") )) {
////      val fw = new java.io.PrintWriter(mergeSrc+"/"+src.replace(".sim", ".cu"))
////      implicit val recorder = (obs:List[PairObs]) => {
////        obs foreach {
////          p => fw.println(p.userA+" "+p.userB+" "+p.start+" "+p.end)
////        }
////        fw.flush
////      }
////      parseFile(mergeSrc+"/"+src).foreach( o => parseOne(o, baseStream.filter(b => o.start < b.start )) )
////      fw.flush()
////      fw.close()
////    }
////    
////    // iterate over file in mergeSrc that match .sim
////    // parse(merge(basefile, simfile)) // with fw corresponding to .sim filename, but .sim replaced w/ .uu
////
////    // TODO receive a margin-of-error separate for overlaps
////    // val oname = "/Users/carlpearson/Dropbox/epidemics4share/paired.o" // args(2)
////    
////  }
//
//  final def extract(head:Observation, tail:Stream[Observation]) : List[PairObs] = {
//    tail.takeWhile(head.overlapping). // assert: no self.user + self.loc in here; if there were, they should have been consumed in preprocess
//      filter({ _.loc == head.loc }).map { intersect(_, head) }.toList
//  }
//
//  final def parseOne(x:Observation, rest:Stream[Observation])(implicit recorder: List[PairObs]=>Unit ) = {
//    recorder(extract(x, rest))
//  }
//  
//  @tailrec
//  final def parse(stream:Stream[Observation])(implicit recorder: List[PairObs]=>Unit )
//    : Unit = stream match {
//      case x #:: Empty => Unit
//      case x #:: rest => {
//        recorder(extract(x, rest))
//        parse(rest)
//      }
//  }
//
//}
