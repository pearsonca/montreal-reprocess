package edu.cap10

import scala.io.{Source, BufferedSource}
import scala.annotation.tailrec
import scala.Stream.Empty

/**
 * @author carlpearson
 */
package object montreal {
  
  type HotSpotID = Long
  type UserID = Long
  type Day = Long
  type Time = Long
  
  case class PairObservation(
      userA:UserID,userB:UserID,loc:HotSpotID,
      start:Time, end:Time,
      reason:String
  ) {
    override val toString = f"$userA,$userB,$loc,$start,$end,$reason"
  }
  case class Observation(user:UserID, loc:HotSpotID, start:Time, end:Time, reason:String = "background") {
    def notTooLate(other:Observation)(implicit margin:Int) : Boolean = other.start <= (end+margin)
    
    def tooEarly(other:Observation)(implicit margin:Int) : Boolean = (other.end+margin) < start
    
    def intersect(other:Observation) = { //System.out.println(f"$reason intersecting ${other.reason}")
      assert((start <= other.end) & (end >= other.start), this + " vs " + other)
      PairObservation(
        user, other.user, loc,
        Math.max(start, other.start), Math.min(end, other.end),
        if (reason == other.reason) reason else "background"
      )
    }
  }
    
  val refLogins = "./input/merged.o"
  val refLocMap = "./input/reverse-location-id-map.csv" // input has locations j=1...n; line j-1 (0 indexed) gives the associated original location k
  
  lazy val locMap = Source.fromFile(refLocMap).getLines.toArray.map(_.toInt)
  
  def lineParse(line:String) : Observation = {
    val thing = line.split("[ ,]+")
    if (thing.length == 4) {
      val Array(user_id, loc_id, login, logout) = thing.map(_.toLong)
      Observation(user_id, loc_id, login, logout)
    } else {
      val Array(user_id, loc_id, login, logout) = thing.take(4).map(_.toLong)
      Observation(user_id, loc_id, login, logout, thing(4))
    }    
  }
  
  def fileToObsStream(f:BufferedSource) = f.getLines.toStream.map(lineParse)
  def pathToObsStream(path:String) = fileToObsStream(Source.fromFile(path))
  
  val refObservations : Stream[Observation] = pathToObsStream(refLogins)
  
  final def extract(head:Observation, tail:Stream[Observation])(implicit margin:Int) : List[PairObservation] = {
    tail.takeWhile(head.notTooLate).filterNot(head.tooEarly). // assert: no self.user + self.loc in here; if there were, they should have been consumed in preprocess
      filter({ _.loc == head.loc }).map { head.intersect _ }.toList
  }
  
  final def parseOne(head:Observation, tail:Stream[Observation], verbose:Boolean = false)(implicit recorder: List[PairObservation] => Unit, margin:Int = 0) = {
    val reducedTail = tail.dropWhile(head.tooEarly)
    if (verbose) {
      System.err.println("tail ends:")
      System.err.println(tail.take(5).map { _.end } mkString " ")
      System.err.println("head start:")
      System.err.println(head.start)
      System.err.println("tail after drop:")
      System.err.println(reducedTail.take(5).map { _.end } mkString " ")
      scala.io.StdIn.readLine("enter to cont.")
    }
    recorder(extract(head, reducedTail))
    reducedTail
  }
  
  // parse assumes everything in order
  @tailrec
  final def parse(head:Observation, tail:Stream[Observation], verbose:Boolean = false)(implicit recorder: List[PairObservation] => Unit, margin:Int = 0)
    : Unit = if (!tail.isEmpty) {
//      assert(tail.foldLeft((true, head.start))((prev, cur) => {
//        (prev._1 && prev._2 <= cur.start, cur.start)
//      })._1)
    parseOne(head, tail, verbose)
    parse(tail.head, tail.tail, verbose)
  }
  
}