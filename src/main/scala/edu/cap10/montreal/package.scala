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
    def overlapping(other:Observation)(implicit margin:Int) : Boolean = other.start <= end+margin
    def tooEarly(other:Observation)(implicit margin:Int) : Boolean = other.end + margin < start
    def intersect(other:Observation) = { //System.out.println(f"$reason intersecting ${other.reason}")
      PairObservation(
        user, other.user, loc,
        Math.max(start, other.start), Math.min(end, other.end),
        if (reason == other.reason) reason else "background"
      )
    }
  }
    
  val refLogins = "./input/merged.o"
  val refLocMap = "./input/locRef.csv" // row i => reduced location j = i+1 => original location k
  
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
    tail.takeWhile(head.overlapping). // assert: no self.user + self.loc in here; if there were, they should have been consumed in preprocess
      filter({ _.loc == head.loc }).map { head.intersect _ }.toList
  }
  
  final def parseOne(head:Observation, tail:Stream[Observation])(implicit recorder: List[PairObservation] => Unit, margin:Int = 0) = {
    val reducedTail = tail dropWhile head.tooEarly
    recorder(extract(head, reducedTail))
    reducedTail
  }
  
  // parse assumes everything in order
  @tailrec
  final def parse(head:Observation, tail:Stream[Observation])(implicit recorder: List[PairObservation] => Unit, margin:Int = 0)
    : Unit = if (!tail.isEmpty) {
    parseOne(head, tail)
    parse(tail.head, tail.tail)
  }
  
}