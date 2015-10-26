package edu.cap10.montreal

import scala.io.Source

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
  val reproRoot = args(0)
  val uidOffset = args(1).toInt
  val timeOffset = args(2).toInt
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
    val sampleStream = fileToObsStream(Source.fromFile(file)).map({
      obs => Observation(obs.user + uidOffset, locMap(obs.loc.toInt-1), obs.start + timeOffset, obs.end + timeOffset, obs.reason)
    })
    val cc_printer = new PrintWriter(new File(file.toString.replace(".csv", f"-cc-$window.csv")))
    val cu_printer = new PrintWriter(new File(file.toString.replace(".csv", f"-cu-$window.csv")))
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