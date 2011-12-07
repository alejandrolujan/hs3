package hs3

import java.io._
import scala.io.Source
import com.weiglewilczek.slf4s.Logging

object Samples extends Logging {
	val tiny =    "test/tiny.txt"		// 5 problems, test box
	val small =   "test/small.txt"		// 244 problems
	val large =   "test/msk_009.txt"	// 1012 problems, very hard
	val largest = "test/subig20.txt"	// 17445 problems, hard
	  
	def main(args: Array[String]) {
		var attempts = 0
		var success = 0
		var maxattempts = 100000
		var startTime = System.currentTimeMillis
		
		for(line <- Source.fromFile(small).getLines) {
			attempts += 1
			val board = Board.fromLine(line.trim)
			val solved = board.solve
			
			if(solved) {
				logger.info(attempts + ": Solved :)")
				success += 1
			} else {
				logger.info(attempts + ": Stuck :(")
			}
		}
		
		val total = System.currentTimeMillis - startTime
		
		logger.info("Solved " + success + "/" + attempts + " in " + total/1000 + " secs")
		logger.info("That's " + (100.0*success/attempts) + "%")
		logger.info("Avg processing time: " + (total/attempts) + "ms")
	}
}