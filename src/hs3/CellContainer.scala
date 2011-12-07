package hs3

import scala.collection.mutable.Set
import com.weiglewilczek.slf4s.Logging
import Board._

abstract class CellContainer extends Logging {
  
	var cells = List[Cell]()
	
	def addCell(cell: Cell) { cells = cells :+ cell }
	
	def discard(discard: Int, initiator: Cell) = cells.filterNot(_ == initiator).foreach(c => c.discard(discard))
	
	/** Returns true if all values are represented in this container. This, combined with the fact that there are as many values as the size of this container, ensures this container is solved. */
	def isSolved: Boolean = {
		// Get all the values in this container
		val valueSet = Set[Int]()
		cells.filter(_.solved).foreach(c => valueSet.add(c.value.get))
		
		valueSet.size == Board.size
	}
	
	/** Finds naked tuples in this container, and returns true if the container was updated, false otherwise */
	def findNakedTuples = {
		var updated = false
		
		for(cell <- cells.filter(!_.solved)) {
			findNakedTuple(cell.potentialValues) match {
				case Some(tuple) =>
					// Found a naked N-tuple, remove values from other cells in container
				  	logger.debug("Found naked tuple: " + tuple.mkString(","))
				  	for(nonTupleCell <- (cells -- tuple.toList).filterNot(_.solved))
				  		updated |= nonTupleCell.discard(cell.potentialValues)
				case _ =>
			}
		}
		
		updated
	}
	
	private def findNakedTuple(values: Set[Int]) = {
	  
		val tuple = cells.filter(_.potentialValues == values)
		
		if(tuple.size == values.size) 
			Some(tuple) 
		else
			None
	}
	
	/** Finds hidden tuples in this container and return true if the container was updated, false otherwise */
	def findHiddenTuples = {
		var updated = false
		var values = Set[Int]()
		
		// Hidden tuples of size > 3 yield almost zero gain and double the processing time
		for(combSize <- 2 to 3) {
			for(combination <- (1 to size).combinations(combSize)) {
				val values = Set[Int]() ++ combination
				findHiddenTuple(values) match {
					case Some(twin) =>
					  	logger.debug("Found hidden tuple: " + twin.mkString(",") + " with values " + values.mkString(","))
					  	updated |= processHiddenTuple(twin, values)
					case _ =>
				}
			}
		}
		
		updated
	}
	
	private def findHiddenTuple(values: Set[Int]) = {
		val tuple = Set[Cell]()
		var containedValues = Set[Integer]()
		
		for(cell <- cells.filterNot(_.solved)) {
			val intersection = cell.potentialValues & values 
			if(!intersection.isEmpty) {								// If cell contains at least one of values
				tuple.add(cell)										// it belongs in the tuple
				intersection.foreach(v => containedValues.add(v)) 
			}
		}
			  
		if(tuple.size == values.size && values.size == containedValues.size) 
			Some(tuple) 
		else 
			None 
	}
	
	def processHiddenTuple(tuple: Set[Cell], values: Set[Int]) = {
		var updated = false
		
		for(cell <- tuple)
			updated |= cell.discardAllBut(values)
		
		updated
	}
}