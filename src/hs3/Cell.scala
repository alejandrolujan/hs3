package hs3

import scala.collection.mutable.Set
import com.weiglewilczek.slf4s.Logging

class Cell(region: Region, row: Row, column: Column) extends Logging {
	
	//private var _value: Option[Int] = None
	val potentialValues = Set[Int]()

	// Add all potential values
	for(i <- 1 to Board.size) potentialValues.add(i)
		
	// Register this cell with the containers
	region.addCell(this)
	row.addCell(this)
	column.addCell(this)
	
	/** Returns the actual value of this cell, None if it's still undefined (more than one potential value) */
	def value = { 
		if(potentialValues.size == 1) 
			Some(potentialValues.head) 
		else 
			None
	}
			
	/** Assigns newValue to this cell, cascading the corresponding discards to all the cell containers this belongs to. */
	def value_=(newValue: Int) {
		require(potentialValues(newValue), "Can't assign impossible value " + newValue + " to " + this)

		// Register new value and clean up potentials
		potentialValues.clear()
		potentialValues.add(newValue)
		
		// Cascade discard my new value on all the containers I belong to
		region.discard(newValue, this)
		column.discard(newValue, this)
		row.discard(newValue, this)
	}
	
	/** Removes a value from the potential values of this cell. Returns true if the cell was modified, false otherwise. */
	def discard(discard: Int) = {
		require(value != Some(discard), "Can't discard actual value from " + this)
		
		var updated = false
		
		if(potentialValues.remove(discard)) {
			updated = true
			if(potentialValues.size == 1)
				value = potentialValues.head
		}
		
		updated
	}
	
	def discard(values: Set[Int]): Boolean = {
		var updated = false
		values.foreach(value => updated |= discard(value))
		updated
	}
	
	def isPotential(i: Int) = potentialValues(i)
	
	def solved = potentialValues.size == 1

	/** Removes all potential values except for those found in _values_. Returns true if the cell was modified, false otherwise. */
	def discardAllBut(values: Set[Int]) = {
		var updated = false
		
		for(value <- potentialValues -- values)
			updated |= discard(value)
		
		updated
	}
	
	override def toString = "(" + column.index + "," + row.index + ", [" + potentialValues.toList.sort((e1,e2)=>(e1<e2)).mkString(",") + "])" 
}