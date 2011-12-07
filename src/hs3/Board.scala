package hs3

import Board._
import com.weiglewilczek.slf4s.Logging

class Board extends Logging {
  
	private var rows = List[Row]()
	private var columns = List[Column]()
	private var regions = List[Region]()
	
	// Initialize rows columns and regions
	for(i <- 0 until size){
		rows = rows :+ new Row(i+1)
		columns = columns :+ new Column(i+1)
		regions = regions :+ new Region
	}
	
	// Initialize each cell
	for(i <- 0 until size){
		for(j <- 0 until size){
			val region = 3*(i/3) + (j/3)
			new Cell(regions(region), rows(i), columns(j))
		}
	}
		
	def move(m: Move) = rows(m.row).assignValue(m.column, m.value)
	
	def solved = rows.filter(r => !r.isSolved).isEmpty

	def solve = {
		var updated = false
		var isSolved = false
		
		do{
			updated = false
			
			for(row <- rows) 
				updated |= row.findNakedTuples || row.findHiddenTuples
			
			for(column <- columns) 
				updated |= column.findNakedTuples || column.findHiddenTuples
				
			for(region <- regions)
				updated |= region.findNakedTuples || region.findHiddenTuples
		
			isSolved = solved
			
		} while(!isSolved && updated)
		  
		isSolved
	}
	
	override def toString = rows.mkString("\n")
	
}

object Board {
	val size = 9

	/** Returns a board built from a standard line representation. See test/tiny.txt for an example */
	def fromLine(line: String) = {
		val board = new Board
		
		for(i <- 0 until line.length){
			val row = i / size
			val column = i % size
			val value = line.charAt(i)
			
			if(value!='.')
				board.move(new Move(row, column, value-48))
		}
		
		board
	}
}