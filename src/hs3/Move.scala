package hs3

class Move(val row: Int, val column: Int, val value: Int) {
  
	require(row >= 0 && row < Board.size, "row=" + row)
	require(column >= 0 && column < Board.size, "column=" + column)
	require(value > 0 && value <= Board.size, "value=" + value)
	
	override def toString = "(" + row + "," + column + ") : " + value
}