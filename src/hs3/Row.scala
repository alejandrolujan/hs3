package hs3

class Row(val index: Int) extends CellContainer {
	def assignValue(column: Int, value: Int) = cells(column).value = value
	
	override def toString = {
		val sb = new StringBuilder
		
		for(cell <- cells) sb.append(cell.value.getOrElse('.'))
		sb.insert(6, " ").insert(3, " ")
		
		sb.toString
	}
}