val table = Array(Array(0,0,0,0,0), // 0 false
		  Array(0,2,0,0,7), // 1 numbers eof
		  Array(0,2,3,5,7), // 2 numbers e . eof
		  Array(0,4,0,0,0), // 3 numbers
		  Array(0,4,0,5,7), // 4 numbers e eof
		  Array(0,6,0,0,0), // 5 numbers
		  Array(0,6,0,0,7), // 6 numbers eof
		  Array(7,7,7,7,7)) // 7 true
def isNumber(s: String) = {
	def isNumber_helper(index: Int, stat: Int) :Boolean = {
		val typeOfChar = {
			if(index > s.length - 1) 4 // eof
			else {
				val c = s(index)
				if(c <= '9' && c >= '0') 1 // number
				else if(c == '.') 2 // .
				else if(c == 'e') 3 // e
				else 0
			}
		}
		if(stat == 7) true
		else if(stat == 0) false
		else isNumber_helper(index + 1, table(stat)(typeOfChar))
	}
	isNumber_helper(0,1)
}


val v = "123.456e8"
val test = isNumber(v)

	