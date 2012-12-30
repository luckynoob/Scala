val list = Array[Int](1,3,4,5,7,9)

def powerSet(l: Array[Int]) = {
	def select(i: Int, n:Int) = 
		for(j <- 0 to n if((i & (1 << j)) != 0)) yield l(j)
	val n = l.length - 1
	val p = (1 << (n+1)) - 1
	val power = for(i <- 0 to p) yield select(i,n).mkString(" ")
	power.mkString("\n")
}