def match_KMP(t: String, p: String) = {
	val m = p.length
	val kmp = new Array[Int](m)
	kmp(0) = -1
	def recurse(k:Int, c:Char) :Int = {
		if(p(k+1) == c) k + 1
		else if(k == -1) -1
		else recurse(kmp(k), c)
	}
	for(q <- 1 until m) kmp(q) = recurse(kmp(q-1),p(q))

	val n = t.length
	var count = -1
	for(i <- 0 until n) {
		count = recurse(count, t(i))
		if(count == m-1) {
			count = kmp(count)
			println("Index at: " + (i-count))
		}
	}
}