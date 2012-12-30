// square root
def abs(x: Double)= if(x >= 0) x else -x
def square(x: Double) = x * x
def average(guess: Double, x: Double) = (guess + x) / 2
def improve(guess: Double, x: Double) = average(guess, x/guess)
def sqroot_it(guess: Double, x: Double) : Double = {
	val nextGuess = improve(guess,x)
	if(abs(guess-nextGuess) > 0.01) sqroot_it(nextGuess,x)
	else guess
}
def sqroot(x: Double) = sqroot_it(1,x)

// fibonacci
def fib_it(a:Int, b:Int, count:Int) :Int =
	if(count == 0) b else fib_it(a + b, a, count - 1)
def fib(x:Int) = fib_it(1, 1, x)

// count number of ways to make a change
val valueOfCoin = Array[Int](0, 1, 5, 10, 25, 50)
def cc(amount: Int, kindsOfCoins: Int): Int = {
	if(amount == 0) 1
	else if(amount < 0 || kindsOfCoins == 0) 0
	else cc(amount, kindsOfCoins - 1) +
		cc(amount - valueOfCoin(kindsOfCoins), kindsOfCoins)
	
}
def count(amount :Int) = cc(amount, 5)

// pascal triangle
def pascal(n: Int, k: Int) :Int = {
	if(k == 0 || k == n) 1
	else (pascal(n - 1, k - 1) + pascal(n - 1, k))
}

// sin function
def cube(x: Double) = x * x * x
def sin_polynomial (x: Double) = 3 * x - 4 * cube(x)
def sin(x: Double) :Double = {
	if(x < 0.001) x
	else sin_polynomial(sin(x/3))
}

val pi = 3.1415926

// fast exponential
def square(x :BigInt) = x * x
def remain(x: Int, y: Int) = x - (x / y) * y
def even(x: Int) = (remain(x,2) == 0)
def fastexp(x: Int, y: Int) :BigInt = {
	if (y == 0) 1
	else (square(fastexp(x, y/2)) * (if(even(y)) 1 else x))
}

// gcd
def gcd(a: Int, b: Int) :Int = if(b == 0) a else gcd(b, a % b)

// exponential mod
def square(x: Int) :Int = x * x
def expmod(x: Int, y: Int, m: Int) :Int = {
	if(y == 0) 1
	else (square(expmod(x,y/2,m)) * (if(even(y)) 1 else x) % m)
} 

// pi sum, the recursion goes too deep and converges slowly
def genTerm(n: Int) :Double = 1.0 / ((4 * n + 1) * (4 * n + 3))
def piSum(n: Int, b: Double) :Double = {
	val a = genTerm(n)
	if(n > b) 0
	else a + piSum(n + 1, b)
}
val Pi = piSum(0, 100) * 8
