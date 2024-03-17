//esercizio 3 ______________________________________________________________
//a: funzione
val positive: Int => String = n => n  match
    case n if n >= 0 => "positive"
    case _ => "negative"

//a: metodo
def positive(n: Int): String = n match
    case n if n>=0 => "positive"
    case _ => "negative"
	
//b: funzione
val empty: String => Boolean = s => s == ""
val neg: (String => Boolean) => (String => Boolean) = f => !f(_)
val notEmpty = neg(empty)

//b: metodo
val empty: String => Boolean = s => s == ""

def neg(f: String => Boolean): (String => Boolean) = 
        s => !f(s)

val notEmpty = neg(empty)

//c: metodo generico
val empty: String => Boolean = s => s == ""

def neg[X](f: X=>Boolean): X => Boolean = 
    s => !f(s)
val notEmpty = neg[String](empty)
println(notEmpty(""))

//esercizio 4 ______________________________________________________________

//versione funzione curried
val checker: (Int) (Int) (Int) => Boolean = x,y,z => x<=y && y==z

//versione funzione non-curried
val checker: (Int, Int, Int) => Boolean = (x,y,z) => x<=y && y==z

//versione metodo curried
def checker(x: Int)(y: Int)(z: Int): Boolean =
    x<=y && y==z

//versione metodo non-curried
def checker(x: Int, y: Int, z: Int): Boolean =
    x<=y && y==z

//esercizio 5 ______________________________________________________________
def compose(f: Int=> Int, g: Int=> Int): Int=> Int =
    i=>f(g(i))
compose(_ - 1,_ * 2)(5)

//generico
def compose[A](f: A=> A, g: A=> A): A=> A =
    i=>f(g(i))
compose[String](_+"a",_+"b")("c")

//esercizio 6 ______________________________________________________________
def gcd(a: Int, b: Int): Int = (a,b) match
    case (a,b) if a<b => gcd(b,a)
    case (a,b) if b==0 => a
    case _ => gcd(b, a % b)

//esercizio 7 ______________________________________________________________
enum Shape:
	case Square(side: Double)
	case Rectangle(height: Double, width: Double)
	case Circle(radius: Double)

object Shape:
	def perimeter(shape: Shape): Double = shape match
		case Square(side) => side * 4
		case Rectangle(height, width) => height * 2 + width * 2
		case Circle(radius) => radius * 6.28

	def scale(shape: Shape, alpha: Double): Shape = shape match
		case Square(side) => Square(side*alpha)
		case Rectangle(height, width) => Rectangle(height*alpha, width*alpha)
		case Circle(radius) => Circle(radius*alpha)


//esercizio 8 ______________________________________________________________
//implementazione map e filter

def map[A, B](optional: Optional[A], f: A => B): Optional[B] = optional match
  case Maybe(value) => Maybe(f(value))
  case _ => Empty()

def filter[A](optional: Optional[A], f: A => Boolean): Optional[A] = optional match
  case Maybe(value) if f(value)==true => Maybe(value)
  case _ => Empty()
  
//test
 @Test def mapShouldReturnEmptyWhenEmpty(): Unit = {
    val empty: Optional[Int] = Optional.Empty()
    val result = Optional.map(empty, _ + 1)
    assertTrue(Optional.isEmpty(result))
  }

  @Test def mapShouldReturnTransformedValueWhenNonEmpty(): Unit = {
    val nonEmpty = Optional.Maybe(0)
    val result = Optional.map(nonEmpty, _ + 1)
    assertEquals(1, Optional.orElse(result, 1))
  }

  @Test def filterShouldReturnEmptyWhenEmpty(): Unit = {
    val empty: Optional[Int] = Optional.Empty()
    val result = Optional.filter(empty, _ > 1)
    assertTrue(Optional.isEmpty(result))
  }

  @Test def filterShouldReturnEmptyWhenNonEmptyBecauseOfCondition(): Unit = {
    val nonEmpty = Optional.Maybe(0)
    val result = Optional.filter(nonEmpty, _ > 1)
    assertTrue(Optional.isEmpty(result))
  }

  @Test def filterShouldReturnFilteredValue(): Unit = {
    val nonEmpty = Optional.Maybe(10)
    val result = Optional.filter(nonEmpty, _ > 1)
    assertEquals(10, Optional.orElse(result, 0))
  }
