package u02

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
	
import Shape.* 

  @Test def mapShouldReturnEmptyWhenEmpty(): Unit = {
    val empty: Optional[Int] = Optional.Empty()
    val result = Optional.map(empty, _ + 1)
    assertTrue(Optional.isEmpty(result))
  }