object Colour {
  val black = Colour(0, 0, 0)
  val red = Colour(1, 0, 0)
  val green = Colour(0, 1, 0)
  val blue = Colour(0, 0, 1)
  val magenta = red + blue
  val cyan = green + blue
  val yellow = red + green
  val white = red + green + blue

  def fromRGB(x: Int) = {
    val a = (x >>> 24) & 255
    val r = (x >>> 16) & 255
    val g = (x >>> 8) & 255
    val b = x & 255
    Colour(r, g, b)
  }
}

case class Colour(r: Double, g: Double, b: Double) {
  def rgbTuple = (up(r), up(g), up(b))

  private def up(x: Double) = (x * 256).toInt max 0 min 255

  def rgb = {
    val (r, g, b) = rgbTuple
    (r << 16) | (g << 8) | b
  }

  def +(c: Colour) = Colour(r + c.r, g + c.g, b + c.b)

  def -(c: Colour) = Colour(r - c.r, g - c.g, b - c.b)

  def *(a: Double) = Colour(r * a, g * a, b * a)

  def /(a: Double) = Colour(r / a, g / a, b / a)

  def mix(c: Colour) = Colour(
    (r * c.r),
    (g * c.g),
    (b * c.b))
}
