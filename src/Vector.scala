object Vector {
  val origin = Vector(0, 0, 0)
  val x = Vector(1, 0, 0)
  val y = Vector(0, 1, 0)
  val z = Vector(0, 0, 1)
}

case class Vector(x: Double, y: Double, z: Double) {
  def +(v: Vector) = Vector(x + v.x, y + v.y, z + v.z)

  def -(v: Vector) = Vector(x - v.x, y - v.y, z - v.z)

  def *(a: Double) = Vector(x * a, y * a, z * a)

  def /(a: Double) = Vector(x / a, y / a, z / a)

  def unary_- = Vector(-x, -y, -z)

  def cross(v: Vector) =
    Vector(y * v.z - z * v.y,
      z * v.x - x * v.z,
      x * v.y - y * v.x)

  def dot(v: Vector) = x * v.x + y * v.y + z * v.z

  def normSquared: Double = dot(this)

  // Get the 2-norm (i.e., the length)
  def norm: Float = math.sqrt(normSquared).toFloat

  // Normalize so length is 1.
  def normalized = this / norm
}
