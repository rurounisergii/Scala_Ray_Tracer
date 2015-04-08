sealed trait Shape {
  def normal(v: Vector): Vector

  def intersect(ray: Ray): List[Vector]

  def shine: Double

  def reflect: Double = shine

  def specular: Double = shine

  def colour: Colour
}

case class Sphere(val center: Vector, val radius: Double, val colour: Colour, val shine: Double) extends Shape {

  // normal vector at point v on surface of the sphere
  override def normal(v: Vector): Vector = (v - center).normalized

  override def intersect(ray: Ray): List[Vector] = {
    // Vector to the center
    val vc = center - ray.orig

    // The ray is ray.orig + t * ray.dir
    // We want to solve for t to compute the intersections.

    // coefficients of the quadratic equation for t
    val A = ray.dir.normSquared
    val B = 2.0f * (ray.dir dot vc)
    val C = vc.normSquared - radius * radius

    val disc = B * B - 4.0f * A * C

    if (disc < 0)
      return Nil

    val d = math.sqrt(disc)

    val t0 = (-B + d) / (2.0 * A)
    val t1 = (-B - d) / (2.0 * A)

    val p0 = ray.orig + ray.dir * t0
    val p1 = ray.orig + ray.dir * t1

    List(p0, p1)
  }
}