class Image(val width: Int, val height: Int) {
  import java.awt.image.BufferedImage
  import java.io.File
  import javax.imageio.ImageIO

  val im = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)

  def apply(x: Int, y: Int) = Colour.fromRGB(im.getRGB(x, y))

  def update(x: Int, y: Int, c: Colour) = {
    im.setRGB(x, y, c.rgb)
  }

  def print(file: String) = {
    val f = new File(file)
    ImageIO.write(im, "png", f)
  }
}
