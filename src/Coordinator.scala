
import akka.actor.Actor
object Coordinator extends Actor{
  def receive = {
    case (x: Int, y: Int, colour: Colour) => set(x, y, colour)
    case (im: Image, of: String) => init(im, of)
    case "print" => print
  }
  def init(im: Image, of: String) = {
    image = im
    outfile = of
    waiting = im.width * im.height
  }

  // Number of pixels we're waiting for to be set.
  var waiting = 0
  var outfile: String = null
  var image: Image = null

  // TODO: make set a message
  def set(x: Int, y: Int, c: Colour) = {
    image(x, y) = c
    waiting -= 1
    if (waiting == 0) print
  }

  def print = {
    //assert(waiting == 0)
    image.print(outfile)
    context.system.shutdown()
  }
}
