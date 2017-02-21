
import common._

package object scalashop {

  /** The value of every pixel is represented as a 32 bit integer. */
  type RGBA = Int

  /** Returns the red component. */
  def red(c: RGBA): Int = (0xff000000 & c) >>> 24

  /** Returns the green component. */
  def green(c: RGBA): Int = (0x00ff0000 & c) >>> 16

  /** Returns the blue component. */
  def blue(c: RGBA): Int = (0x0000ff00 & c) >>> 8

  /** Returns the alpha component. */
  def alpha(c: RGBA): Int = (0x000000ff & c) >>> 0

  /** Used to create an RGBA value from separate components. */
  def rgba(r: Int, g: Int, b: Int, a: Int): RGBA = {
    (r << 24) | (g << 16) | (b << 8) | (a << 0)
  }

  /** Restricts the integer into the specified range. */
  def clamp(v: Int, min: Int, max: Int): Int = {
    if (v < min) min
    else if (v > max) max
    else v
  }

  /** Image is a two-dimensional matrix of pixel values. */
  class Img(val width: Int, val height: Int, private val data: Array[RGBA]) {
    def this(w: Int, h: Int) = this(w, h, new Array(w * h))
    def apply(x: Int, y: Int): RGBA = data(y * width + x)
    def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c
  }

  /** Computes the blurred RGBA value of a single pixel of the input image. */
  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
    val min_x = 0
    val max_x = src.width - 1
    val lower_x_bound = clamp(x - radius, min_x, max_x)
    val upper_x_bound = clamp(x + radius, min_x, max_x)

    val min_y = 0
    val max_y = src.height - 1
    val lower_y_bound = clamp(y - radius, min_y, max_y)
    val upper_y_bound = clamp(y + radius, min_y, max_y)

    val no_pixels = (upper_y_bound - lower_y_bound + 1) * (upper_x_bound - lower_x_bound + 1)

    var total_R = 0
    var total_G = 0
    var total_B = 0
    var total_A = 0

    for(_x <- lower_x_bound to upper_x_bound; _y <- lower_y_bound to upper_y_bound) {
      val rgba = src(_x, _y)
      total_R += red(rgba)
      total_G += green(rgba)
      total_B += blue(rgba)
      total_A += alpha(rgba)
    }

    rgba(total_R/no_pixels, total_G/no_pixels, total_B/no_pixels, total_A/no_pixels)
  }

}
