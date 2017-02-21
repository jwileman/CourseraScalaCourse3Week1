package scalashop

import java.util.concurrent.ForkJoinTask

import org.scalameter._
import common._

object HorizontalBoxBlurRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 5,
    Key.exec.maxWarmupRuns -> 10,
    Key.exec.benchRuns -> 10,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val radius = 3
    val width = 1920
    val height = 1080
    val src = new Img(width, height)
    val dst = new Img(width, height)
    val seqtime = standardConfig measure {
      HorizontalBoxBlur.blur(src, dst, 0, height, radius)
    }
    println(s"sequential blur time: $seqtime ms")

    val numTasks = 32
    val partime = standardConfig measure {
      HorizontalBoxBlur.parBlur(src, dst, numTasks, radius)
    }
    println(s"fork/join blur time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }
}


/** A simple, trivially parallelizable computation. */
object HorizontalBoxBlur {

  /** Blurs the rows of the source image `src` into the destination image `dst`,
   *  starting with `from` and ending with `end` (non-inclusive).
   *
   *  Within each row, `blur` traverses the pixels by going from left to right.
   */
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
    for (y <- from until end; x <- 0 until src.width) {
      dst.update(x, y, boxBlurKernel(src, x, y, radius))
    }
  }

  /** Blurs the rows of the source image in parallel using `numTasks` tasks.
   *
   *  Parallelization is done by stripping the source image `src` into
   *  `numTasks` separate strips, where each strip is composed of some number of
   *  rows.
   */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    val stripWidth = Math.max(src.height / numTasks, 1) //e.g. stripWidth=11 if src.height=57 and numTasks=5

    //Gives splitting points e.g. for an image of height 57, with 5 strips, gives Range(0, 11, 22, 33, 44)
    val range = (0 to src.height - stripWidth).by(stripWidth)
    val strips = range.zip((range :+ src.height).tail).toList //Gives List((0,11), (11,22), (22,33), (33,44), (44,57))

    var tasks = List[ForkJoinTask[Unit]]()
    for ((from, end) <- strips) {
      val computation = task {
        blur(src, dst, from, end, radius)
      }
      tasks = computation :: tasks
    }

    for (computation <- tasks) {
      computation.join()
    }
  }

}
