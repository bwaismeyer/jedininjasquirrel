package lucawa.twitter
import scala.io.Source
object ReadNGrams {
  
  def main(args:Array[String]) = {
    val filename = args(0)
    val source = Source.fromFile(filename)
    val lines = source.getLines()
    for(i <- 0 until 100) {
      println(lines.next())
    }
  }

}