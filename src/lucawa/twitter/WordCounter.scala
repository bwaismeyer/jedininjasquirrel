package lucawa.twitter
import clTools.WeightedCollection
import scala.io.Source
import java.io.File

object WordCounter {

  val masterCounts = new WeightedCollection[String](true,false)
  var initialized = false;
  
  def main(args:Array[String]) = {
    init
  }
  
  def init = {
    val files = new File("output").listFiles().filter(f => f.isFile() && f.getName().startsWith("slice") ) //Vector("output/slice-#.txt")
    var lineNum:Long = 0 
    for(f <- files) {
      val source = Source.fromFile(f)
      val lines = source.getLines
      while(lines.hasNext) {
        val line:IndexedSeq[String] = lines.next.split("\t")
        masterCounts.add(cleanWord(line(0)),line(1).toDouble)
        lineNum = lineNum+1
        if(lineNum % 100000 == 0) {
          println("Processed line number %d, file is %s, counts has %10.0f elems".format(lineNum,f.toString,masterCounts.totalWeight))
        }
      }
    }
    initialized=true
    //println("Processed line number %d, counts has %10.0f elems, %10d keys".format(lineNum,masterCounts.totalWeight,masterCounts.getItems.size))
  }
  
  def cleanWord(w:String) = {
    w.toLowerCase().replaceAll("[\\.\\!\\?,'`\"]*","")
  }
  
  /**
   * Returns counts for all of the words in the IndexedSeq.
   * 
   * Currently a stub -- does not actually work, but instead returns all zeros.
   * 
   */
  def countWords(ws:IndexedSeq[String]):IndexedSeq[Int] = {
    for(w <- ws) yield masterCounts.getWeight(cleanWord(w)).toInt
  }
  
}