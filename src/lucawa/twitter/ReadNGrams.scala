package lucawa.twitter
import scala.io.Source
import scala.collection.immutable.TreeMap
import scala.collection.mutable.HashMap
import clTools.Clogger.log
import java.io.PrintWriter

object ReadNGrams {
  
  clTools.Clogger.setLogLevel(20)
  var countMap = HashMap.empty[String,Int]
  
  def main(args:Array[String]) = {
    val inputPath = args(0)
    val outputPath = if(args.size < 2) "output/trimmedCounts.txt" else args(1)
    trim(inputPath,outputPath,10,10000)
  }
  
  def trim(inputPath:String,outputPath:String,minCount:Int=1,maxLines:Long=10000000):Unit = {
    val source = Source.fromFile(inputPath)("UTF-8")
    val lines = source.getLines()
    
    val out = new PrintWriter(outputPath)
    var added = 0
    val maxLines:Int = 10000
    var i = 0
    while(i<maxLines && lines.hasNext) {
      i=i+1
      val thisLine = lines.next
      val (str,count) = getStringCount(thisLine) 
      if(count > minCount) {
        added=added+1
        //countMap.put(str,count)
        out.println(str + "\t" + count)

        if( added % (maxLines/100) == 0) {
          log("Line %d of %d, added %d, word is %s, count is %d".format(i,maxLines,added,str,count))
        }
      }
    }
    out.close
    println(countMap.get("a"))
  }
  
  private def countLines(filename:String):Long = {
    val source = Source.fromFile(filename)("UTF-8")
    val lines = source.getLines()
    var count:Long = 0;
    while(lines.hasNext) {
      lines.next
      count = count+1;
      if(count % 100000 == 0) {
        log("count: " + count)
      }
    }
    count
  }
  
  private def getStringCount(line:String,delim:String="\t"):(String,Int) = {
    try {
      val splitStr = line.split(delim)
      (splitStr(0),splitStr.tail.map(_.toInt).sum)
    } catch {
      case e:Exception => {
        println("offending line: " + line)
        throw new RuntimeException(e)
      }
    }
  }

}