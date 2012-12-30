package lucawa.twitter
import scala.io.Source
import scala.collection.immutable.TreeMap
import scala.collection.mutable.HashMap
import clTools.Clogger.log
import java.io.PrintWriter
import scala.collection.immutable.SortedSet

object ReadNGrams {
  
  clTools.Clogger.setLogLevel(20)
  var countMap = HashMap.empty[String,Int]
  
  def main(args:Array[String]) = {
    val inputPath = args(0)
    //val outputPath = if(args.size < 2) "output/trimmedCounts.txt" else args(1)
    //trim(inputPath,outputPath,10,10000)
    mungeStream(inputPath,new PrefixSliceMunger)
  }
  
  trait StreamMunger {
    def isDone:Boolean
    def munge(line:String):Unit
    def close:Unit
  }
  
  class PrefixSliceMunger(val outPrefix:String="output/slice") extends StreamMunger {
    val prefixChars = "abcdefghijklmnopqrstuvwxyz01234567890@#"
    val prefixes = prefixChars.toCharArray().toIndexedSeq.map((c:Char) => c.toString)
    val prefixToWriter = prefixes.map((s:String) => (s -> getPrefixPrintWriter(s))).toMap
        
    var munged=0

    override def isDone ={munged > 5000000}

    override def munge(line:String):Unit = {
      val (untrimmedStr,count) = getStringCount(line)
      val str = untrimmedStr.replaceAll("&","").replaceAll("_","")
      val currentWriter = if(str.length > 0) {
       prefixToWriter.get(str.substring(0,1).toLowerCase)
      } else None
      if(currentWriter.isDefined) {
        currentWriter.get.println(str + "\t" + count)
      } else {
        println("No prefix found for " + str)
      }
      munged = munged+1
      if(munged % 1000 == 0) {
        log("Munged " + munged + " lines.")
      }
    }
        
    private def getPrefixPrintWriter(stringPrefix:String):PrintWriter = {
      new PrintWriter("%s-%s.txt".format(outPrefix,stringPrefix))
    }
    
    override def close = {
      prefixToWriter.values.foreach(_.close)
    }
  }
  
  /*
  class SortedSliceMunger(val sliceSize:Int=100000,val outPrefix:String="output/slice") extends StreamMunger {
    val maxSlices=100
    var currentSlice=0
    var currentLine=0
    var currentWriter:PrintWriter = getPrintWriter
    var currentSort:SortedSet[(String,Int)] = SortedSet.empty
    
    // A sanity check -- in general, the file should end long before this occurs
    override def isDone = {currentSlice >= maxSlices}

    override def munge(line:String):Unit = {
      if(currentLine >= sliceSize) {
        currentSlice = currentSlice+1
        currentLine=0
        writeSlice
        currentWriter = getPrintWriter
      }
    }
    
    private def writeSlice = {
      currentWriter.close
    }
    
    private def getPrintWriter:PrintWriter = {
      new PrintWriter("%s-%03d".format(outPrefix,currentSlice))
    }
    
    override def close = {
      writeSlice
    }
  }*/
  
  
  def mungeStream(inputPath:String,sm:StreamMunger) = {
    val source = Source.fromFile(inputPath)("UTF-8")
    val lines = source.getLines()
    while(lines.hasNext && !sm.isDone) {
      sm.munge(lines.next)
    }
    sm.close
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
  
  def getStringCount(line:String,delim:String="\t"):(String,Int) = {
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