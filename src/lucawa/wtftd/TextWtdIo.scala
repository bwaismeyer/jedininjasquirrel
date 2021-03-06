package lucawa.wtftd
import scala.io._
import java.io._
import scala.collection.mutable.Stack

object TextWtdIo {
  
  def main(args:Array[String]):Unit = {
    val twi = new TextWtdIo("C:/Users/chris/Documents/professional/todoListFromUi.txt")
    val w = twi.readWtftd
    val two = new TextWtdIo("C:/Users/chris/Documents/professional/todoListSorted.txt")
    
    two.syncWtftd(w)
    //println("All tasks: " + w.printAllTasks)
    //println("Tasks in order: ")
    //println(w.printTasksInOrder)
    writeString("C:/Users/chris/Documents/professional/todoItemsInOrder.txt",w.printTasksInOrder())
    writeString("C:/Users/chris/Documents/professional/todoItemsInOrderReading.txt",w.printTasksInOrder(Set("reading","")))

    
  }
  
  def test = {
    val twi = new TextWtdIo("C:/Users/chris/Documents/professional/todoList.txt")
    val two = new TextWtdIo("data/newTmpTodo.txt")
    val w = twi.readWtftd
    println("Tasks in order: ")
    println(w.printTasksInOrder())
    w.createChild(w.root,"pretend task",1.0)
    println(w.printAllTasks)
    two.syncWtftd(w)
    val w2 = two.readWtftd
  }
  def writeString(fPath:String,s:String) = {
     val f = new File(fPath)
      val out =  new PrintWriter(f,"UTF-8")
      try {out.print(s);true} 
	  catch {case e:Throwable => {e.printStackTrace;false}}
      finally{out.close}
   }
}

class TextWtdIo(val path:String) extends WtdIo {
  
  def readWtftd:Wtftd = {
    val linePattern = new scala.util.matching.Regex("""(\s*)([\+\-])\s+(\-?\d+\.?\d*)(|[^\s]*)\s+(\S.*)""", "indent","complete","priority","context","taskString")
    val w = new Wtftd
    val source = Source.fromFile(new File(path))(Codec.UTF8)
    val lines = source.getLines
    val parentStack = new Stack[(Task,Int)]
    parentStack.push((w.root,-1))
    while(lines.hasNext) {
      val line = lines.next
      if(linePattern.findFirstIn(line).isEmpty) {
        throw new RuntimeException("Line did not match pattern. Line was %s".format(line))
      }
      val linePattern(indent,completeStr,priorityStr,contextStr,taskString) = line
      val isComplete = completeStr match {
        case "+" => true
        case "-" => false
        case _   => throw new RuntimeException("unknown completeness state")
      }
      val context:Set[String] = if(contextStr.length > 1) contextStr.substring(1).split(",").toSet else Set.empty
      val priority = priorityStr.toDouble
      val depth = indent.size
      while(depth <= parentStack.head._2) {
    	  parentStack.pop
      }
      val thisParent = parentStack.head._1
      val newTask = w.createChild(thisParent,taskString,priority,context)
      if(isComplete) {
        newTask.done = true
      }
      parentStack.push((newTask,depth))
      //println(indent.length + " complete: " + isComplete + " priority " + priority + "\tcontext: " + context + "\tstring: " + taskString)
    }
    w
  }
  
  // Better not to depend on t
   def taskToLine(t:Task):String = {
      val contextStr = if(t.getContext(false).isEmpty) "" else {"|" + t.getContext(false).mkString(",")}
      val priorityStr = if(t.getPriority == t.getPriority.toInt) t.getPriority.toInt.toString else t.getPriority.toString
	  return (if(t.done) "+" else "-") + " " + priorityStr + contextStr + " " + t.description
	}


   def syncWtftd(w:Wtftd):Boolean = {
	  val sb = new StringBuffer()
	  w.root.children.foreach(c => {sb.append(c.printTree(0,"\t",taskToLine))}) 
	  TextWtdIo.writeString(path,sb.toString)
   }
   

  
}