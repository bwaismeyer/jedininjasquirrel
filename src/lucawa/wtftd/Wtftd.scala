package lucawa.wtftd
//import scala.collection.mutable.PriorityQueue

object Wtftd {
	def main(args:Array[String]):Unit = {
	  test0
	}
	
	def test0 = {
	  val w = new Wtftd
	  w.createChild(w.root,"A",10.0)
	  w.createChild(w.root,"B",2.0)
	  w.createChild(w.root.firstChild,"A1",1.0)
	  
	  val w2 = w.copy
	  w.createChild(w.root.firstChild,"A2",.50)
	  println("next task: " + w.getNextTask())
	  //root.children.
	  w.root.firstChild.updatePriority(100.0)
	  println(w.printAllTasks)
	  println("First big task: " + w.root.firstChild)
	  println("next task: " + w.getNextTask())
	  // Setting a sub-tree to be complete, while skipping its elements
	  w.root.firstIncompleteChild.get.done = true
	  println("next task: " + w.getNextTask())
	  println("W:"+w.printAllTasks)
	  println("W2:"+w2.printAllTasks)
	  var isDone = false;
	  while(!isDone) {
	    val t = w.getNextTask()
	    println("Next task from getNextTask(): " + t)
	    if(t==w.root) isDone=true; else {
	      println("Setting to finished")
	      t.done = true;
	      println("All remaining tasks: " + w.printAllTasks)
	    }
	  }
	}
	
}

class Wtftd(val root:Task=new Task("root",0.0,None)) {
	/**
	 * Given a set of tasks, add those without parents to the root.
	 */
	def addTopLevelTasks(ts:Iterable[Task]) = {
	  ts.foreach(t => {
	    if(!t.hasParent) {
	    	root.addChild(t)
	    	t.setParent(root)
	    }
	  })
	}
	
	def copy:Wtftd = {
	  new Wtftd(root.copy)
	}
	
	/**
	 * If contexts argument is empty, returns all incomplete tasks.
	 * Otherwise returns all tasks with a context in the set.
	 * To specify undefined contexts, use an empty string in the set.
	 */
	def printTasksInOrder(contexts:Set[String]=Set.empty) = {
	  //println("ptio contexts:" + contexts)
	  val sb = new StringBuffer
	  val cw = this.copy
	  var next = cw.getNextTask() //root,(t:Task) => !t.done)
	  while(next != cw.root) {
	    val currentContext = next.getContext(true)
	    //println("currCon:" + currentContext)
	    val overlap = currentContext.intersect(contexts)
	    //println("overlap:" + overlap)
	    if(contexts.isEmpty || 
	        overlap.size > 0 ||
	        (currentContext.isEmpty && contexts.contains(""))) { 
	    	//println("added")
	    	sb.append(next.ancestryString + next.toString +"\n")
	    }
	    next.done = true
	    next = cw.getNextTask()
	  }
	  sb.toString
	}
	
	def printAllTasks = {
	  val sb = new StringBuffer("All tasks:\n")
	  root.children.foreach(c => {sb.append(c.printTree(0))})
	  sb.toString
	}
	
	def createChild(parent:Task,desc:String,priority:Double,context:Set[String]=Set.empty):Task = {
	  val child = new Task(desc,priority,Some(parent),context)
	  parent.addChild(child)
	  child
	}
	
	def tasksEmpty:Boolean = {
	  val retN = getNextTask()
	  if(retN == root) {
	    return true
	  } else {
	    return false
	  }
	}

	def getNextTask(parent:Task=root,condition:Function1[Task,Boolean]=(t:Task) => !t.done):Task = {
		if(parent.isLeaf) {
			parent
		} else {
		    val oc = parent.firstSatisfyingChild(condition)
			if(oc.isEmpty) 
			  parent else getNextTask(oc.get,condition)
		}
	}
}

class Task(val description:String,private var priority:Double,private var parent:Option[Task],private var context:Set[String]=Set.empty) {
  
  	implicit object Ord extends Ordering[Task] {
		def compare(x: Task, y: Task) = y.priority.compare(x.priority)
	}
  	
	var done = false;
	// The need to change key values makes priorityQueues not-so-useful.
	var children = Vector.empty[Task]
    
	def firstChild = children.head
	def firstIncompleteChild:Option[Task] = {
	  for(c <- children) {
	    if(!c.done) return Some(c)
	  }
	  return None
	}
	
    def firstSatisfyingChild(condition:Function1[Task,Boolean]):Option[Task] = {
	  for(c <- children) {
	    if(condition(c)) return Some(c)
	  }
	  return None
	}
    
    def copy:Task = {
      val newMe = new Task(description,priority,None,context)
      for(c <- children) {
        val newChild = c.copy
        newMe.addChild(newChild)
        newChild.setParent(newMe)
      }
      newMe.done = this.done
      newMe
    }
    
    // Returns a derived tree that excludes all children that do not satisfy the target contexts.
    // If contexts is empty, treat all contexts as valid.
    def filteredCopy(contexts:Set[String]):Task = {
      val newMe = new Task(description,priority,None,context)
      for(c <- children) {
        if(contexts.size == 0 || // need to look at both parents' and children's contexts
           (c.getChildContexts(true) ++ c.getContext(true)).intersect(contexts).size > 0) {
            val newChild = c.filteredCopy(contexts)
            newMe.addChild(newChild)
            newChild.setParent(newMe)
        } else {
          //println("excluded " + c)
        }
      }
      newMe.done = this.done
      newMe
    }
	
	def getPriority = priority
	
	def getContext(inherit:Boolean=true):Set[String] = {
	  context ++ {if(inherit && parent.isDefined) parent.get.context else Set.empty}
	}
	
	// All contexts for all children, optionally excluding completed subtrees.
	def getChildContexts(includeCompletedTasks:Boolean=false):Set[String] = {
	  def childContexts = for(c <- children;if(!c.done || includeCompletedTasks)) yield {
	    c.getChildContexts(includeCompletedTasks)
	  }
	  context ++ childContexts.flatten
	}
	
	def hasParent = !parent.isEmpty
	
	def isLeaf     = {children.size == 0}
	
	def addChild(t:Task) = {
	  children = (children :+ t).sortBy(-_.priority)
	}
	
	def updatePriority(p:Double) = {
	  this.priority = p;
	  if(!parent.isEmpty) {
	    // Expensive, yes.
	    parent.get.children = parent.get.children.sortBy(-_.priority)
	  }
	}
	
	def setParent(p:Task) = {
	  if(parent.isEmpty) {
	    this.parent = Some(p)
	    assert(p.children.contains(this))
	  } else {
	    throw new RuntimeException("no current support for changing parents, just setting empty")
	  }
	}
	
	override def toString:String = {
	  return (if(done) "+" else "-") + " " + description + " " + priority
	}
	
    def ancestryString:String = {
      // the second check excludes the root
	  if(this.hasParent && this.parent.get.hasParent) {
	    this.parent.get.ancestryString + this.parent.get.description + " / "
	  } else ""
	}
	
	def printTree(depth:Int=0,depthStr:String="\t",stringFunc:Function1[Task,String]=(t:Task)=>t.toString):String = {
	  val sb = new StringBuffer
	  for(i <- 0 until depth) {
	    sb.append(depthStr)
	  }
	  sb.append(stringFunc(this))
	  sb.append("\n")
	  this.children.foreach(t => sb.append(t.printTree(depth+1,depthStr,stringFunc)))
	  sb.toString
	}
	
}