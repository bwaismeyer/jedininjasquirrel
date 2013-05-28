package lucawa.wtftd

import javax.swing.tree.TreeModel
import javax.swing.event.TreeModelListener
import javax.swing.event.TreeModelEvent
import javax.swing.tree.TreePath

/*
 * Permits visualization using a JTree
 */
class TreeModelWrapper(wtd:Wtftd) extends TreeModel {

  private def tOnly = {throw new RuntimeException("only tasks accepted")}
  
  private var taskFilter:Function1[Task,Boolean] = (t) => true
  private def filteredChildren(t:Task):IndexedSeq[Task] = {
    t.children.filter(taskFilter)
  }
  
  def setFilter(excludeComplete:Boolean,filtContexts:Set[String]) = {
    taskFilter =
      (t:Task) => {(!t.done || !excludeComplete) && {
       (filtContexts.size==0 || (t.getChildContexts(true) ++ t.getContext(true)).intersect(filtContexts).size > 0) 
      }}
      fireTreeStructureChanged(wtd.root)
  }
  
  var treeModelListeners = Set.empty[TreeModelListener]
  
  
  
  def addTreeModelListener(tml:TreeModelListener) = {
    treeModelListeners = treeModelListeners + tml
  }
  
  private def fireTreeStructureChanged(oldRoot:Task):Unit = {
        val e:TreeModelEvent = new TreeModelEvent(this,Array(oldRoot.asInstanceOf[Object]))
        for(tml <- treeModelListeners) {
            tml.treeStructureChanged(e)
        }
    }
  
  def getChild(parent:Object,idx:Int):Task = {
    parent match {
      case t:Task => return filteredChildren(t)(idx)
      case _ => tOnly
    }
  }
  
  def getChildCount(parent:Object):Int = {
    parent match {
      case t:Task => return filteredChildren(t).size
      case _ => tOnly
    }
      
  }
    
  def getIndexOfChild(parent:Any, child:Any):Int = {
        parent match {
      case pt:Task => {
        child match {
          case ct:Task => {
            for(ci <- 0 until getChildCount(pt)) {
               if(filteredChildren(pt)(ci)==ct) return ci
            }
            return -1
          }
          case _ => tOnly
        }
      }
      case _ => tOnly
    }
  }  
  
  def getRoot:Task = {
    return wtd.root
  }
  
  def isLeaf(obj:Any) = {
    obj match {
      case t:Task => t.isLeaf
      case _ => throw new RuntimeException("only tasks")
    }
  }
  
  def removeTreeModelListener(l:TreeModelListener) = {
    treeModelListeners = treeModelListeners - l
  }
  
  def valueForPathChanged(path:TreePath, newValue:Object) = {
    throw new RuntimeException("not implemented")
  }
}