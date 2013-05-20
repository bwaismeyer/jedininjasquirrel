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
      case t:Task => return t.children(idx)
      case _ => tOnly
    }
  }
  
  def getChildCount(parent:Object):Int = {
    parent match {
      case t:Task => return t.children.size
      case _ => tOnly
    }
      
  }
    
  def getIndexOfChild(parent:Any, child:Any):Int = {
        parent match {
      case pt:Task => {
        child match {
          case ct:Task => {
            for(ci <- 0 until getChildCount(pt)) {
               if(pt.children(ci)==ct) return ci
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