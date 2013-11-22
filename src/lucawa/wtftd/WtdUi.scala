package lucawa.wtftd

import javax.swing.{JTextArea, JFrame, JPanel, JLabel, JButton,JTree,JPopupMenu,JMenuItem}
import javax.swing.SwingUtilities
import javax.swing.tree.TreePath
import scala.swing._
import scala.swing.GridBagPanel._
import scala.swing.event._
import java.awt.event.MouseListener
import java.awt.event.MouseAdapter
import java.awt.event.MouseEvent

/**
 * UI for interacting with todo lists.
 * There are some missing triggers/events in the UI. Changes don't automatically save.
 * 
 */
object WtdUi extends SimpleSwingApplication {

  var currentTaskFilter:Function1[Task,Boolean] = (t) => true
  var currentContexts = Set.empty[String]
  var currentTask: Option[Task] = None
  
  val twi = new TextWtdIo("C:/Users/chris/Documents/professional/todoList.txt")
  val savePath = "C:/Users/chris/Documents/professional/todoList.txt"
  val w = twi.readWtftd
  var tmw = new TreeModelWrapper(w)
  
  val completedTasksBox = new CheckBox("Show completed tasks")
  val saveListButton    = new Button("Save todo-list")
  val isDoneBox = new CheckBox("Completed")
  val updateBox = new Button("Update desc/priority")
  
  val priorityField = new TextArea(1,3)
  val descField = new TextArea(4,40)

  // determines whether to show completed tasks
  def includeCompletedTasks = completedTasksBox.selected
  // the label of the button that displays tasks from all contexts
  val allTasksString = "All tasks"
    
  val flatTasks = new TextArea(10,20) {
	lineWrap = true  
    wordWrap = true
  }
  val flatTaskPane = new ScrollPane {contents = flatTasks}
  flatTaskPane.preferredSize = new Dimension(400,100)

  
  /**
   * Sets up a new tree to view, and sets up listeners so that clicking on tree nodes has the intended effect.
   * There's a chance that the listeners aren't properly GCed when the tree changes, so there could be a mild memory leak.
   */
  def listenToTree(mjt:JTree) = {
    val ml = new MouseAdapter() {
      override def mousePressed(e:MouseEvent) = {
      val selRow:Int = mjt.getRowForLocation(e.getX(), e.getY());
      val selPath:TreePath = mjt.getPathForLocation(e.getX(), e.getY());
      if(selRow != -1) {
    	 if(e.getClickCount() == 1) {
    	   val clickedTask  = selPath.getLastPathComponent().asInstanceOf[Task]
    	   if(SwingUtilities.isRightMouseButton(e)) {
    	     val myMenu = new TaskPopupMenu(clickedTask)
    	     myMenu.show(e.getComponent(), e.getX(), e.getY())
    	     //val newT = new Task("new task",0.0,Some(clickedTask),context=Set.empty)
    	     //clickedTask.addChild(newT)
    	   } else {
    	     currentTask = Some(w.getNextTask(clickedTask,currentTaskFilter))
    	     displayTask(currentTask)    	     
    	   }

         }
         else if(e.getClickCount() == 2) {
           //myDoubleClick(selRow, selPath);
         }
      }
     }
   }
   mjt.addMouseListener(ml);
  }
  
  def displayTask(tOpt:Option[Task]) = {
    if(tOpt.isDefined) {
      flatTasks.text = tOpt.get.ancestryString + "\n\n" + tOpt.get.toString
      taskControlPanel.setTask(tOpt)
    } else {
      flatTasks.text = "No task selected"
    }
  }
  
  val filteredTreePane = new ScrollPane {
    this.preferredSize = new Dimension(500,400)
    println("foo")
    contents = newTreeComponent(w) //,!WtdUi.completedTasksBox.selected,WtdUi.currentContexts)
  }
  
  def updateFilter() = {
    currentTaskFilter =
      (t:Task) => 
        {(!t.done || this.completedTasksBox.selected) && {(currentContexts.size==0 || (t.getChildContexts(true) ++ t.getContext(true)).intersect(currentContexts).size > 0)}}
  }
  
  def newTreeComponent(wt:Wtftd) = {
    tmw = new TreeModelWrapper(wt)
    println("updatedTreeComp: " + wt)
    updateFilter()
    tmw.setFilter(currentTaskFilter)
    val nt = new JTree(tmw)
    nt.setRootVisible(false)
    listenToTree(nt)
    new Component {
    	override lazy val peer = nt
    }
  }
  
  val allButton = new Button(allTasksString)
  val allContexts = w.root.getChildContexts(true)

  val contextButtonPanel = new GridPanel(allContexts.size,1) {
    val contextButtons = allContexts.toIndexedSeq.sorted.map(ts => new Button(ts))
    contextButtons.foreach(b => {
      println("listening to: " + b)
      WtdUi.listenTo(b)
      })
    contents += allButton
    WtdUi.listenTo(allButton)
    contents ++= contextButtons
    //this.preferredSize = new Dimension(150,200)
  }
  
  val taskControlPanel = new GridPanel(6,2) {
    descField.maximumSize = descField.size
    descField.lineWrap = true
    descField.charWrap = true
    
    val ancestryField = new TextArea(1,50)
    ancestryField.editable = false
    val contextField = new TextArea(1,20)
    contextField.editable = false // Need to verify safety of changing context before allowing editing (and updating)
    
    def setTask(to:Option[Task]) = {
      if(to.isDefined) {
        val t = to.get
    	isDoneBox.selected = t.done
    	descField.text = t.description
    	descField.columns = 20
    	descField.wordWrap = true
        priorityField.text = t.getPriority.toString
        ancestryField.text =  t.ancestryString
        contextField.text = t.getContext(true).mkString(",")
      } else {
        isDoneBox.selected = false
        priorityField.text = "NA"
        contextField.text  = "NA"
        descField.text = "NA"
      }
    }
    contents += new Label("Task Ancestry")
    contents += ancestryField
    contents += new Label("Task desc")
    contents += descField
    contents += new Label("is done")
    contents += isDoneBox
    contents += new Label("priority")
    contents += priorityField
    contents += new Label("context")
    contents += contextField
    contents += new Label("update")
    contents += updateBox
    
    WtdUi.listenTo(isDoneBox)
    WtdUi.listenTo(updateBox)
  }
  
  def refreshTree() = {
    // Presently this rebuilds the tree, so the focus etc. are lost.
    filteredTreePane.contents = newTreeComponent(w)
    // One alternative is to use valueForPathChanged, which is not currently implemented
    //tmw.valueForPathChanged
  }
  
  reactions += {
    case ButtonClicked(b) if(b == allButton) => {
      currentContexts = Set.empty
      refreshTree()
    }
    case ButtonClicked(b) if(allContexts.contains(b.text)) => {
      currentContexts = Set(b.text)
      refreshTree()
    }
    case ButtonClicked(b) if(b == this.isDoneBox) => {
      if(currentTask.isDefined) {
        currentTask.get.done = isDoneBox.selected
      }
      val oldCurrentTask = currentTask

      currentTask = if(currentTask.isDefined) {
        if(currentTask.get.getParent.isDefined) {
          Some(w.getNextTask(currentTask.get.getParent.get,currentTaskFilter))
        } else {None}
      } else {
        None
      }
      println("new currentTask: " + currentTask)
      displayTask(currentTask)
      tmw.fireTreeStructureChanged(oldCurrentTask.get.getParent.get)
    }
    case ButtonClicked(b) if(b == this.updateBox) => {
      if(currentTask.isDefined) {
        // TODO: validate fields
        val pr = this.priorityField.text.toDouble
        val ct = currentTask.get
        ct.description = this.descField.text
        ct.updatePriority(pr)
        println("updated desc to " + ct.description)
        tmw.fireTreeStructureChanged(ct.getParent.get)
      }
    }
    
    case ButtonClicked(b) if(b== this.saveListButton) => {
      val two = new TextWtdIo(savePath)
      two.syncWtftd(this.w)
      println("Saved to " + savePath)
    }
    case ButtonClicked(b) =>  {
      refreshTree()
    }

    
  }

  def top = new MainFrame {
    
    contents = new GridBagPanel {
      def constraints(x: Int, y: Int, 
		    gridwidth: Int = 1, gridheight: Int = 1,
		    weightx: Double = 0.0, weighty: Double = 0.0,
		    fill: GridBagPanel.Fill.Value = GridBagPanel.Fill.None): Constraints = {
      val c = new Constraints
      c.gridx = x
      c.gridy = y
      c.gridwidth = gridwidth
      c.gridheight = gridheight
      c.weightx = weightx
      c.weighty = weighty
      c.fill = fill
      c
    }
      //contents ++= treePane :: buttonPanel :: filteredTreePane :: Nil 
      //add(treePane,constraints(0,0,2,1))
      add(new Label("Contexts"),constraints(0,0))
      add(contextButtonPanel,constraints(0,1,1,1,0.0,0.0,GridBagPanel.Fill.Both))
      add(completedTasksBox,constraints(0,2,1,1,0.0,0.0,GridBagPanel.Fill.Both))
      WtdUi.listenTo(completedTasksBox)
      add(saveListButton,constraints(0,3,1,1,0.0,0.0,GridBagPanel.Fill.Both))
      WtdUi.listenTo(saveListButton)
      add(new Label("Task tree"),(1,0))
      add(filteredTreePane,constraints(1,1,2,1,1.0,1.0,GridBagPanel.Fill.Both))
      //add(new Label("Next task"),(3,0))
      //add(flatTaskPane,constraints(3,1,2,1,1.0,1.0,GridBagPanel.Fill.Both))
      //add(taskControlPanel,constraints(3,2,2,1,1.0,1.0,GridBagPanel.Fill.Both))
      add(taskControlPanel,constraints(3,1,2,1,0.0,1.0,GridBagPanel.Fill.Vertical))
    }
  }
}

class TaskPopupMenu(t:Task) extends Component {
  override lazy val peer : JPopupMenu = new JPopupMenu

  val showTaskItem = new MenuItem("show task")
  val addTaskItem = new MenuItem("add task")
  val deleteTaskItem = new MenuItem("delete task")
  
  add(showTaskItem)
  add(addTaskItem)
  add(deleteTaskItem)

  this.listenTo(showTaskItem)
  this.listenTo(addTaskItem)
  this.listenTo(deleteTaskItem)
  reactions += {
    case x:ButtonClicked if(x.source == showTaskItem) => {
      WtdUi.currentTask = Some(t)
      WtdUi.displayTask(Some(t))
      //val cjt = WtdUi.currentJTree.get
      //WtdUi.tmw
    }
    case x:ButtonClicked if(x.source == addTaskItem) => {
      // TODO: Change focus to newly created task.
      WtdUi.w.createChild(t,"new task", 10, Set.empty)
      WtdUi.tmw.fireTreeStructureChanged(t)
    }
    case x:ButtonClicked if(x.source == deleteTaskItem) => {
      println("[TODO] confirm intent to delete a task")
      val parent = t.getParent.get
      WtdUi.w.deleteChild(parent, t)
      WtdUi.tmw.fireTreeStructureChanged(parent)
    }//(x:Any) => println(x)}
  }
  
  def add(item:MenuItem) : Unit = { peer.add(item.peer) }
  def show(parent:java.awt.Component,x:Int,y:Int) : Unit = { peer.show(parent,x,y) }
  def setVisible(visible:Boolean) : Unit = { peer.setVisible(visible) }
  /* Create any other peer methods here */
}
