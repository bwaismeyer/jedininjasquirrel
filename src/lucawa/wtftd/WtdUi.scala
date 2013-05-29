package lucawa.wtftd

import javax.swing.{JTextArea, JFrame, JPanel, JLabel, JButton,JTree}
import javax.swing.tree.TreePath
import scala.swing._
import scala.swing.GridBagPanel._
import scala.swing.event._
import java.awt.event.MouseListener
import java.awt.event.MouseAdapter
import java.awt.event.MouseEvent

/**
 * UI for interacting with todo lists.
 * There are some missing triggers/events in the UI, and no provision for saving changes.
 */
object WtdUi extends SimpleSwingApplication {

  var currentTaskFilter:Function1[Task,Boolean] = (t) => true
  var currentContexts = Set.empty[String]
  var currentTask: Option[Task] = None

  val twi = new TextWtdIo("C:/Users/chris/Documents/professional/todoList.txt")
  val w = twi.readWtftd
  
  val completedTasksBox = new CheckBox("Show completed tasks")
  val isDoneBox = new CheckBox("Completed") 
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
    	   println("Single click on row %s, path=%s".format(selRow.toString,selPath.toString))
    	   println("child contexts: " + clickedTask.getChildContexts(false))
    	   currentTask = Some(w.getNextTask(clickedTask))
    	   displayTask(currentTask)
         }
         else if(e.getClickCount() == 2) {
           //myDoubleClick(selRow, selPath);
         }
      }
     }
   }
   mjt.addMouseListener(ml);
  }
  
  private def displayTask(tOpt:Option[Task]) = {
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
    contents = updatedTreeComp(w) //,!WtdUi.completedTasksBox.selected,WtdUi.currentContexts)
  }
  
  def updateFilter() = {
    currentTaskFilter =
      (t:Task) => {(!t.done || this.completedTasksBox.selected) && {
       (currentContexts.size==0 || (t.getChildContexts(true) ++ t.getContext(true)).intersect(currentContexts).size > 0) 
      }}
  }
  
  def updatedTreeComp(wt:Wtftd) = {// ,excludeComplete:Boolean,filtContexts:Set[String]) = {
    val tmw = new TreeModelWrapper(wt)
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
  
  val taskControlPanel = new GridPanel(3,1) {
    val priorityField = new TextArea(1,3)
    val contextField = new TextArea(1,20)
    var taskO:Option[Task] = None
    
    def setTask(to:Option[Task]) = {
      this.taskO = to
      if(to.isDefined) {
        val t = to.get
    	isDoneBox.selected = t.done
        priorityField.text = t.getPriority.toString
        contextField.text = t.getContext(true).mkString(",")
      } else {
        isDoneBox.selected = false
        priorityField.text = "NA"
        contextField.text  = "NA"
      }
    }
    contents += isDoneBox
    contents += priorityField
    contents += contextField
    
    this.listenTo(isDoneBox)
    WtdUi.listenTo(isDoneBox)
    reactions += {
      case ButtonClicked(b) => {
        if(b == isDoneBox && taskO.isDefined) {
          taskO.get.done = isDoneBox.selected
          println("setting " + taskO.get + " done to " + isDoneBox.selected)
        }
      }
    }
  }
  
  def refreshTree() = {
      filteredTreePane.contents = updatedTreeComp(w) //,!includeCompletedTasks,currentContexts)
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
      currentTask = if(currentTask.isDefined) {
        if(currentTask.get.getParent.isDefined) {
          Some(w.getNextTask(currentTask.get.getParent.get,currentTaskFilter))
        } else {None}
      } else {
        None
      }
      println("new currentTask: " + currentTask)
      //currentTask = Some(w.getNextTask(currentTask.get.parent).getOrElse(w.root),currentTaskFilter))
      displayTask(currentTask)
      refreshTree()
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
      add(new Label("Task tree"),(1,0))
      add(filteredTreePane,constraints(1,1,2,1,1.0,1.0,GridBagPanel.Fill.Both))
      add(new Label("Next task"),(3,0))
      add(flatTaskPane,constraints(3,1,2,1,1.0,1.0,GridBagPanel.Fill.Both))
      add(taskControlPanel,constraints(3,2,2,1,1.0,1.0,GridBagPanel.Fill.Both))
    }
  }
}