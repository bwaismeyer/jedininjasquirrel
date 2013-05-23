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
 * Major todo right now is to support changing tasks from the UI, rather than merely displaying them.
 * At the moment, many operations rely on filteredCopy, which creates a defensive copy of everything, and changes to the copy don't persist.
 * 
 * One alternative approach would be to use filters in TreeModelWrapper, which always keeps the same tree around.
 */
object WtdUi extends SimpleSwingApplication {

  //val foo = new BoxPanel(Orientation.Vertical)

  val twi = new TextWtdIo("C:/Users/chris/Documents/professional/todoList.txt")
  val w = twi.readWtftd
  
  val completedTasksBox = new CheckBox("Show completed tasks")
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
    	   val nextTask = w.getNextTask(clickedTask)
    	   displayTask(nextTask)
         }
         else if(e.getClickCount() == 2) {
           //myDoubleClick(selRow, selPath);
         }
      }
     }
   }
   mjt.addMouseListener(ml);
  }
  
  private def displayTask(t:Task) = {
    flatTasks.text = t.ancestryString + "\n\n" + t.toString
    taskControlPanel.setTask(t)
  }
 
  val filteredTreePane = new ScrollPane {
    this.preferredSize = new Dimension(500,400)
    contents = updatedTreeComp(w)
  } 
  
  def updatedTreeComp(wt:Wtftd) = {
    val nt = new JTree(new TreeModelWrapper(wt))
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
    val isDoneBox = new CheckBox("Completed") 
    val priorityField = new TextArea(1,3)
    val contextField = new TextArea(1,20)
    var taskO:Option[Task] = None
    def setTask(t:Task) = {
      this.taskO = Some(t)
      isDoneBox.selected = t.done
      priorityField.text = t.getPriority.toString
      contextField.text = t.getContext(true).mkString(",")
    }
    contents += isDoneBox
    contents += priorityField
    contents += contextField
    
    this.listenTo(isDoneBox)
    
    reactions += {
      case ButtonClicked(b) => {
        if(b == isDoneBox && taskO.isDefined) {
          taskO.get.done = isDoneBox.selected
        }
      }
    }
  }
  
  reactions += {
    case ButtonClicked(b) => {
      println("clicked " + b.text)
      if(b == allButton) {
        val filteredW = new Wtftd(w.root.filteredCopy(Set.empty,includeCompletedTasks))
        displayTask(filteredW.getNextTask())
        filteredTreePane.contents = updatedTreeComp(filteredW)
      } else if(allContexts.contains(b.text)) {
        val filteredW = new Wtftd(w.root.filteredCopy(Set(b.text),includeCompletedTasks))
        displayTask(filteredW.getNextTask())
        filteredTreePane.contents = updatedTreeComp(filteredW)
      }
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
      add(new Label("Task tree"),(1,0))
      add(filteredTreePane,constraints(1,1,2,1,1.0,1.0,GridBagPanel.Fill.Both))
      add(new Label("Next task"),(3,0))
      add(flatTaskPane,constraints(3,1,2,1,1.0,1.0,GridBagPanel.Fill.Both))
      add(taskControlPanel,constraints(3,2,2,1,1.0,1.0,GridBagPanel.Fill.Both))
    }
  }
}