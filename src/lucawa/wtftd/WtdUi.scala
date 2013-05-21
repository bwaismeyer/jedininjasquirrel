package lucawa.wtftd

import javax.swing.{JTextArea, JFrame, JPanel, JLabel, JButton,JTree}
import javax.swing.tree.TreePath
import scala.swing._
import scala.swing.GridBagPanel._
import scala.swing.event._
import java.awt.event.MouseListener
import java.awt.event.MouseAdapter
import java.awt.event.MouseEvent

object WtdUi extends SimpleSwingApplication {

  //val foo = new BoxPanel(Orientation.Vertical)

  val twi = new TextWtdIo("C:/Users/chris/Documents/professional/todoList.txt")
  val w = twi.readWtftd
  
  val treeModel = new TreeModelWrapper(w)
  val myJTree = new JTree(treeModel)
  
  val ml = new MouseAdapter() {
   override def mousePressed(e:MouseEvent) = {
     val selRow:Int = myJTree.getRowForLocation(e.getX(), e.getY());
     val selPath:TreePath = myJTree.getPathForLocation(e.getX(), e.getY());
     if(selRow != -1) {
    	 if(e.getClickCount() == 1) {
    	   //mySingleClick(selRow, selPath);
    	   println("Single click on row %s, path=%s".format(selRow.toString,selPath.toString))
    	   println("child contexts: " + selPath.getLastPathComponent().asInstanceOf[Task].getChildContexts(false))
         }
         else if(e.getClickCount() == 2) {
           //myDoubleClick(selRow, selPath);
         }
     }
   }
 }
 myJTree.addMouseListener(ml);
  
  val treePane = new ScrollPane {
    contents = new Component {
	  override lazy val peer = myJTree
    }
  }
  
  var filteredTreePane = new ScrollPane {
    contents = new Component {
	  override lazy val peer = new JTree(treeModel)
    }
  } 
  val allTasksString = "All tasks"
  val allButton = new Button(allTasksString)
  val allContexts = w.root.getChildContexts(true)
  val flatTasks = new TextArea(10,20) {
	lineWrap = true  
    wordWrap = true
  }
  val flatTaskPane = new ScrollPane {contents = flatTasks}
  flatTaskPane.preferredSize = new Dimension(400,100)
  val buttonPanel = new GridPanel(allContexts.size,1) {
    //contents ++= readingButton :: workButton :: Nil
    val contextButtons = allContexts.toIndexedSeq.sorted.map(ts => new Button(ts))
    contextButtons.foreach(b => {
      println("listening to: " + b)
      WtdUi.listenTo(b)
      })
    contents += allButton
    WtdUi.listenTo(allButton)
    contents ++= contextButtons
    this.preferredSize = new Dimension(150,200)
  }
  
  //listenTo(readingButton)
  //listenTo(workButton)
  
  reactions += {
    case ButtonClicked(b) => {
      println("clicked " + b.text)
      //if(b.text == allTasksString) {
      if(b == allButton) {
        val filteredW = new Wtftd(w.root)
        flatTasks.text = filteredW.getNextTask().toString
        filteredTreePane.contents = new Component {override lazy val peer = new JTree(new TreeModelWrapper(filteredW))}
      } else if(allContexts.contains(b.text)) {
        val filteredW = new Wtftd(w.root.filteredCopy(Set(b.text)))
        flatTasks.text = filteredW.getNextTask().toString
        filteredTreePane.contents = new Component {override lazy val peer = new JTree(new TreeModelWrapper(filteredW))}
      }
    }
  }
      /*
      b.text match {
        case "reading" => {
          println("reading button!")
          val filteredW = new Wtftd(w.root.filteredCopy(Set("reading")))
          filteredTreePane.contents = new Component {override lazy val peer = new JTree(new TreeModelWrapper(filteredW))}
        }
        case "work" => {
          println("work button!")
          val filteredW = new Wtftd(w.root.filteredCopy(Set("work")))
          filteredTreePane.contents = new Component {override lazy val peer = new JTree(new TreeModelWrapper(filteredW))}
        }
        case _ => println("some other button!")
        }
        * 
        */

    //
  //val treeModel = new TreeModelWrapper(filteredW)

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
      add(treePane,constraints(0,0,2,1))
      add(buttonPanel,constraints(2,0))
      add(filteredTreePane,constraints(3,0,2,1))
      add(flatTaskPane,constraints(5,0,2,1))
    }
  }
  
  
  

 
  
}