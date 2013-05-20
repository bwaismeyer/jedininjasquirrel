package lucawa.wtftd

import javax.swing.{JTextArea, JFrame, JPanel, JLabel, JButton,JTree}
import javax.swing.tree.TreePath
import scala.swing._
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
  
  val readingButton = new Button("reading");
  val workButton = new Button("work");

  val allContexts = w.root.getChildContexts(true)
  val buttonPanel = new GridPanel(allContexts.size,1) {
    //contents ++= readingButton :: workButton :: Nil
    val contextButtons = allContexts.toIndexedSeq.sorted.map(ts => new Button(ts))
    contextButtons.foreach(b => {
      println("listening to: " + b)
      WtdUi.listenTo(b)
      })
    contents ++= contextButtons
  }
  
  //listenTo(readingButton)
  //listenTo(workButton)
  
  reactions += {
    case ButtonClicked(b) => {
      println("clicked " + b.text)
      if(allContexts.contains(b.text)) {
        val filteredW = new Wtftd(w.root.filteredCopy(Set(b.text)))
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
    contents = new GridPanel(1,3) {
      contents ++= treePane :: buttonPanel :: filteredTreePane :: Nil 
    }
  }
  
  
  

 
  
}