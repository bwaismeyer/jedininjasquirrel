package lucawa.toys

import swing._
import java.awt.geom._
import java.awt.{Stroke,BasicStroke}

object FractalTree extends SimpleSwingApplication {
  def top = new MainFrame {
    title = "Hello, World!"
    contents = new HelperPanel(6,.150)
  }
}

class HelperPanel(val depth:Int,val thickCoeff:Double) extends Panel {
  val width = 600
  val height= 600
  val pi = 3.14159
  val upperCoeff = .95
  val baseBranchLength = 350.0

  preferredSize = new Dimension(width,height)
  //minimumSize = new Dimension(200,200)

  override def paintComponent(g: Graphics2D) = {
    g.setColor(new Color(100,100,255))
    g.fillRect(0, 0, size.width, size.height)
    g.setColor(new Color(100,255,0))

    val angle = pi/2
    paintBranch(g,width/2.0,20.0,angle,baseBranchLength,depth)
  }



  def paintBranch(g: Graphics2D,startX:Double,startY:Double,angle:Double,branchLength:Double,depthLeft:Int):Unit = {
    g.setColor(new Color(100,255-20*depthLeft,0))
    g.setStroke(new BasicStroke(thickCoeff.floatValue*depthLeft))
    val endX = startX + math.cos(angle)*branchLength;
    val endY = startY + math.sin(angle)*branchLength;
    g.draw(new Line2D.Double(startX,startY,endX,endY))
    if(depthLeft > 0) {
      val midPoint = (.6*startX+.4*endX,.6*startY+.4*endY)
      paintBranch(g,midPoint._1,midPoint._2,angle+pi/12,branchLength/1.52,depthLeft-1)
      paintBranch(g,midPoint._1,midPoint._2,angle-pi/12,branchLength/1.52,depthLeft-1)

      val upperPoint = (upperCoeff*startX+(1.0-upperCoeff)*endX,upperCoeff*startY+(1.0-upperCoeff)*endY)
      paintBranch(g,upperPoint._1,upperPoint._2,angle+pi/12,branchLength/2.5,depthLeft-1)
      paintBranch(g,upperPoint._1,upperPoint._2,angle-pi/12,branchLength/2.5,depthLeft-1)
    }

  }

}