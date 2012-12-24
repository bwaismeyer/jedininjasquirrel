package lucawa.toys

import swing._
import java.awt.geom._
import java.awt.{Stroke,BasicStroke}

object FractalTree extends SimpleSwingApplication {
  def top = new MainFrame {
    title = "Hello, World!"
    contents = new HelperPanel
  }
}

class HelperPanel extends Panel {

  val pi = 3.14159

  preferredSize = new Dimension(400,400)
  minimumSize = new Dimension(200,200)

  override def paintComponent(g: Graphics2D) = {
    g.setColor(new Color(100,100,255))
    g.fillRect(0, 0, size.width, size.height)
    g.setColor(new Color(100,255,0))

    val angle = pi/2
    val branchLength = 350.0
    paintBranch(g,200.0,20.0,angle,branchLength,6)
  }



  def paintBranch(g: Graphics2D,startX:Double,startY:Double,angle:Double,branchLength:Double,depthLeft:Int):Unit = {
    g.setColor(new Color(100,255,0))
    g.setStroke(new BasicStroke(.75f*depthLeft))
    val endX = startX + math.cos(angle)*branchLength;
    val endY = startY + math.sin(angle)*branchLength;
    g.draw(new Line2D.Double(startX,startY,endX,endY))
    if(depthLeft > 0) {
      val midPoint = (.6*startX+.4*endX,.6*startY+.4*endY)
      paintBranch(g,midPoint._1,midPoint._2,angle+pi/12,branchLength/1.52,depthLeft-1)
      paintBranch(g,midPoint._1,midPoint._2,angle-pi/12,branchLength/1.52,depthLeft-1)

      val upperPoint = (.92*startX+.08*endX,.92*startY+.08*endY)
      paintBranch(g,upperPoint._1,upperPoint._2,angle+pi/12,branchLength/2.5,depthLeft-1)
      paintBranch(g,upperPoint._1,upperPoint._2,angle-pi/12,branchLength/2.5,depthLeft-1)
    }

  }

}