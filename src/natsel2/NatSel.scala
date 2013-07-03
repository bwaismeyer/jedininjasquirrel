package natsel2

import javax.swing._
import scala.swing._
import scala.swing.GridBagPanel._
import scala.swing.event._
import scala.collection.mutable.HashMap
import java.awt.image.BufferedImage
import java.awt.Graphics2D
import java.awt.Color
import java.awt.geom.Rectangle2D
import java.awt.Rectangle

class World(val width:Int=800,val height:Int=800) {
  var food:Array[Array[Double]] = Array.fill(width,height)(0.0) //Array.tabulate(800)((i) => {(Array.tabulate(800)(j => j/800.0)).toIndexedSeq})
  for(i <- 0 until food.size;j <- 0 until 100) {food(i)(j) = 1}
  var entities = List.empty[Entity]
  val positions = new HashMap[(Int,Int),Entity]
  
  def addEntity(ent:Entity) = {
    if(positions.isDefinedAt((ent.x,ent.y))) throw new RuntimeException("tried to add entity at an occupied position")
    if(ent.x < 0 || ent.y >= width || ent.y < 0 || ent.y >= height) throw new RuntimeException("tried to add entity outside the world bounds")
    //val ent = Entity.buildDefault(x,y,en)
    entities = ent :: entities
    positions.put((ent.x,ent.y),ent)
  }
  
  def tick = {
    println("tick")
    for(e <- entities) {
      e.tick(this)
    }
    moveEntities
    consumeFood
    reproduceEntities
    food = diffuse(food)
    reap
  }
  
  def wrapCoords(c:Int,mc:Int) = {
    if(c<0) {mc-1} else {
      c % mc
    }
  }
  
  def wrapX(x:Int) = {
    wrapCoords(x,this.width)
  }
  
  def wrapY(y:Int) = {
    wrapCoords(y,this.height)
  }
  
  def moveEntities = {
    entities.foreach(e => {
      val mv = e.chosenMove
      val newPos = (wrapCoords(e.x+mv._1,width),wrapCoords(e.y+mv._2,height))
      if(!isOccupied(newPos._1,newPos._2)) {
        positions.remove((e.x,e.y))
        positions.put(newPos,e)
        //new Entity(newPos._1,newPos._2)
        e.x = newPos._1
        e.y = newPos._2
      } else {
        // no action taken if a move would collide
      }
    })
  }
  
  def isOccupied(x:Int,y:Int):Boolean = {
    positions.contains((x,y))
  }
  
  def consumeFood = {
    entities.foreach(e => {
      e.addEnergy(food(e.x)(e.y))
      food(e.x)(e.y) = 0
      e.useEnergy
    })
  }
  
  def reap = {
    entities = entities.filter(e => {
      if(e.isDead) positions.remove((e.x,e.y))
      !e.isDead
    })
  }
  
  def reproduceEntities = {
    entities.foreach(e => {
      e.reproduce(this)
    })
  }
  
  def diffuse(field:Array[Array[Double]]):Array[Array[Double]] = {
    val width = field.size
    val height= field(0).size
    var flowSum = 0.0
    //val flow:Array[Array[Double]] = Array.tabulate(width)((i) => {Array.tabulate(height)(j =>0.0))})
    val flow:Array[Array[Double]] = (Array.fill(width,height)(0.0))
    
    for(w <- 0 until width;h <- 0 until height) {
      val fv = field(w)(h)/4.0
      val lowW = wrapCoords(w-1,width)
      val highW = wrapCoords(w+1,width)
      val lowH = wrapCoords(h-1,height)
      val highH = wrapCoords(h+1,height)
      //if(fv > 0 && lowH > 100) println(lowH)
      flow(lowW)(h) = flow(lowW)(h)+fv
      flow(highW)(h) = flow(highW)(h)+fv
      flow(w)(lowH) = flow(w)(lowH)+fv
      flow(w)(highH) = flow(w)(highH)+fv
    }
    flow
  }
    
  
}

object NatSel {
  val world = new World
  world.addEntity(Entity.buildDefault(100,100,1))
  //world.addEntity(Entity.buildDefault(200,200,1))
  //world.addEntity(Entity.buildDefault(400,400,1))
  //List(new Entity(50,50,10),new Entity(200,200,10),new Entity(405,405,10))
  val modelMin = (0,0)
  val modelMax = (world.width,world.height)
  // might keep around between ticks to avoid unnecessary initialization. Skipping for now.
  //val foodFlow = Array.tabulate(800)((i) => {Array.tabulate(800)(j => j/800.0)})
  val span = (world.width,world.height)
  val imP = new ImagePanel
  
  def main(args:Array[String]) = {
    mainFrame.pack()
    mainFrame.visible = true
    
    run(1000)
  }
  
  val mainFrame = new Frame {
      val myIm = new BufferedImage(span._1,span._2,BufferedImage.TYPE_INT_RGB)
      imP.updateIm(myIm)
      imP.getGraphics.drawRect(10,10,100,100)
      //val ent = new Entity(50,50)
	  //drawEntity(ent,imP.getGraphics)
      paintAll
	  contents = imP
	  listenTo(imP.mouse.clicks)
      reactions += {
        case MouseClicked(_,_,_,_,_) => {
        }
      }
  }
    
  def run(ticks:Int):Unit = {
    for(i <- 0 until ticks) {
      Thread.sleep(10)
      world.tick
      paintAll
      mainFrame.repaint()
    }
  }
  

  def paintAll = {
    imP.clear
    val g2 = imP.getGraphics
    
    for(w <- 0 until world.food.size;h <- 0 until world.food(0).size) {
      val foodFloat = world.food(w)(h).floatValue
      //assert(foodFloat <= 1.0,"food is %f at pos %d,%d".format(foodFloat,w,h))
      imP.setColor(new Color(0f,0f,math.min(1f,foodFloat)))
      imP.colorPix(w-modelMin._1,modelMax._2-h-1)
      /*
      g2.setColor(new Color(0f,0f,food(w)(h).floatValue))
      val colorRect = new Rectangle(w-modelMin._1, 
          modelMax._2-h-1,1,1)
      g2.fill(colorRect)
      *
      */
    } 
    for(e <- world.entities) {
      paintEntity(e,g2)
    } 

  }
  
  def paintEntity(e:Entity,g2:Graphics2D) = {
    val st = (e.x,e.y)
    val entRect = new Rectangle(st._1-modelMin._1, 
          modelMax._2-st._2-1,
          1,
          1)
    //println(st._1-modelMin._1)
    imP.setColor(e.color)
    //g2.fill(entRect)
    imP.colorPix(st._1-modelMin._1, 
          modelMax._2-st._2-1)
  }
}

class ImagePanel extends Panel { 
  private val bgColor = Color.BLACK
  private var _imagePath = ""                                                 
  private var bufferedImage:BufferedImage = null                              
  private var g2:Graphics2D = null
  private var colorRgb:Int = 0;
  
  def updateIm(bi:BufferedImage) = {
    this.bufferedImage = bi
    g2 = bi.createGraphics
    this.preferredSize = new Dimension(bi.getWidth,bi.getHeight())
  }
  
  def setColor(c:Color) = {
    this.colorRgb = c.getRGB()
  }
  
  def clear = {
    g2.setColor(bgColor)
    g2.fillRect(0,0,bufferedImage.getWidth,bufferedImage.getHeight)
  }
  
  def getGraphics = {
    g2
  }
  
  def colorPix(w:Int,h:Int) = {
    bufferedImage.setRGB(w,h,colorRgb)
  }
  
  override def paintComponent(g:Graphics2D) = {                                                                           
    if (null != bufferedImage) g.drawImage(bufferedImage, 0, 0, null)         
  }                                                                           
}                                                                             
