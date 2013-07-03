package natsel2

import java.awt.Color

object Entity {
  
  def buildDefault(x:Int,y:Int,en:Double,dirDet0:Double=.5,dirDet1:Double=0.0,dirEff0:Double=.5,dirEff1:Double= -.5) = {
    val ent = new Entity(x,y,en)
    val d0 = new Detector(dirDet0)
    val d1 = new Detector(dirDet1)
    val c0 = new Computer(diffFunc)
    val e0 = new Effector(dirEff0)
    val e1 = new Effector(dirEff1)
    c0.addParent(d0)
    c0.addParent(d1)
    e0.addParent(c0)
    e1.addParent(c0)
    ent.addOrgan(d0)
    ent.addOrgan(d1)
    ent.addOrgan(c0)
    ent.addOrgan(e0)
    ent.addOrgan(e1)
    ent
  }
  
  def diffFunc = {
    (x:IndexedSeq[Double]) => {
      println("diffFunc on : " + x)
       var c = 0.0
       var sign = 1.0
       for(v <- x) {
         c = c+sign*v
         sign = sign * -1.0
       }
       c
    }
  }
}

class Entity(var x:Int,var y:Int,var energy:Double) {
  
  var organs:IndexedSeq[Organ] = Vector.empty
  var detectors:IndexedSeq[Detector] = Vector.empty
  var effectors:IndexedSeq[Effector] = Vector.empty
  var computers:IndexedSeq[Computer] = Vector.empty
  
  val reproduceOverhead = .1
  val energyUse = .1
  
  def tick(w:World) = {
    for(d <- detectors) {
      d.detect(w, this.x, this.y)
    }
    for(o <- organs) {
      o.update
    }
  }
  
  // Note that organisms can move more than 1 square now if they have multiple effectors
  def chosenMove:(Int,Int) = {
    //((1-math.floor(math.random*3)).intValue(),(1-math.floor(math.random*3)).intValue())
    val effs = effectors.map(e => e.effect)
    val ex = effs.map(_._1).sum
    val ey = effs.map(_._2).sum
    (ex,ey)
  }
  
  def addOrgan(o:Organ) = {
    organs = organs :+ o
    o match {
      case d:Detector => {detectors = detectors :+ d} 
      case e:Effector => {effectors = effectors :+ e}
      case c:Computer => {computers = computers :+ c}
    }
  }
  
  def color:Color = Color.RED
  def isDead = {energy < 0}
  def addEnergy(e:Double) = {
    energy = energy + e
  }
  def useEnergy() = {
    energy = energy - energyUse
  }
  
  def reproduce(world:World) = {
    if(energy > 1) {
      val (childX,childY) = (world.wrapX(x),world.wrapY(y+1))
      if(!world.positions.isDefinedAt((childX,childY))) {
        val newEntity = Entity.buildDefault(childX, childY, energy/(2.0+reproduceOverhead),
            math.random,math.random,
            math.random,math.random)
        //world.addEntity(childX,childY,energy/(2.0+reproduceOverhead))
        world.addEntity(newEntity)
        this.energy = energy/(2.0+reproduceOverhead)
      }
    }
  }
}

/*
 * These are currently placeholders.
 */
trait Organ {
  protected var parents:Vector[Organ] = Vector.empty[Organ]
  protected var parentVal = 0.0
  def addParent(o:Organ) = {
    parents = parents :+ o
  }
  
  def update = {
    parentVal = combineParents(parents.map(_.lastVal))
    //println("update for %s new value is %2.3f".format(this.toString,parentVal))
  }
  
  protected def combineParents(x:IndexedSeq[Double]):Double
  def lastVal:Double
  
  def computeVec(startingDir:Double,param:Double):(Int,Int) = {
    val logSig = 1.0/(1.0+math.exp(-param))
    val angle = math.Pi*(2.0*startingDir+logSig)
    //println("computeVec angle: " + angle)
    val xD = math.cos(angle)
    val yD = math.sin(angle)
    (if(xD>0) 1 else -1,if(yD>0) 1 else -1)
  }
}

class Detector(val direction:Double) extends Organ {
  var lv = 0.0
  override def lastVal = lv
  override def combineParents(x:IndexedSeq[Double]) = {x.sum}
  
  def detect(w:World,x:Int,y:Int) {
    val detectDelta = computeVec(direction,parentVal)
    // poll world at (x+ddx,y+ddy) to get output
    lv = w.food(w.wrapX(x+detectDelta._1))(w.wrapY(y+detectDelta._2))
    //println("detect lv=%f, direction=%f, parentVal=%f, detectDelta=%s".format(lv,direction,parentVal,detectDelta.toString))
  } 
}

class Effector(val direction:Double) extends Organ {
  override def lastVal = parentVal // could be direction
  override def combineParents(x:IndexedSeq[Double]) = {x.sum}
  
  // Just chosen moves at the moment
  def effect:(Int,Int) = {
    //println("effector lastVal: " + lastVal)
    computeVec(direction,lastVal)
  }
}

class Computer(val combFunc:Function1[IndexedSeq[Double],Double]) extends Organ {
  def combineParents(x:IndexedSeq[Double]) = combFunc(x)
  def lastVal = {
    println("compute lv:" + parentVal)
    parentVal
  }
}


