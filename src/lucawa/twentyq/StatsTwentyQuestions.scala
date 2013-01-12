/**
 * An extremely simple implementation of twenty questions for statistical tests. At the moment, it doesn't record much useful information,
 * or save the results to disk, or do anything remotely clever. But it's a start...
 */
package lucawa.twentyq

object StatsTwentyQuestions {

	var root:TQNode = StatTest("Binomial test","Binomial test","")

	def main(args:Array[String]):Unit = {
	println("Start")

	//io.Source.stdin.getLine(3)
	var currentNode = root
	var latestQ:Option[Question] = None
	var latestAns:Boolean = false

	while(true) {
		currentNode match {
		  case question:Question => {
			latestQ = Some(question)
			println(currentNode.asInstanceOf[Question].questionText)
			val latestAns = getYNAns
			if(latestAns) {currentNode = question.yesChild} else {currentNode = question.noChild}
		  }
		  case test:StatTest => {
			println("Are you thinking of " + test.shortName + "?")
			if(getYNAns) {
				println("sweet!")
			} else {
				println("What test did you have in mind?")
				val newTest = recordTest
				println("What yes/no question would distinguish that test from " + test.shortName + "?")
				val qText = getLine
				println("What is the right answer?")
				val (newQY,newQN) = if(getYNAns) {
					(newTest,test)
				} else {
					(test,newTest)
				}
				val newQ = Question(qText,newQY,newQN)
				//val qToUpdate = latestQ.getOrElse(root)
				if(latestQ.isDefined) {
				  if (latestAns) {
					latestQ.get.yesChild = newQ
				  } else {
					latestQ.get.noChild = newQ
				  }
				} else {
				  println("updated the root question")
				  root = newQ

				}			
			}
			println("another round!")
			latestQ = None
			currentNode=root
		  }
		  case _ => {throw new RuntimeException("invalid node type")}
	  }

	}
	}

def getLine = io.Source.stdin.getLines.next()
def recordTest:StatTest = {
		val shortName = io.Source.stdin.getLines.next()
		StatTest(shortName,"","")
}

def getYNAns:Boolean = {
			val answer = io.Source.stdin.getLines.next()
			val lcAns = answer.toLowerCase()
			lcAns match {
			  case "yes" => true
			  case "y" => true
			  case "1" => true
			  case "no" => false
			  case "n" => false
			  case "0" => false
			  case s:String => {
			    println(s+ " isn't a yes/no answer!")
			    getYNAns
			  }
			}  
		}

}

trait TQNode {
	def isTerminal:Boolean
}

case class Question(val questionText:String,var yesChild:TQNode,var noChild:TQNode) extends TQNode {
	def isTerminal = false
}

case class StatTest(val shortName:String,val longName:String,val referenceUrl:String) extends TQNode {
	def isTerminal = true

}