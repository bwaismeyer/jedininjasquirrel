package lucawa.wtftd
import java.security._
import org.apache.commons.codec.digest.DigestUtils
import clTools.HttpGetter
import java.net.URL
import scala.xml._

object RtmWtdIo  {
	val serviceUrl = "http://api.rememberthemilk.com/services/rest/?";
	val authUrl = "http://api.rememberthemilk.com/services/auth/?";


	def main(args:Array[String]):Unit = {
			val t = if(args.size > 2) Some(args(2)) else None;

			val rwi = new RtmWtdIo(args(0),args(1),t,true);

			val w = rwi.readWtftd;
			println(w.printAllTasks)
			val textWrite = new TextWtdIo("data/rtmTodoList.txt")
			textWrite.syncWtftd(w)
			val nt = w.getNextTask().asInstanceOf[RtmTask];
			//val noteResult = rwi.addNote("bloo","bla\nblah\nbla",nt)
			//addNote(nt.rtmNoteId,"testWsidNote","<hello>blar</hello>",timeline,nt.rtmListId,nt.rtmSeriesId,nt.rtmTaskId)
			//println(noteResult)
			//wsidifyByFilter("tag:tmp",10.0)
	}
}

class RtmWtdIo(ak:String,val secret:String,t:Option[String],useAllTasks:Boolean=false) extends WtdIo {
	var apiKeyPair:(String,String) = ("" -> "");
	var token:String = "";
	var timeline="";

	apiKeyPair = ("api_key" -> ak);
	token = t.getOrElse(getNewToken);
	if(!isTokenValid(token)) {
		token = getNewToken
	}
	timeline = createTimeline;

	def readWtftd:Wtftd = {
		val w = new Wtftd;
		initWsidTasks(w)
		w
	}

	def syncWtftd(w:Wtftd):Boolean = {
		System.err.println("Not implemented")
		false
	}


	def invokeMethod(methodName:String,extraParams:Map[String,String]) = {
		val params = Map("auth_token" -> token,apiKeyPair,"method" -> methodName) ++ extraParams;
		val tlUrl = signedRequest(RtmWtdIo.serviceUrl,params);
		XML.load(tlUrl);
	}

	def createTimeline:String = {
		val tList = invokeMethod("rtm.timelines.create",Map.empty);
		println(tList)
		val tlElement = (tList \ "timeline");
		tlElement.text
	}

	def getTaskXml(filter:String="") = {
		val params = Map("auth_token" -> token,apiKeyPair,"method" -> "rtm.tasks.getList");
		val finalParams = if(filter.length > 0) {
			params + ("filter" -> filter)
		} else {
			params
		}
		val tlUrl = signedRequest(RtmWtdIo.serviceUrl,finalParams);
		val tList = XML.load(tlUrl);;
		val pp = new scala.xml.PrettyPrinter(30,4)
		println(pp.format(tList))
		println("==========================================")
		tList
	}


	def addNote(noteTitle:String,noteText:String,t:RtmTask):Elem = {
		addNote(t.rtmNoteId,noteTitle,noteText,timeline,t.rtmListId,t.rtmSeriesId,t.rtmTaskId)
	}

	def addNote(noteId:String,noteTitle:String,noteText:String,
			timeline:String,listId:String,seriesId:String,taskId:String):Elem = {
		val noteParams = Map(
			("timeline" -> timeline),
			("list_id" -> listId),
			("taskseries_id" -> seriesId),
			("task_id" -> taskId),
			("note_title" -> noteTitle),
			("note_text"->noteText))
			println(noteParams)
			invokeMethod("rtm.tasks.notes.add",noteParams)
	}

	def wsidifyByFilter(filt:String,defaultPriority:Double) = {
		def hasWsidTag(tagSeq:NodeSeq):Boolean = {
				val tagText = for(tagI <- 0 until tagSeq.size) {
					if(tagSeq(tagI).text == "wsid") return true
				}
				return false
		}
		val tList = getTaskXml(filt)
				val listNodes:NodeSeq = tList \ "tasks" \ "list";
		val rtmListId = listNodes(0).attribute("id").get.text;
		val seriesNodes:NodeSeq = tList \ "tasks" \ "list" \ "taskseries";
		for(i <- 0 until seriesNodes.size) {
			println("----------------" + i)
			val currentSeries = seriesNodes(i);
			val rtmSeriesId = currentSeries.attribute("id").get.text;
			val rtmName = currentSeries.attribute("name").get.text;
			println("RTM Name: " + rtmName)
			val rtmId = currentSeries.attribute("id").get.text;
			val rtmTags = currentSeries \ "tags" \ "tag";
			val isWsidNode = hasWsidTag(rtmTags);
			if(!isWsidNode) {
				//val notes = currentSeries \ "notes" \ "note"
				//for(n <- 0 until notes.size) {
				//
				//}     
			}
		}
	}


	def initWsidTasks(w:Wtftd) = {
		val tList = getTaskXml(if(useAllTasks) "" else "tag:wsid");
		val listNodes:NodeSeq = tList \ "tasks" \ "list";

		// A map from wtftd ids to taskseries ids
		var idMap = Map.empty[String,String];
		var taskMap = Map.empty[String,RtmTask];
		var childMap = Map.empty[RtmTask,Set[String]];
		for(li <- 0 until listNodes.size) {
			val currentList = listNodes(li);
			val taskSeriesNodes:NodeSeq = currentList \ "taskseries";
			val rtmListId = currentList.attribute("id").get.text;
			for(i <- 0 until taskSeriesNodes.size) {
				println("----------------" + i)
				val currentSeries = taskSeriesNodes(i);
				val rtmName = currentSeries.attribute("name").get.text;
				println("RTM Name: " + rtmName)
				val rtmSeriesId = currentSeries.attribute("id").get.text;
				val notes = currentSeries \ "notes" \ "note";
				// At the moment, only the last task in a series (presumably this is about repeating tasks) is considered
				val currentTasks = (currentSeries \ "task");
				val currentTask = currentTasks(currentTasks.size-1);
				val rtmTaskId = currentTask.attribute("id").get.text;
				val (wsidPriority:Double,wsidContexts:Set[String],wsidId:String,noteId:String,childSet:Set[String]) = notesToWsidFeatures(notes).getOrElse{
				  if(useAllTasks) (0.0,None,genRandomId,"",Set.empty[String])
				}

				val curTask = new RtmTask(rtmName,wsidPriority,wsidContexts,rtmListId,rtmSeriesId,rtmTaskId,noteId)
				taskMap = taskMap + (wsidId -> curTask)
				childMap = childMap + (curTask -> childSet);
				idMap = idMap + (wsidId -> rtmSeriesId);
				println("Created rtmTask : " + curTask + ", with wsidId=" + wsidId + ", map sizes=" + (taskMap.size,childMap.size,idMap.size))
			}
		}
		println("taskMap: " + taskMap)
		for(cp <- childMap) {
			val parent = cp._1;
			val childIds = cp._2;
			for(childId <- childIds) {
				println(childId)
				val child = taskMap.get(childId).get;
				parent.addChild(child)
				child.setParent(parent)
			}
		}
		println(taskMap)
		w.addTopLevelTasks(taskMap.values)
	}

	def genRandomId = {
		// Time-based collisions should be extremely rare, and the random element should make those a non-issue
	    System.currentTimeMillis() + "_" + System.nanoTime() + "_" + "%8.0f".format(math.random*1E8)
	}

	def notesToWsidFeatures(notes:scala.xml.NodeSeq):Option[(Double,Set[String],String,String,Set[String])] = {
		for(n <- 0 until notes.size) {
			val thisNote = notes(n)
					val noteTitle = thisNote.attribute("title").get.text;
			// We've found data for the note
			if(noteTitle == "wsidData") {
				println("Found data note...")
				val noteId = thisNote.attribute("id").get.text
				val noteText = thisNote.text
				assert(noteText.contains("<wsid"))
				val nXml = XML.loadString(noteText)
				val wsidId = nXml.attribute("id").get.text
				println("note id:" + noteId)
				println("wsid id:" + wsidId)
				val childNodes =  (nXml \ "child")
				println("wsid children: " + childNodes)
				var childSet = Set.empty[String];
				for(ci <- 0 until childNodes.size) {
					childSet = childSet + childNodes(ci).attribute("id").get.text
				}
				val wsidPriority = nXml.attribute("priority").get.text.toDouble;
				val contextAtt = nXml.attribute("context");
				val wsidContext:Set[String]  = if(contextAtt.isDefined) contextAtt.get.text.split(",").toSet else Set.empty;
				return Some((wsidPriority,wsidContext,wsidId,noteId,childSet))
			}
		}
		return None
	}


			def getNewToken = {
					val myFrob = getFrob;
					browseToUri(authUrl(myFrob))
					val tokenUrl = signedRequest(RtmWtdIo.serviceUrl,Map("frob" -> myFrob,apiKeyPair,"method" -> "rtm.auth.getToken"))
					Thread.sleep(5000)
					println(tokenUrl)
					val tokenXml = XML.load(new URL(tokenUrl));
					println(tokenXml)
					println("alt:")
					val tokenNode = (tokenXml \ "auth" \ "token");
					val myToken = tokenNode(0).text;
					println("my token: " + myToken)
					myToken
			}

			def isTokenValid(t:String):Boolean = {
					val tokenCheckUrl = signedRequest(RtmWtdIo.serviceUrl,Map("auth_token" -> t,apiKeyPair,"method" -> "rtm.auth.checkToken"))
							val tokenCheck = XML.load(tokenCheckUrl);
					println("token check: " + tokenCheck)
					tokenCheck.attribute("stat").get.text == "ok"
			}

			def authUrl(frob:String):String = {
					val authParams = Map(apiKeyPair,"perms" -> "delete","frob" -> frob)
							signedRequest(RtmWtdIo.authUrl,authParams)
			}

			def getFrob = {
					val params = Map("method" -> "rtm.auth.getFrob",apiKeyPair)
							println("getFrob params: " + params)
							val frobXml = scala.xml.XML.load(new URL(signedRequest(RtmWtdIo.serviceUrl,params)))
							frobXml.child.text
			}

			def signedRequest(baseUrl:String,params:Map[String,String]):String = {
					val signature = getApiSig(params)
							baseUrl + urlifyParams(params + ("api_sig" -> signature))
			}

			def urlifyParams(params:Map[String,String]) = {
				val r = params.map(p => p._1 + "="+p._2)
						r.mkString("&")
			}

			def getApiSig(params:Map[String,String]):String = {
				val sb = new StringBuffer(secret)
				val pairList:IndexedSeq[String] = params.map((f:(String,String)) => f._1 + f._2).toIndexedSeq.sorted
				pairList.foreach(sb.append(_))
				val s = sb.toString
				DigestUtils.md5Hex(s)
			}

			def browseToUri(myUri:String) = {
				if( !java.awt.Desktop.isDesktopSupported() ) {
					System.err.println( "Desktop is not supported (fatal)" );
					System.exit( 1 );
				}
				val desktop = java.awt.Desktop.getDesktop();
				if( !desktop.isSupported( java.awt.Desktop.Action.BROWSE ) ) {
					System.err.println( "Desktop doesn't support the browse action (fatal)" );
					System.exit( 1 );
				}
				val uri = new java.net.URI( myUri );
				desktop.browse( uri );
			}
		}
