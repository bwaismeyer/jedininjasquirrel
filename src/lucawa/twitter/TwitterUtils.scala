/** This depends on version 3.0.3 of the twitter4j library, which is freely available under
 * an Apache 2.0 license. It is a modified version of Chris's TwitterQueries.scala, which uses an earlier
 * version of twitter4j.
 **/
package lucawa.twitter

import scala.collection.immutable.HashMap
import java.io.FileInputStream
import java.io.ObjectInputStream
import java.io.FileOutputStream
import java.io.ObjectOutputStream
import twitter4j._
import twitter4j.conf._
import scala.collection.JavaConversions._
import scala.collection.mutable.ArrayBuffer

object TwitterUtils {

  var oAConsumerKey = ""
  var oAConsumerSecret = ""
  var oAToken = ""
  var oATokenSecret = ""
  
  val logLevel = 10
  val config = buildConfiguration
  val twitter = new TwitterFactory(config).getInstance()
	
  // The current implementation depends on caches, stored in the cache/ in the working directory.
  // To eliminate cache storing, the cache read/write code can be removed.
  val followerCachePath = "cache/ttFollowerCache.serialized"
  var followerCache:HashMap[String,Set[Long]] = HashMap.empty[String,Set[Long]] 
  val friendCachePath = "cache/ttFriendCache.serialized"
  var friendCache:HashMap[String,IndexedSeq[Long]] = HashMap.empty[String,IndexedSeq[Long]]
  val userCachePath = "cache/ttUserCache.serialized"
  var userCache:HashMap[Long,User] = HashMap.empty[Long,User]
  
  // Reading the two caches above
  readCache()
  
  log("userCache size = %d".format(userCache.size))
  log("friendCache size = %d".format(friendCache.size))
  log("friendCache keys = %s".format(friendCache.keys.toString))
  log("followerCache size = %d".format(followerCache.size))
  log("followerCache keys = %s".format(followerCache.keys.toString))
  
  def main(args:Array[String]):Unit = {
    assert(args.length == 4,"There must be four arguments passed, containing credentials")
    this.oAConsumerKey = args(0)
    this.oAConsumerSecret = args(1)
    this.oAToken = args(2)
    this.oATokenSecret = args(3)
    listenToStream(Vector("sick","flu","sneezing"))
  }
  
  def log(s: => String,lev:Int=10) {
    if(lev <= logLevel) {
      println("Log: " + s)
    }
  }
  
  def getRemainingHits(resType:String):Int = {
    val rlStatus = twitter.getRateLimitStatus(resType)
    rlStatus.get().getRemaining()
  }
  
  def getRefreshSeconds(resType:String):Int = {
    twitter.getRateLimitStatus(resType).get(resType).getSecondsUntilReset()
  }
  
  private def buildConfiguration:Configuration = {
    val cb:ConfigurationBuilder = new ConfigurationBuilder();
    cb.setDebugEnabled(true)
    cb.setOAuthConsumerKey(oAConsumerKey)
    cb.setOAuthConsumerSecret(oAConsumerSecret)
    cb.setOAuthAccessToken(oAToken)
    cb.setOAuthAccessTokenSecret(oATokenSecret);
    //val tf = new TwitterFactory(cb.build());
    //Twitter twitter = tf.getInstance();
    log("TwitterQueries: building configuration")
    cb.build
  }
  /**
   * Looks up users by screenName. Note that this method does not have access to the cache (which is keyed by Long userIds)
   * but will be used to populate the cache.
   */
  def lookupUser(screenName:String):User = {
    val newUser = twitter.lookupUsers(Array(screenName)).head
    userCache = userCache + (newUser.getId() -> newUser)
    newUser
  }
  
  /**
   * @param maxHits number of twitter hits if there are uncached users.
   * Users earlier in the indexedSeq will be returned first, but there is no guarantee
   * that a given user will be looked up unless maxHits*100 > usersIds.length
   */  
  def lookupUsers(userIds:Seq[Long],maxHits:Int=1):Set[User] = {
    var retSet = Set.empty[User]
    val lookupBuffer = new ArrayBuffer[Long]
    for(uid <- userIds) {
      if(userCache.containsKey(uid)) {
        log("user cache hit",20)
        retSet = retSet + userCache.get(uid).get
      } else {
        log("user cache miss",20)
        lookupBuffer.append(uid)
      }
    }
    val remainingGroups = lookupBuffer.toIndexedSeq.grouped(100).toIndexedSeq
    log("initial remainingGroups size=%d, maxHits=%d".format(remainingGroups.size,maxHits))
    val apiHitRounds = math.min(remainingGroups.size,maxHits)
    log("Planned API hits: " + apiHitRounds)
    log("initial remainingGroups size=%d, maxHits=%d".format(remainingGroups.size,maxHits))
    for(gi <- 0 until apiHitRounds) {
      try {
        log("remainingGroups size: " + remainingGroups.size)
        val currentGroup = remainingGroups(gi)
	    val newUserSet = twitter.lookupUsers(currentGroup.toArray).toSet
	    newUserSet.foreach((u:User) => {
	      userCache = userCache + (u.getId -> u)
	      retSet = retSet + u
        })
      } catch {
      	  case e:TwitterException => {
          log("TwitterException: " + e.toString())
          e.getStatusCode match {
            case 404 => {"404 suggests user wasn't found, attempting to continue"}
            case 502 => {"502 : twitter is down or being upgraded."}
            case _ => {throw new RuntimeException("Unknown exception type, may be rate-limiting")}
          }
        }
      }
    }
    retSet
  }
  
  def getFriendsForId(userName:String):IndexedSeq[Long] = {
    if(friendCache.isDefinedAt(userName)) {
      log("friend cache hit for " + userName)
      friendCache.get(userName).get
    } else {
      log("friend cache miss for " + userName)
      val friendSeq = try {
      val friendSeqTry = twitter.getFriendsIDs(userName,-1).getIDs
      friendCache = friendCache + ((userName,friendSeqTry))
      friendSeqTry
      } catch {
      case e:TwitterException => {
          e.getStatusCode() match {
            case 404 => {
              log("user missing, returning empty vector.")
              return Vector.empty[Long]
            }
            case _ => {
              log("unhandled exception status: %d",e.getStatusCode)
              throw new RuntimeException(e)
            }
          }
        }
      }
      friendSeq
    }  
  }
  
  def getFollowersForId(userId:String):Set[Long] = {
    val res = followerCache.get(userId)
    if(res.isDefined) {
      log("cache hit for getFollowersForId: " + userId)
      res.get
    } else {
      log("cache miss for getFollowersForId: " + userId)
      val newRes = getFollowersForIdUncached(userId)
      log("Found " + newRes.size + " followers.")
      followerCache = followerCache.+((userId,newRes))
      newRes
    }
  }
  
  def getFollowersForIdUncached(userId:String):Set[Long] = {
    val lookupResults = try {
      twitter.lookupUsers(Array(userId))
    } catch {
      case _ => {
        log("getFollowersForIdUncached failed on userId %s".format(userId))
        throw new RuntimeException("Cannot continue without user info.")
      }
    }
    val firstU = lookupResults.get(0)
    log("Getting followers for:" + firstU)
    log("UserId: " + firstU.getId)
    if(firstU.isProtected()) {
      log("User is projected, can't return followers")
      Set.empty[Long]
    } else {
      val followerCount = firstU.getFollowersCount()
      log("Listed follower count is " + followerCount)
      //val followerList = twitter.getFollowersIDs(firstU.getId(),1)
      var followerSet = Set.empty[Long]
                                  
      var myFollowerList = twitter.getFollowersIDs(userId,-1)
      followerSet = followerSet ++ myFollowerList.getIDs()

      var nextCursor = myFollowerList.getNextCursor()
      var batches = 0;
      val maxBatches = 5;
      while (nextCursor != 0 && batches < maxBatches) {
        myFollowerList = twitter.getFollowersIDs(userId,nextCursor)
        nextCursor = myFollowerList.getNextCursor()  
        followerSet = followerSet ++ myFollowerList.getIDs()
        batches = batches+1
      }
      followerSet
    }
  }
  
  def readCache() = {
    def deSerializeHelper(path:String):Option[Any] = {
      var retObj:Option[Any] = Some(Unit)
      try {
        val fileIn = new FileInputStream(path);
	    val in = new ObjectInputStream(fileIn);
        retObj = Some(in.readObject())
        in.close();
        fileIn.close();
        log("Read cache object, class: " + retObj.get.getClass)
      } catch {
        case e:Exception => {
          log("Couldn't read cache object: " + e.getMessage())
          log("Returning 'None' for cache object.")
          retObj=None 
        }
      }
      retObj
    }
    
    //try {
      val ucOpt = deSerializeHelper(userCachePath).asInstanceOf[Option[Any]].getOrElse({
        log("new user cache");HashMap.empty[Long,User]})
      userCache = ucOpt.asInstanceOf[HashMap[Long,User]]
      //println("userCache: " + userCache)
      
      val followOpt = deSerializeHelper(followerCachePath).asInstanceOf[Option[Any]].getOrElse({
        log("new follow cache");HashMap.empty[String,Set[Long]]})
      followerCache = followOpt.asInstanceOf[HashMap[String,Set[Long]]]
      //println("followerCache: " + followerCache)
      
      val friendOpt = deSerializeHelper(friendCachePath).asInstanceOf[Option[Any]].getOrElse({
         log("new friend cache");
        HashMap.empty[String,IndexedSeq[Long]]})
      friendCache   = friendOpt.asInstanceOf[HashMap[String,IndexedSeq[Long]]]
      //println("friendCache: " + friendCache)
    //} catch {
    //  case e:Exception => {
    //    log(e.getStackTraceString)
    //    log("Couldn't read cache and problem with fallback.")
    //  }
    // }
  }

  def writeCache() = {
     def serializeHelper(path:String,obj:Serializable) {
       try{
         val fileOut = new FileOutputStream(path);
         val out =  new ObjectOutputStream(fileOut);
         out.writeObject(obj);
         out.close();
         fileOut.close();
       }
      }
     serializeHelper(followerCachePath,followerCache)
     serializeHelper(friendCachePath,friendCache)
     serializeHelper(userCachePath,userCache)
  }
  
  
  def listenToStream(keywords:IndexedSeq[String]):Unit = {
    val listener = new PrintStatusListener
    val config = buildConfiguration
    val twitterStream:TwitterStream = (new TwitterStreamFactory(config)).getInstance();
    var filterQuery = new  FilterQuery()
    twitterStream.addListener(listener)
    filterQuery = filterQuery.track(keywords.toArray)
    twitterStream.filter(filterQuery)
    Thread.sleep(5000)
   
  }
  
}

class PrintStatusListener extends StatusListener {
 def onStatus(status:Status) {
            System.out.println("@" + status.getUser().getScreenName() + " - " + status.getText());
        }

def onDeletionNotice(statusDeletionNotice:StatusDeletionNotice) {
            System.out.println("Got a status deletion notice id:" + statusDeletionNotice.getStatusId());
        }

def onTrackLimitationNotice(numberOfLimitedStatuses:Int) {
            System.out.println("Got track limitation notice:" + numberOfLimitedStatuses);
        }

def onScrubGeo(userId:Long,upToStatusId:Long) {
            System.out.println("Got scrub_geo event userId:" + userId + " upToStatusId:" + upToStatusId);
        }

def onStallWarning(sw:StallWarning) = {
  throw new RuntimeException(sw.getMessage())
}

def onException(ex:Exception) {
            ex.printStackTrace();
        }
}