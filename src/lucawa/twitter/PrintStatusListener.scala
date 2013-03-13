package lucawa.twitter

import java.io.FileInputStream
import java.io.ObjectInputStream
import java.io.FileOutputStream
import java.io.ObjectOutputStream
import twitter4j._
import twitter4j.conf._
import scala.collection.JavaConversions._
import scala.collection.mutable.ArrayBuffer

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