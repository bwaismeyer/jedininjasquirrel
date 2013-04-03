package lucawa.wtftd

// More elegant than keeping extra maps around
class RtmTask(desc:String,p:Double,val rtmId:String,val noteId:String) extends Task(desc,p,None)