package lucawa.wtftd

// More elegant than keeping extra maps around
class RtmTask(desc:String,p:Double,context:Set[String],
    val rtmListId:String,val rtmSeriesId:String,val rtmTaskId:String,val rtmNoteId:String) extends Task(desc,p,None,context)