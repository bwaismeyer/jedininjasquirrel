package lucawa.wtftd

trait WtdIo {

  def readWtftd:Wtftd
  // true if success, false if failure. Should leave original untouched under failure...
  def syncWtftd(w:Wtftd):Boolean
  
}