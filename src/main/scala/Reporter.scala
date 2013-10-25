abstract class Reporter {
  def error(msg: Any) : Unit
  def warning(msg: Any) : Unit 
  def info(msg: Any) : Unit 
  def fatalError(msg: Any) : Nothing
}

class DefaultReporter extends Reporter {
  protected val errorPfx   = "[ Error ] "
  protected val warningPfx = "[Warning] "
  protected val infoPfx    = "[ Info  ] "
  protected val fatalPfx   = "[ Fatal ] "

  def output(msg: String) : Unit = {
    /*Console.err.*/println(msg)
  }

  protected def reline(pfx: String, msg: String) : String = {
    val color = if(pfx == errorPfx || pfx == warningPfx || pfx == fatalPfx) {
      Console.RED
    } else {
      Console.BLUE
    }
    "[" + color + pfx.substring(1, pfx.length-2) + Console.RESET + "] " +
    msg.trim.replaceAll("\n", "\n" + (" " * (pfx.size)))
  }

  def error(msg: Any) = output(reline(errorPfx, msg.toString))
  def warning(msg: Any) = output(reline(warningPfx, msg.toString))
  def info(msg: Any) = output(reline(infoPfx, msg.toString))
  def fatalError(msg: Any) = { output(reline(fatalPfx, msg.toString)); exit(0) }
}

class QuietReporter extends DefaultReporter {
  override def warning(msg: Any) = {}
  override def info(msg: Any) = {}
}
