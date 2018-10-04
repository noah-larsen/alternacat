package utils.io

object Logging {

  def turnOffApacheCommonLogging(): Unit ={
    System.setProperty("org.apache.commons.logging.Log", "org.apache.commons.logging.impl.NoOpLog")
  }

}
