package utils.other

import java.time.Duration
import java.time.temporal.{ChronoUnit, UnsupportedTemporalTypeException}
import java.util.concurrent.TimeUnit

import utils.io.Display

case class Timer(private val prefix: String = new String) {

  def printTime(): Unit ={
    println(time)
  }


  def time: String = {
    val maxNRelevantUnits = 2
    Some(prefix).filter(_.nonEmpty).map(Display.withColonSpace).getOrElse(new String) + display(System.nanoTime() - startTime, maxNRelevantUnits)
  }


  private val startTime = System.nanoTime()


  private def display(nanos: Long, maxNRelevantUnits: Int): String = {

    def display(chronoUnit: ChronoUnit): String = {
      chronoUnit match {
        case ChronoUnit.DAYS => "d"
        case ChronoUnit.HOURS => "h"
        case ChronoUnit.MINUTES => "m"
        case ChronoUnit.SECONDS => "s"
        case ChronoUnit.MILLIS => "ms"
        case _ => chronoUnit.name().toLowerCase
      }
    }

    val chronoUnits = Seq(ChronoUnit.DAYS, ChronoUnit.HOURS, ChronoUnit.MINUTES, ChronoUnit.SECONDS, ChronoUnit.MILLIS)
    Display.withSpaces(convert(nanos, chronoUnits, maxNRelevantUnits).map(x => x._1.toString + display(x._2)))

  }


  private def convert(nanos: Long, chronoUnits: Seq[ChronoUnit], maxNRelevantUnits: Int): Seq[(Long, ChronoUnit)] = {

    def convert(duration: Duration, chronoUnit: ChronoUnit): Long = {
      chronoUnit match {
        case ChronoUnit.DAYS => duration.toDays
        case ChronoUnit.HOURS => duration.toHours
        case ChronoUnit.MINUTES => duration.toMinutes
        case ChronoUnit.SECONDS => duration.getSeconds
        case ChronoUnit.MILLIS => duration.toMillis
        case _ => duration.get(chronoUnit)
      }
    }


    val duration = Duration.ofNanos(nanos)
    val relevantChronoUnits = Some(chronoUnits.filter(convert(duration, _) > 0).take(maxNRelevantUnits)).getOrElse(Seq(ChronoUnit.NANOS))
    relevantChronoUnits.tail.scanLeft((nanos, convert(duration, relevantChronoUnits.head), relevantChronoUnits.head))((x, y) => nanos - Duration.of(x._2, x._3).toNanos match {
      case z => (z, convert(Duration.ofNanos(z), y), y)}).map(x => (x._2, x._3))

  }

}
