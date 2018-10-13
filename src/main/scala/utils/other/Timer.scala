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

    def asSuffix(chronoUnit: ChronoUnit): String = {
      chronoUnit match {
        case ChronoUnit.DAYS => "d"
        case ChronoUnit.HOURS => "h"
        case ChronoUnit.MINUTES => "m"
        case ChronoUnit.SECONDS => "s"
        case _ => s" ${chronoUnit.name().toLowerCase}"
      }
    }


    def toSeconds(value: Long, units: ChronoUnit): Double = units.getDuration.multipliedBy(value).toNanos.toDouble / TimeUnit.SECONDS.toNanos(1)
    def format(value: Double, nSignificantDigits: Int): String = value.toString match {case x => Some(x.indexWhere(!_.isDigit)).filter(_ != -1).map(y => x.take(y) + x
      .substring(y).take(nSignificantDigits + 1)).getOrElse(x)}
    def sumSeconds(values: Seq[(Long, ChronoUnit)]): Double = values.foldLeft(0.0)((x, y) => x + toSeconds(y._1, y._2))
    val chronoUnits = Seq(ChronoUnit.DAYS, ChronoUnit.HOURS, ChronoUnit.MINUTES, ChronoUnit.SECONDS, ChronoUnit.MILLIS)
    val nSignificantDigits = 2
    convert(nanos, chronoUnits, maxNRelevantUnits) match {
      case x if x.headOption.exists(_._2 == ChronoUnit.SECONDS) => x.head._1 + asSuffix(x.head._2)
      case x if x.headOption.exists(_._2.compareTo(ChronoUnit.SECONDS) < 0) => format(sumSeconds(x), nSignificantDigits) + asSuffix(ChronoUnit.SECONDS)
      case x => Display.withSpaces(x.map(y => y._1.toString + asSuffix(y._2)))
    }

  }


  private def convert(nanos: Long, chronoUnits: Seq[ChronoUnit], maxNRelevantUnits: Int): Seq[(Long, ChronoUnit)] = {

    def convert(duration: Duration, chronoUnit: ChronoUnit): Long = {
      chronoUnit match {
        case ChronoUnit.DAYS => duration.toDays
        case ChronoUnit.HOURS => duration.toHours
        case ChronoUnit.MINUTES => duration.toMinutes
        case ChronoUnit.SECONDS => duration.getSeconds
        case ChronoUnit.MILLIS => duration.toMillis
        case ChronoUnit.NANOS => duration.toMillis
        case _ => duration.get(chronoUnit)
      }
    }


    val duration = Duration.ofNanos(nanos)
    val relevantChronoUnits = Some(chronoUnits.sorted.reverse.filter(convert(duration, _) > 0).take(maxNRelevantUnits)).getOrElse(Seq(ChronoUnit.NANOS))
    relevantChronoUnits.tail.scanLeft((nanos, convert(duration, relevantChronoUnits.head), relevantChronoUnits.head))((x, y) => nanos - Duration.of(x._2, x._3).toNanos match {
      case z => (z, convert(Duration.ofNanos(z), y), y)}).map(x => (x._2, x._3))

  }

}
