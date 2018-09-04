package utils.io

import java.util.concurrent.TimeUnit

import scala.util.Try

object Retry {

  def apply[T](f: => Try[T], maxNAttempts: Int = Int.MaxValue, nSecondsDelay: Int = 0, uponRetry: PartialFunction[Exception, Unit] = {case _ =>}): Try[T] = {
    maxNAttempts match {
      case 1 => f
      case x if x > 1 => f.recoverWith{
        case e: Exception =>
          uponRetry(e)
          TimeUnit.SECONDS.sleep(nSecondsDelay)
          apply(f, maxNAttempts - 1, nSecondsDelay, uponRetry)
      }
      case _ => throw new IllegalArgumentException
    }
  }

}
