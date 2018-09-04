package consoleApplication

import utils.commands.Parameter.{ListParameter, StringsParameter, ValueParameter}

import scala.util.Try

object CommonParameters {

  object MaxDepth extends ValueParameter(x => Try(x.toInt).filter(_ > 0), Some(1))
  object Keyword extends StringsParameter

}
