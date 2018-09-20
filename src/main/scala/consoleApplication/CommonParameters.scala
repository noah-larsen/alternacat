package consoleApplication

import utils.commands.Parameter.{ListParameter, StringsParameter, ValueParameter}

import scala.util.Try

object CommonParameters {

  object Keyword extends StringsParameter
  object MaxDepth extends ValueParameter(x => Try(x.toInt).filter(_ > 0), Some(1))
  object PartOfName extends StringsParameter

}
