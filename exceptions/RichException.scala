package utils.exceptions

class RichException(
                     cause: Option[Throwable] = None
                   ) extends Exception(cause.orNull) {

  override def getMessage: String = {
    ???
  }

}
