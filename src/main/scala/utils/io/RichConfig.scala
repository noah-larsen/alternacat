package utils.io

import com.typesafe.config.Config
import com.typesafe.config.ConfigException.Missing

import scala.util.Try

trait RichConfig {

  implicit class RichConfig(config: Config){

    def get[T](path: String, configToPathToValue: Config => String => T): Option[T] = {
      Try(Some(configToPathToValue(config)(path))).recover{case e: Missing => None}.get
    }


    def get(path: String): Option[String] = {
      get(path, _.getString)
    }

  }

}
