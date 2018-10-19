package utils.enumerated


import scala.reflect.ClassTag
import scala.reflect.api.JavaUniverse
import scala.util.{Success, Try}

trait Enumerated {

  type T <: SelfNamed


  def values: Seq[T] = {
    enumeratedTypes.typeSelf.decls.filter(_.isModule).map(x => Try(u.runtimeMirror(this.getClass.getClassLoader).reflectModule(x.asModule))
      .recover{case e: ScalaReflectionException => u.runtimeMirror(this.getClass.getClassLoader).reflect(this).reflectModule(x.asModule)}
      .get.instance).collect{case x if enumeratedTypes.classT.isInstance(x) => x.asInstanceOf[T]}.toSeq
  }


  def withName(name: String, ignoreCase: Boolean = true, normalizeSpaceChars: Boolean = true, trim: Boolean = true): Option[T] = {
    val normalSpace = " "
    def process(value: String) = Some(value).map(x => if(ignoreCase) x.toLowerCase else x).map(x => if(normalizeSpaceChars) x.map(y => if(y.isSpaceChar) normalSpace else y).mkString else x).map(x => if(trim) x.trim else x).get
    values.find(x => process(x.name) == process(name))
  }


  protected val enumeratedTypes: EnumeratedTypes


  protected val u: JavaUniverse = scala.reflect.runtime.universe
  protected case class EnumeratedTypes(typeSelf: u.Type, classT: Class[T])

}