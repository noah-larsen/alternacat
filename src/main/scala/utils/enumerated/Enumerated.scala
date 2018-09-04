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


  def withName(name: String, ignoreCase: Boolean = true): Option[T] = {
    values.find(x => if(ignoreCase) x.name.equalsIgnoreCase(name) else x.name == name)
  }


  protected val enumeratedTypes: EnumeratedTypes


  protected val u: JavaUniverse = scala.reflect.runtime.universe
  protected case class EnumeratedTypes(typeSelf: u.Type, classT: Class[T])

}