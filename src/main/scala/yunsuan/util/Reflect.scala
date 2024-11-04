package yunsuan.util

object Reflect {

  import scala.reflect.runtime.universe._

  val mirror = runtimeMirror(this.getClass.getClassLoader)

  def getUIntWidthOfObject(a: Any): Int = {
    val objMirror = mirror.reflect(a)
    require(objMirror.symbol.isModuleClass, "The arg passed to getMaxUIntWidthOfObject should be an object")

    val uintFields = objMirror.symbol.info.decls.collect {
      case field: TermSymbol if field.info =:= typeOf[chisel3.UInt] => field
    }

    val widthes = uintFields
      .map(field => objMirror.reflectField(field).get.asInstanceOf[chisel3.UInt])
      .map(_.getWidth)

    require(widthes.forall(_ == widthes.head), s"The width in object $objMirror is not equal. " +
      s"They are ${uintFields.map(x => x.name -> objMirror.reflectField(x).get.asInstanceOf[chisel3.UInt].getWidth)}")
    widthes.head
  }

  def getUIntMaxWidthOfObject(a: Any): Int = {
    val objMirror = mirror.reflect(a)
    require(objMirror.symbol.isModuleClass, "The arg passed to getMaxUIntWidthOfObject should be an object")

    objMirror.symbol.info.decls.collect {
      case field: TermSymbol if field.info =:= typeOf[chisel3.UInt] => field
    }
      .map(field => objMirror.reflectField(field).get.asInstanceOf[chisel3.UInt].getWidth)
      .max
  }
}
