/**
  * Created by ASrivastava on 7/3/17.
  */
import scala.annotation.StaticAnnotation
import scala.meta._

class get extends StaticAnnotation
class post extends StaticAnnotation
class patch extends StaticAnnotation
class put extends StaticAnnotation

class Resource extends StaticAnnotation{
   inline def apply(defn: Any) : Any = meta {
      val (cls, obj) = defn match {
         case q"${cls: Defn.Class}; ${obj: Defn.Object}" => (cls, obj)
         case cls: Defn.Class => (cls, q"object ${Term.Name(cls.name.toString)}")
         case _ => abort("@Resource can only be put on classes")
      }

      val list = for {
         param <- cls.ctor.paramss.flatten
         modifier <- param.mods
         newParam <- modifier match {
            case mod"@get" | mod"@post" | mod"@put" => Some(param.copy(mods = Nil))
            case mod"@patch" => Some(param"${param.name}: Option[${param.decltpe.get.asInstanceOf[Type]}] = None")
            case _ => None
         }
      } yield modifier -> newParam
      val newModels = list.groupBy(_._1.toString).mapValues(_.map(_._2)).map{case (name, paramList) =>
         val className = Type.Name(name.stripPrefix("@").capitalize)
         q"case class ${className}(..${paramList})"
      }
      val newObj = obj.copy(templ = obj.templ.copy(stats = Some(obj.templ.stats.getOrElse(Nil) ++ newModels)))
      q"$cls; $newObj"
   }
}
