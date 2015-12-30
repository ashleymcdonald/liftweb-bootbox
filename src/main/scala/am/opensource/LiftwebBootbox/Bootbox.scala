package am.opensource.LiftwebBootbox



import net.liftweb.common.{Full, Empty, Box}
import net.liftweb.http.SHtml
import net.liftweb.http.js._
import net.liftweb.util.Helpers._


/** Object `Bootbox`
  *
  *  @author Ashley McDonald
  *  @version Created on 04/12/2015.
  */
object Bootbox extends Bootbox

/** Trait `Bootbox`
  *
  *  @author Ashley McDonald
  *  @version Created on 04/12/2015.
  */
trait Bootbox {

	/** Case Class `alert`
	  *
	  * @author Ashley McDonald
	  * @version Created on 04/12/2015.
	  * @example
		* {{{
		*  Bootbox.alert( "Hello World" )
		* }}}
	  */
	case class alert(msg:String) extends JsCmd
	{
		val jsMsg: String = msg.encJs
		def toJsCmd: String = s"""bootbox.alert($jsMsg);"""
	}

	/** Case Class `confirm`
	  *
	  * @author Ashley McDonald
	  * @version Created on 04/12/2015.
	  * @example
		* {{{
		*  Bootbox.confirm( "Optional Title", "Are you sure ?", () => JsCmds.Run("console.log('success');") )
		*  Bootbox.confirm( "Are you sure ?", () => JsCmds.Run("console.log('success');") )
		* }}}
	  */
	case class confirm(title:String = "",msg:String, result: () => JsCmd) extends JsCmd
	{
		val jsTitle: String = title.encJs
		val jsMsg: String = msg.encJs
		val jsResult: String = SHtml.ajaxInvoke(result).toJsCmd
		def toJsCmd: String =
			if(title=="")
				s"""bootbox.alert($jsMsg, function(result){if(result) $result });"""
			else
				s"""bootbox.confirm({
				    |    message: $jsMsg,
				    |    title: $jsTitle,
				    |    callback: function(result) {
				    |       if (result) { $result }
				    |    }
				    | });""".stripMargin
	}

	/** Case Class `prompt`
	  *
	  * @author Ashley McDonald
	  * @version Created on 04/12/2015.
	  * @example
		* {{{
	  *  Bootbox.prompt( "Whats your name", "Anonymous", name => JsCmds.Run("console.log('the name was ' + " + name.encJs + ");"))))
	  * }}}
	  */
	case class prompt(title:String,defaultValue:String = "", action: String => JsCmd) extends JsCmd
	{
		val jsTitle: String = title.encJs
		val jsDefaultValue: String = defaultValue.encJs

		def toJsCmd: String =
			s"""bootbox.prompt({
			    |    value: $jsDefaultValue,
			    |    title: $jsTitle,
			    |    callback: function(result) {
			    |      ${SHtml.ajaxCall("result",action).toJsCmd}
			    |    }
			    | });""".stripMargin
	}

	/** Case Class `custom`
	  *
	  * @author Ashley McDonald
	  * @version Created on 04/12/2015.
	  * @example
		* {{{
		*  Bootbox.custom( "Title", "Are you sure ?", List(button("clickMe", () => JsCmds.Run("console.log('success');"))))
		* }}}
	  */
	case class custom(title:String = "",msg:String, buttons: List[button]) extends JsCmd
	{
		val jsTitle: String = title.encJs
		val jsMsg: String = msg.encJs

		def toJsCmd: String =
			s"""bootbox.confirm({
			    |    message: $jsMsg,
			    |    title: $jsTitle,
			    |    buttons: {
			    |      $buttons
			    |    }
			    | });""".stripMargin
	}

	/** Case Class `button`
	  *
	  * @author Ashley McDonald
	  * @version Created on 04/12/2015.
	  */
	case class button(title:String, name:String, cssClass:String, result: () => JsCmd){
		val jsTitle: String = title.encJs
		val jsCssClass: String = cssClass.encJs
		val jsResult: String = SHtml.ajaxInvoke(result).toJsCmd

		override def toString = s"""
		$name: {
	        label: $jsTitle,
	        className: $jsCssClass,
	        callback: function() {
	          $jsResult
	        }
        }"""
	}

	/** Object `button`
	  *
	  * @author Ashley McDonald
	  * @version Created on 04/12/2015.
	  */
	object button {
		def apply(title:String) = new button(title,nextFuncName,"btn-primary", () => JsCmds.Noop)
		def apply(title:String,result: () => JsCmd) = new button(title,nextFuncName,"btn-primary", result)
	}

	import scala.language.implicitConversions
	implicit def buttonListToString(buttons:List[button]):String = buttons.map(_.toString).mkString(",")

}


