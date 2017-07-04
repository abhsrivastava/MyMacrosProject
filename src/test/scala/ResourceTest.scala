/**
  * Created by ASrivastava on 7/3/17.
  */

import org.scalatest._

class ResourceTest extends FunSuite with Matchers{
   test("test of Get, Post, Patch, Put Models") {
      @Resource case class User(
        @get id: Int,
        @get @post @patch name: String,
        @get @post email: String,
        registeredOn: Long)

      """User.Get(id=0, name = "Rick", email="foo@bar.com")""" should compile
      """User.Post(name="Rick", email="foo@bar.com")""" should compile
      """User.Patch(name=Some("Rick"))""" should compile
      """User.Patch()""" should compile
   }
}
