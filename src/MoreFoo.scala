/**
 * Created by User on 1/19/2015.
 */
import swing._

object MoreFoo extends SimpleSwingApplication {

  def top = new MainFrame {
      title = "Hello, World!"
      contents = new Button {
        text = "Click Me!"
      }
    }



}
