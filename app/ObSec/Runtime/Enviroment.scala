package ObSec.Runtime

/**
  * Created by racruz on 27-03-2017.
  */
abstract class Environment[T] {
  def lookup(x: String):T
  def isEmpty:Boolean
}
class EmptyEnvironment[T] extends Environment[T]{
  override def lookup(x: String): T = throw new Error("Empty environment")

  override def isEmpty(): Boolean = false
}
class ExtEnvironment[T](env:Environment[T],x:String,v:T) extends Environment[T]{
  override def lookup(y: String): T = if(x.equals(y))v else env.lookup(y)
  override def isEmpty(): Boolean = true
}

object Environment{
  def empty[T]() = new EmptyEnvironment[T]
  def extend[T](env:Environment[T],x:String,v:T) = new ExtEnvironment[T](env,x,v)
}
