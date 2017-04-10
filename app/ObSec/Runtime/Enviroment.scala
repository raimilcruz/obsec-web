package ObSec.Runtime

/**
  * Created by racruz on 27-03-2017.
  */
abstract class Environment[T] {
  def lookup(x: String):T
  def isEmpty:Boolean
  def head:(String,T)
  def tail:Environment[T]
}
class EmptyEnvironment[T] extends Environment[T]{
  override def lookup(x: String): T = throw new Error("Empty environment")

  override def isEmpty: Boolean = true

  override def head: (String,T) = throw new Error("Invalid operation")

  override def tail: Environment[T] = throw new Error("Invalid operation")
}
class ExtEnvironment[T](env:Environment[T],x:String,v:T) extends Environment[T]{
  override def lookup(y: String): T = if(x.equals(y))v else env.lookup(y)
  override def isEmpty: Boolean = false

  override def head: (String,T) = (x,v)

  override def tail: Environment[T] = env
}

object Environment{
  def empty[T]() = new EmptyEnvironment[T]
  def extend[T](env:Environment[T],x:String,v:T) = new ExtEnvironment[T](env,x,v)
}
