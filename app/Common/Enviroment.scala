package Common

/**
  * Created by racruz on 27-03-2017.
  */
abstract class Environment[T] {
  def lookup(x: String):T
  def isEmpty:Boolean
  def head:(String,T)
  def tail:Environment[T]
  def contains(x:String):Boolean


  def keys:List[String]
  def values:List[T]
  def toList:List[(String,T)]
  //not a pure OO method but it is convenient
  def extend(x:String,v:T):Environment[T] = new ExtEnvironment(this,x,v)
}
private class EmptyEnvironment[T] extends Environment[T]{
  override def lookup(x: String): T = throw new Error("Empty environment")

  override def isEmpty: Boolean = true

  override def head: (String,T) = throw new Error("Invalid operation")

  override def tail: Environment[T] = throw new Error("Invalid operation")

  def contains(x:String):Boolean = false

  def keys:List[String]= List()
  def values:List[T]= List()
  def toList:List[(String,T)] = List()

  override def toString: String = "[]"

}
private class ExtEnvironment[T](env:Environment[T],x:String,v:T) extends Environment[T]{
  override def lookup(y: String): T = if(x.equals(y))v else env.lookup(y)
  override def isEmpty: Boolean = false

  override def head: (String,T) = (x,v)

  override def tail: Environment[T] = env

  def contains(y:String):Boolean = if(x.equals(y)) true else env.contains(y)

  def keys:List[String]= env.keys ++ List(x)
  def values:List[T] = env.values ++ List(v)
  def toList:List[(String,T)] = env.toList ++ List((x,v))

  override def toString: String = s"($x,$v)::$env"
}

object Environment{
  def empty[T]():Environment[T] = new EmptyEnvironment[T]
  def extend[T](env:Environment[T],x:String,v:T):Environment[T] = new ExtEnvironment[T](env,x,v)
}
