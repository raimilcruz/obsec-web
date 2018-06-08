package Common

import scala.collection.mutable

class Scope[T] {

  def lookup(x: String): T = throw new Error(s"Variable ${x} not in Scope")

  def contains(x: String) = false

  def add(x: String, v: T): Unit = throw new Error("Not a functional scope")

  def toList: List[(String, T)] = List()
}

class NestedScope[T](parent: Scope[T]) extends Scope[T] {
  protected val bindings: mutable.HashMap[String, T] = mutable.HashMap.empty[String, T]

  override def lookup(x: String): T = {
    if (bindings.contains(x)) bindings(x)
    else parent.lookup(x)
  }

  override def contains(x: String): Boolean = bindings.contains(x) || parent.contains(x)

  override def add(x: String, v: T): Unit = {
    if (bindings.contains(x))
      throw new Error("Variable is already in this scope")
    bindings(x) = v
  }

  override def toList: List[(String, T)] = parent.toList ++ bindings.toList
}


