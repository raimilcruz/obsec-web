package ObSecG

object Playground{
  def main(args:Array[String]):Unit={
    print(foo[Int,Int](1))
  }
  def foo[T >: Int, T1 >: T](x:T):T1 = x
}
/*class Playground {
  def usage1():Unit={
    val a: SecType[SecString,StringConcat]
  }
}
trait StringConcat{
  def concat[lm>: SecString](other: SecType[lm,SecString]): SecType[lm with StringConcat,SecString]
}
trait SecString{
  //def trim() : SecString[self]
  def trim[lm >: SecString](): SecType[lm,SecString]
  def concat[lthis,lm >: SecString](other : SecType[lm,SecString]): SecType[lm with lthis,SecString]
}
case class SecStringImpl(s:String) extends SecString{
  //def value:String = s
  override def trim[lm >: SecString](): SecType[lm, SecString]
  = SecTypeImpl(SecStringImpl(s))

  override def concat[lthis, lm >: SecString](other: SecType[lm, SecString])
    : SecType[lm with lthis, SecString]
  = throw Exception
}

trait SecType[S2, S1 <:S2]{
  def safetyType() : S1
  def declassificationType() : S2
}
case class SecTypeImpl[S2, S1<:S2](val safety:S1) extends SecType[S2,S1] {
  override def safetyType(): S1 = safety

  override def declassificationType(): S2 = safety
}*/
//type ¬[A] = A => Nothing

