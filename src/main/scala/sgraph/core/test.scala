//package sgraph.core
//
//object MyDSL{
//
//}
//
//
//case class Foo[N](e:N => Boolean)
//case class Bar[E](e:E => Boolean)
//
//object testApp extends App{
//
//  trait HalfEdgLike[N,E]
//  case class HalfEdg[N,E](e:E, n:N) extends HalfEdgLike[N,E]
//
//  case class AA[E](e:E) {
//    def ->[N](n:N):HalfEdg[N,E] = HalfEdg(e,n)
//  }
//
//  case class BB[E](e:Bar[E]) {
//    def ->[N](n:Foo[N]):HalfEdg[Foo[N],Bar[E]] = HalfEdg(e,n)
//  }
//
//  case class Nod[N,E](head:N, edgs:HalfEdgLike[N,E])
//
//  def --[E](e:E):AA[E] = AA(e)
//  def --?[E](e:E => Boolean):BB[E] = BB(Bar(e))
//
//  case class --*[N,E,T[E]](e: E => Boolean) extends HalfEdgLike[N,T[E]] {
//    def --->(n: N => Boolean) = HalfEdg(Bar(e), Foo(n))
//  }
//
//  val x3 = Nod[Int,String](13, --("aa")->(12))
//  val x1 = Nod[Foo[Int],Bar[String]](Foo(x => x == 10), --?[String](x => x == "")->(Foo(z => z == 10)))
//  val x2 = Nod[Foo[Int],Bar[String]](Foo(x => x == 10), --*(z => z == "a")--->())
//
//  //implicit def convertor[N,E]((n:N, e:E))
//  def generate[N,E](qNode:Nod[Foo[N],Bar[E]]) = {
//    ///
//  }
//
//
//
//}
