package scalagraph.matching
import scalax.collection.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._
import scalax.collection.edge.LDiEdge
import scalax.collection.edge.Implicits._

//trait NodeMatchLike[N] extends Product with Serializable{
//  def eval:(N => Option[N])
//}
//
//case class NodeMatchAND[N](eval:(N => Option[N])) extends NodeMatchLike[N]
//case class NodeMatchANDCons[N](eval:(N => Option[N])) extends NodeMatchLike[N]
//case class NodeMatchOR[N](eval:(N => Option[N])) extends NodeMatchLike[N]
//
//
//trait EdgeMatchLike[E] extends Product with Serializable{
//  def eval:(E => Option[E])
//}
//
//
//case class EdgeMatch[E](eval:(E => Option[E])) extends EdgeMatchLike[E]
//case class EdgeMatchStar[E](eval:(E => Option[E])) extends EdgeMatchLike[E]
//
case class Pentree(x:String)
case class DepPath(y:String)

object Matching  {


}

object app extends App {

  val n1 = Pentree("NP")
  val n2 = Pentree("VP")
  val n3 = Pentree("VP")

  val g = Graph( (n1 ~+#> n2)(DepPath("subj")), (n1 ~+#> n3)(DepPath("obj")))
  val nodes = List(n1,n2,n3)
  val edges = List((n1 ~+#> n2)(DepPath("subj")), (n1 ~+#> n3)(DepPath("obj")))
  val g2 = Graph.from(nodes,edges)
  val g1 = g.edges
}

