package sgraph

/**
 * Created by Omid on 10/27/2015.
 */
package object core {

  type NodeMatch[N] = NodeMatchAND[N]
  type QueryLike[N,E] = DGraph[NodeMatchLike[N], EdgeMatchLike[E]]
  val QueryLike = DGraph
//  object QueryLike {
//    val instance = DGraph
//  }

}
