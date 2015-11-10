package dgraph

import scala.collection.immutable.Queue

trait NodeMatchLike[N] extends Product with Serializable{
  def eval:(N => Boolean)
}

//case class NodeMatch[N](eval:(N => Option[N])) extends NodeMatchLike[N]

case class NodeMatchAND[N](eval:(N => Boolean)) extends NodeMatchLike[N]
case class NodeMatchANDCons[N](eval:(N => Boolean)) extends NodeMatchLike[N]
case class NodeMatchOR[N](eval:(N => Boolean)) extends NodeMatchLike[N]





trait EdgeMatchLike[E] extends Product with Serializable{
  def eval:(E => Boolean)
}


case class EdgeMatch[E](eval:(E => Boolean)) extends EdgeMatchLike[E]
case class EdgeMatchStar[E](eval:(E => Boolean)) extends EdgeMatchLike[E]

case class Pentree(x:String)
case class DepPath(y:String)


object tmp extends App{

}

case class Branch[N](solution:Map[Node[NodeMatchLike[N]], Node[N]],
                       goals:Map[Node[N], Node[NodeMatchLike[N]]]) {

  def getMatchedGraph[E](g:DGraph[N,E], q:DGraph[NodeMatchLike[N] , EdgeMatchLike[E]])= {
    var gr = DGraph.empty[N,E]
    val edgeMap = collection.mutable.Map.empty[DEdge[EdgeMatchLike[E]], DEdge[E]]
    for(((from, to), e) <- q.edges) {
      val fromN = solution(q.nodes(from))
      val toN = solution(q.nodes(to))
      val ge = g.edges((fromN.id, toN.id))
      gr = gr.updated(fromN,toN,ge)
      edgeMap.update(e,ge)
    }

    for((index, n) <- q.nodes) {
      val x = solution(n)
      if(!gr.nodes.contains(x.id)) {
        gr = gr.addNode(x)
      }
    }
    (gr, solution, edgeMap.toMap)
  }
}

object GraphMatch {

  def mtch[N,E](g:DGraph[N,E], q:DGraph[NodeMatchLike[N] , EdgeMatchLike[E]]) = {
    var finalSolutions = Queue.empty[Branch[N]]
    val qRoots = q.roots

    for(r <- qRoots) {
      val viableNodes = g.nodes.values.toList
      var localFinalSolutions = Queue.empty[Branch[N]]
      for (n <- viableNodes) {
        var branches = Queue.empty[Branch[N]]
        val ss = Branch(Map.empty[Node[NodeMatchLike[N]], Node[N]], Map[Node[N], Node[NodeMatchLike[N]]](n -> r))

        if(finalSolutions.isEmpty)
          branches = branches.enqueue(Branch(Map.empty[Node[NodeMatchLike[N]], Node[N]], 
            Map[Node[N], Node[NodeMatchLike[N]]](n -> r)))
        else {
            finalSolutions.foreach(br =>
              branches = branches.enqueue(br.copy[N](goals = Map[Node[N], Node[NodeMatchLike[N]]](n -> r))))
        }

        while (!branches.isEmpty) {
          val (br, newQ) = branches.dequeue
          branches = newQ
          if (br.goals.size == 0) localFinalSolutions = localFinalSolutions.enqueue(br)

          else {
            val (gNode, qNode) = br.goals.head
            val br1 = br.copy(goals = br.goals.tail)
            if (br1.solution.contains(qNode) && br1.solution(qNode) == gNode)
              branches = branches.enqueue(br1)
            else if (!br1.solution.contains(qNode) && !br1.solution.values.toList.contains(gNode)) {
              val br2 = br1.copy(solution = br1.solution.updated(qNode, gNode))

              val goalChilds = q.childs(qNode.id).zip(q.childsEdges(qNode.id))
              val perms = qNode.value match {
                case NodeMatchAND(_) => g.childs(gNode.id).zip(g.childsEdges(gNode.id)).toList.combinations(goalChilds.size).toList
                case NodeMatchOR(_) => g.childs(gNode.id).zip(g.childsEdges(gNode.id)).toList.map(List(_))
              }
              val tupList = qNode.value match {
                case NodeMatchAND(_) => perms.map(x => x.zip(goalChilds))
                case NodeMatchOR(_) => perms.flatMap(x => goalChilds.map(y => x.zip(List(y))))
              }

              if ((tupList.isEmpty || tupList.head == Nil) && g.childs(gNode.id).size >= q.childs(qNode.id).size)
                branches = branches.enqueue(br2)
              else {
                val newBranches = childsMatch(tupList, br2)
                newBranches.flatten.foreach(b => branches = branches.enqueue(b))
              }
            }
          }
        }
      }
      finalSolutions = localFinalSolutions
    }

    finalSolutions.toList
    //println(finalSolutions.size)
  }



  private def childsMatch[N,E](tupList:List[List[((Node[N], DEdge[E]), (Node[NodeMatchLike[N]], DEdge[EdgeMatchLike[E]]))]], br:Branch[N]) = {
    for(tup <- tupList) yield {
      if(tup.forall(x => singleMatch(x._1._1,x._2._1,x._1._2,x._2._2,br))) {
        val newGoals = br.goals ++ tup.filter(xy => !br.goals.contains(xy._1._1)).map(xy => xy._1._1 -> xy._2._1)
        Some(br.copy(goals = newGoals))
      } else None
    }
  }

  private def singleMatch[N,E](b:Node[N], B:Node[NodeMatchLike[N]], e:DEdge[E], E:DEdge[EdgeMatchLike[E]],br:Branch[N]) = {
    if(B.value.eval(b.value) &&
      E.value.eval(e.value)) true
    else false
  }


}




