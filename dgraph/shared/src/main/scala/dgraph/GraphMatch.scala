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
  val optional:Boolean = false
  def eval:(E => Boolean)
}


case class EdgeMatch[E](eval:(E => Boolean), override val optional:Boolean = false)
  extends EdgeMatchLike[E]
case class EdgeMatchStar[E](eval:(E => Boolean)) extends EdgeMatchLike[E]

case class Pentree(x:String)
case class DepPath(y:String)


object tmp extends App{

}

case class Branch[N](id:Int, solution:Map[Node[NodeMatchLike[N]], Node[N]],
                       goals:Map[Node[N], Node[NodeMatchLike[N]]]) {

  def getMatchedGraph[E](g:DGraph[N,E], q:DGraph[NodeMatchLike[N] , EdgeMatchLike[E]])= {
    var gr = DGraph.empty[N,E]()
    val edgeMap = collection.mutable.Map.empty[DEdge[EdgeMatchLike[E]], DEdge[E]]
    for(((from, to), e) <- q.edges) {
      if(solution.contains(q.nodes(from)) && solution.contains(q.nodes(to))) {
        val fromN = solution(q.nodes(from))
        val toN = solution(q.nodes(to))
        val ge = g.edges((fromN.id, toN.id))
        gr = gr.updated(fromN, toN, ge)
        edgeMap.update(e, ge)
      }
    }

    for((index, n) <- q.nodes) {
      if(solution.contains(n)) {
        val x = solution(n)
        if (!gr.nodes.contains(x.id)) {
          gr = gr.addNode(x)._2
        }
      }
    }
    (gr, solution, edgeMap.toMap)
  }
}

object GraphMatch {


  def mtch[N,E](g:DGraph[N,E], q:DGraph[NodeMatchLike[N] , EdgeMatchLike[E]]) = {
    var finalSolutions = Queue.empty[Branch[N]]
    var branchParent = DGraph.empty[Int, Int]()
    var bId = -1
    val qRoots = q.roots

    for(r <- qRoots) {
      val viableNodes = g.nodes.values.toList.filter(x => r.value.eval(x.value))
      var localFinalSolutions = Queue.empty[Branch[N]]
      for (n <- viableNodes) {
        var branches = Queue.empty[Branch[N]]
        //val ss = Branch(Map.empty[Node[NodeMatchLike[N]], Node[N]], Map[Node[N], Node[NodeMatchLike[N]]](n -> r))

        if(finalSolutions.isEmpty) {
          bId += 1
          branchParent = branchParent.addNode(bId)._2
          branches = branches.enqueue(
            Branch(bId, Map.empty[Node[NodeMatchLike[N]], Node[N]], Map[Node[N], Node[NodeMatchLike[N]]](n -> r)))

        }
        else {
          finalSolutions.foreach(br =>
            branches = branches.enqueue(br.copy[N](goals = Map[Node[N], Node[NodeMatchLike[N]]](n -> r))))
        }

        while (branches.nonEmpty) {
          val (br, newQ) = branches.dequeue
          branches = newQ
          if (br.goals.isEmpty)
            localFinalSolutions = localFinalSolutions.enqueue(br)

          else {

            val (gNode, qNode) = br.goals.head
            val br1 = br.copy(goals = br.goals.tail)
            if (br1.solution.contains(qNode) && br1.solution(qNode) == gNode)
              branches = branches.enqueue(br1)
            else if (!br1.solution.contains(qNode) && !br1.solution.values.toList.contains(gNode)) {
              val br2 = br1.copy(solution = br1.solution.updated(qNode, gNode))
              val goalChilds1 = q.childs(qNode.id).zip(q.childsEdges(qNode.id))
              //
              val (optionals, requierd) = goalChilds1.partition(_._2.value.optional)
              val optionalPerm = (1 to optionals.size).flatMap(f => optionals.combinations(f).toList)
              val newSetGolaChilds = if(requierd.nonEmpty) optionalPerm.map(op => requierd ++ op) :+ requierd else optionalPerm.map(op => requierd ++ op)
              //
              val tupList = qNode.value match {
                case NodeMatchAND(_) =>
                  newSetGolaChilds.toList.flatMap(goalChilds => {
                    g.childs(gNode.id).zip(g.childsEdges(gNode.id)).toList
                      .combinations(goalChilds.size).toList
                      .flatMap(comb => comb.permutations.toList)
                      .map(lst => lst.zip(goalChilds))
                  })
                case NodeMatchOR(_) => //this probably won't work with optional and could be 100% wrong
                  g.childs(gNode.id).zip(g.childsEdges(gNode.id)).toList.map(List(_))
                    .flatMap(x => goalChilds1.map(y => x.zip(List(y))))
              }

              if ((tupList.isEmpty || tupList.head == Nil) && g.childs(gNode.id).size >= q.childs(qNode.id).size)
                branches = branches.enqueue(br2)
              else {
                val (newBranches, nbid, nbranchParent) = childsMatch(tupList, br2, bId, branchParent)
                bId = nbid
                branchParent = nbranchParent
                newBranches.flatten.foreach(b => branches = branches.enqueue(b))
                if(requierd.isEmpty)
                  branches = branches.enqueue(br2)

              }
            }
          }
        }
      }
      finalSolutions = localFinalSolutions
    }

    val solutionMaps = finalSolutions.toList.map(b => b.id -> b.solution.map(s => s._1.id -> s._2.id).toSet).toMap
    val viableBranches = solutionMaps.filterNot{case (bid, sm) => solutionMaps.filterNot(_._1 == bid).values.exists(sm2 => sm.subsetOf(sm2))}.keySet
    finalSolutions.toList.filter(b => viableBranches.contains(b.id))

  }
  private def childsMatch[N,E](tupList:List[List[((Node[N], DEdge[E]), (Node[NodeMatchLike[N]], DEdge[EdgeMatchLike[E]]))]],
                               br:Branch[N], currentBId:Int, branchParent:DGraph[Int, Int]) = {
    var bId = currentBId
    var brp = branchParent
    val brs = for (tup <- tupList) yield {
      if (tup.forall(x => singleMatch(x._1._1, x._2._1, x._1._2, x._2._2)) &&
        //redunduncy check
        tup.forall(x => (!br.goals.contains(x._1._1)) || (br.goals.contains(x._1._1) && br.goals(x._1._1) == x._2._1))) {
        val newGoals = br.goals ++ tup.filter(xy => !br.goals.contains(xy._1._1)).map(xy => xy._1._1 -> xy._2._1)
        bId += 1
        brp = brp.addNode(bId, 0, brp.nodes.find(_._2.value == br.id).get._1)._2
        Some(br.copy(goals = newGoals, id = bId))


      } else None
    }
    (brs, bId, brp.copy())
  }

  private def singleMatch[N,E](b:Node[N], B:Node[NodeMatchLike[N]], e:DEdge[E], E:DEdge[EdgeMatchLike[E]]) = {
    if(B.value.eval(b.value) &&
      E.value.eval(e.value)) true
    else false
  }
}




