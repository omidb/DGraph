package dgraph

import utest._

import scala.collection.immutable.TreeMap

object DGraphCoreTest extends TestSuite {


  override def tests = TestSuite {
    'createInSimpleForm {
      val nodes = Map(0 -> Node("n0", 0), 1 -> Node("n1", 1), 2 -> Node("n2", 2))
      val edges = TreeMap((0,1) -> DEdge("e0",0,1), (1,0) -> DEdge("e1",1,0), (1,2) -> DEdge("e2",1,2))
      val g = DGraph.from(nodes,edges)
      assert(g.inMap == Map(0 -> IndexedSeq(1), 1 -> IndexedSeq(0), 2 -> IndexedSeq(1)))
      assert(g.outMap == Map(0 -> IndexedSeq(1), 1 -> IndexedSeq(0, 2), 2 -> IndexedSeq()))
    }

    'createIn {
      import DGraphDSL._
      val g = DGraph.from[String,String](
        Nd("n0",
         --("e0")->Nd("n1"),
         --("e1")->Nd("n2"),
         --("e2")->Nd("n3"))
      )

      val g2 = DGraph.from(QNode("n0", HalfEdge("e0", QNode("n1", EmptyHalfEdge))))



      assert(g.nodes(0) == Node("n0", 0))
      assert(g.nodes(1) == Node("n1", 1))
      assert(g.nodes(2) == Node("n2", 2))
      assert(g.nodes(3) == Node("n3", 3))

      assert(g.edges((0,1)) == DEdge("e0", 0, 1))
      assert(g.edges((0,2)) == DEdge("e1", 0, 2))
      assert(g.edges((0,3)) == DEdge("e2", 0, 3))

    }

    'create2 {
      import DGraphDSL._
      val g = DGraph.from[String,String](
        Nd("n0",
         --("e0")->NdMark("n1","mrk1"),
         --("e1")->Nd("n2"),
         --("e2")->Nd("n3",
                    --("e3")->(Ref("mrk1"))))
      )
      assert(g.nodes(1) == Node("n0", 1))
      assert(g.nodes(0) == Node("n1", 0))
      assert(g.nodes(2) == Node("n2", 2))
      assert(g.nodes(3) == Node("n3", 3))

      assert(g.edges((1,0)) == DEdge("e0", 1, 0))
      assert(g.edges((1,2)) == DEdge("e1", 1, 2))
      assert(g.edges((1,3)) == DEdge("e2", 1, 3))
      assert(g.edges((3,0)) == DEdge("e3", 3, 0))
    }

//    'createMatcher {
//      import DGraphDSL._
//
//      val q1 = query[Int,String](
//        <&(x => x == 10,
//            -?>(f => f == "dood", <&(_ > 10))))
//    }

    'matching {
      import DGraphDSL._
      val g = DGraph.from[String,String](
        Nd("n0",
         --("e0")->NdMark("n1","mrk1"),
         --("e1")->Nd("n2"),
         --("e2")->Nd("n3",
                    --("e3")->(Ref("mrk1"))))
      )

      val q = query[String,String](
        <&(_ == "n0",
            -?>(_ == "e0", <&(_ == "n1"))
        )
      )

      val q1 = query[String,String](
        <&(t => false,
          -?>(_ == "e0", <&(_ == "n1"))
        )
      )

      val res = g.filter(q)

      val res2 = g filter q1

      assert(res2.size == 0)
    }

    'traverseDFS {
      import DGraphDSL._
      val g = DGraph.from[String,String](
        Nd("n0",
          --("e0")->NdMark("n1","mrk1"),
          --("e1")->Nd("n2",
            --("e3")->Nd("n4",
              --("e5")->Ref("mrk1"))),
          --("e2")->Nd("n3"))
      )

      val gp = g.mapDFS(g.roots.head, x => x, y => y)
      assert(g== gp)

      var counter = 0
      val gp1 = g.mapDFS(g.roots.head, x => {counter +=1; (x,counter)}, y => y)
      val nds = gp1.nodes.map(_._2).toList
      val n0 = nds.filter(_.value._1 == "n0").head
      val n1 = nds.filter(_.value._1 == "n1").head
      val n2 = nds.filter(_.value._1 == "n2").head
      val n3 = nds.filter(_.value._1 == "n3").head
      val n4 = nds.filter(_.value._1 == "n4").head
      assert(n0.value._2 == 1)
      assert(n1.value._2 == 2)
      assert(n2.value._2 == 3)
      assert(n4.value._2 == 4)
      assert(n3.value._2 == 5)

    }

    'traverseBFS {
      import DGraphDSL._
      val g = DGraph.from[String,String](
        Nd("n0",
          --("e0")->NdMark("n1","mrk1"),
          --("e1")->Nd("n2",
            --("e3")->Nd("n4",
              --("e5")->Ref("mrk1"))),
          --("e2")->Nd("n3"))
      )

      val gp = g.mapBFS(g.roots.head, x => x, y => y)
      assert(g== gp)

      var counter = 0
      println(counter)
      val gp1 = g.mapBFS(g.roots.head, x => {counter +=1; println((x,counter)); (x,counter)}, y => y)
      val nds = gp1.nodes.map(_._2).toList
      val n0 = nds.filter(_.value._1 == "n0").head
      val n1 = nds.filter(_.value._1 == "n1").head
      val n2 = nds.filter(_.value._1 == "n2").head
      val n3 = nds.filter(_.value._1 == "n3").head
      val n4 = nds.filter(_.value._1 == "n4").head
      assert(n0.value._2 == 1)
      assert(n1.value._2 == 2)
      assert(n2.value._2 == 3)
      assert(n3.value._2 == 4)
      assert(n4.value._2 == 5)

    }

    'fold {
      import DGraphDSL._
      val g = DGraph.from[String,String](
        Nd("n0",
          --("e0")->NdMark("n1","mrk1"),
          --("e1")->Nd("n2",
            --("e3")->Nd("n4",
              --("e5")->Ref("mrk1"))),
          --("e2")->Nd("n3"))
      )

      val concatDFS = g.foldDFS(g.roots.head,"","")((x,y) => x + y, (x,y) => x + y)
      val concatBFS = g.foldBFS(g.roots.head,"","")((x,y) => x + y, (x,y) => x + y)
      assert(concatDFS._1 == "n0n1n2n4n3")
      assert(concatDFS._1 == g.foldNodesDFS(g.roots.head,"")((x,y) => x + y))

      assert(concatBFS._1 == "n0n1n2n3n4")
      assert(concatBFS._1 == g.foldNodesBFS(g.roots.head,"")((x,y) => x + y))

    }
  }
}