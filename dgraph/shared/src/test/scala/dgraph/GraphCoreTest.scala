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

    'createIn {
      import DGraphDSL._
      val g = DGraph.from[String,String](
        Nd("n0",
          --("e0")->Nd("n1"),
          --("e1")->Nd("n2"),
          --("e2")->Nd("n3"))
      )
      val n3 = g.filterNodes(_ == "n3").head
      val g2 = g.addNode("n4","e3",n3.id)._2

      assert(g2.nodes(0) == Node("n0", 0))
      assert(g2.nodes(1) == Node("n1", 1))
      assert(g2.nodes(2) == Node("n2", 2))
      assert(g2.nodes(3) == Node("n3", 3))
      assert(g2.nodes(4) == Node("n4", 4))

      assert(g2.edges((0,1)) == DEdge("e0", 0, 1))
      assert(g2.edges((0,2)) == DEdge("e1", 0, 2))
      assert(g2.edges((0,3)) == DEdge("e2", 0, 3))
      assert(g2.edges((3,4)) == DEdge("e3", 3, 4))

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

      val g2 = DGraph.from[String,String](
        Nd("n0",
          --("e0")->Nd("n1"),
          --("e1")->Nd("n2"),
          --("e2")->Nd("n3",
            --("e3")->Nd("n4")))
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

      val q2 = query[String,String](
        <&(n => n == "n0",
          -?>(_ == "e2",
            <&(_ == "n3", -?>(_ == "e3", <&(_ == "n4"))))
        )
      )

      val res = g.filter(q)

      val res2 = g filter q1
      println(g2.filter(q2))
      assert(g2.filter(q2).nonEmpty)

//      assert(res2.size == 0)
    }

    'matchingAndExtract {
      import DGraphDSL._

      case class SimpleExtract(n0:String, n3:String, e3:String)

      val g = DGraph.from[String,String](
        Nd("n0",
          --("e0")->Nd("n1"),
          --("e1")->Nd("n2"),
          --("e2")->Nd("n3",
            --("e3")->Nd("n4")))
      )

      val q2 = query2[String, String, SimpleExtract](
        <-&(n => n == "n0", (n, p) => p.copy(n0 = n),
          --?>(_ == "e2", (e,p) => p,
            <-&(_ == "n3", (n, p) => p.copy(n3 = n),
              --?>(_ == "e3", (e,p) => p.copy(e3 = e), <-&(_ == "n4",(n,p) => p))))
        )
      )


      val q3 = query2[String, String, SimpleExtract](
        <-&(n => n == "n0", (n, p) => p.copy(n0 = n),
          --?>(_ == "e2", (e,p) => p,
            <-&(_ == "n3", (n, p) => p.copy(n3 = n),
              --??>(_ == "e3", (e,p) => p.copy(e3 = e), <-&(_ == "n4",(n,p) => p)),
              --??>(_ == "e30", (e,p) => p.copy(e3 = e), <-&(_ == "n4",(n,p) => p))
            ))
        )
      )


      val (q,ed) = q2.unzip
      val res = extract(g, SimpleExtract("", "", ""), q, ed)

      val (q0,ed0) = q3.unzip
      val res0 = extract(g, SimpleExtract("", "", ""), q0, ed0)

      assert(res.head._2 == SimpleExtract("n0", "n3", "e3"))
      assert(res0.head._2 == SimpleExtract("n0", "n3", "e3"))
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

      val gp = g.mapDFS(g.roots.head, (x,i) => x, (y,f,t) => y)
      assert(g== gp)

      var counter = 0
      val gp1 = g.mapDFS(g.roots.head, (x,i) => {counter +=1; (x,counter)}, (y, f,t) => y)
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

      val gp = g.mapBFS(g.roots.head, (x, i) => x, (y, f, t) => y)
      assert(g== gp)

      var counter = 0
      println(counter)
      val gp1 = g.mapBFS(g.roots.head, (x,i) => {counter +=1; println((x,counter)); (x,counter)}, (y,f,t) => y)
      val nds = gp1.nodes.values.toList
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

      val concatDFS = g.foldDFS(g.roots.head,"","")((x,y,i) => x + y, (x,y,f,t) => x + y)
      val concatBFS = g.foldBFS(g.roots.head,"","")((x,y,i) => x + y, (x,y,f,t) => x + y)
      assert(concatDFS._1 == "n0n1n2n4n3")
      assert(concatDFS._1 == g.foldNodesDFS(g.roots.head,"")((x,y,i) => x + y))

      assert(concatBFS._1 == "n0n1n2n3n4")
      assert(concatBFS._1 == g.foldNodesBFS(g.roots.head,"")((x,y,i) => x + y))

    }
  }
}