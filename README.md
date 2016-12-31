# DGraph
An implementation of directed graph with powerful graph matching functionality.

**Anything could change in the next releases.**

**This library is experimental and NOT production-ready by any means.**

In this library, a graph has two main components: `Node` and `DEdge`. `Node[N]` contains `id:Int` and a value of type `N` .`DEdge[E]` has `from:Int` and `to:Int` and value of type `E`.
 
Using:
```scala
 resolvers += Resolver.sonatypeRepo("snapshots")

 libraryDependencies += "com.github.omidb" %%% "dgraph" % "0.1.0-SNAPSHOT"
```

##Create Graph
 
 There are two main ways you can create a graph:
 
 **Using Nodes and Edges**
 
```scala
val nodes = Map(0 -> Node("n0", 0), 1 -> Node("n1", 1), 2 -> Node("n2", 2))
val edges = TreeMap((0,1) -> DEdge("e0",0,1), (1,0) -> DEdge("e1",1,0), (1,2) -> DEdge("e2",1,2))
val g:Dgraph[String,String] = DGraph.from(nodes,edges)
```
###Using a recursive representation
Using `QNodeLike` and `HalfEdgeLike` you can represent different recursive graphs, but it will get converted to `Dgraph`.
Following all are `QNodeLike` :
`QNode` is a simple node
`QNodeMarker` is a node with a mark for future references
`QNodeRef` is a references to a marker node
Following are all `HalfEdgeLike` :
`HalfEdge` is an edge to a node
`EmprtHalfEdge` is an empty node and edge

Here is an example:
```scala
val g:Dgraph[String,String] = DGraph.from(QNode("n0", HalfEdge("e0", QNode("n1", EmptyHalfEdge))))
```
For all these we provide a DSL to make things easier:
```scala
import DGraphDSL._
val g = DGraph.from[String,String](
  Nd("n0",
   --("e0")->Nd("n1"),
   --("e1")->Nd("n2"),
   --("e2")->Nd("n3"))
)

val g2 = DGraph.from[String,String](
  Nd("n0",
   --("e0")->NdMark("n1","mrk1"),
   --("e1")->Nd("n2"),
   --("e2")->Nd("n3",
              --("e3")->(Ref("mrk1"))))
)
```
##Graph Pattern
This library provides algorithms for graph matching. A pattern for matching on graphs is a graph of `DGraph[NodeMatchLike[N],EdgeMatchLike[E]]` which both `NodeMatchLike[N]` and `EdgeMatchLike[E]` contains an eval function of `T=> Boolean`
The following are all `NodeMatchLike[N]`:

`NodeMatchAND[N]` all the output edge-nodes should be true  `<&()`
`NodeMatchANDCons[N]` all the output edge-nodes should be true (order is important) `<&()`
`NodeMatchOR[N]` one of the output edge-nodes is enough to be true `<|()`

We can use our DSL to define the pattern graph:
```scala
val q = query[String,String](
        <&(_ == "n0",
            -?>(_ == "e0", <&(_ == "n1"))
        )
      )
```
 `-?>` is a simple `EdgeMatch` ...
##Graph Operations:
let's assume we have a graph:
```scala
import DGraphDSL._
      val g = DGraph.from[String,String](
        Nd("n0",
         --("e0")->NdMark("n1","mrk1"),
         --("e1")->Nd("n2"),
         --("e2")->Nd("n3",
                    --("e3")->(Ref("mrk1"))))
      )
```
and a pattern:
```scala
val q = query[String,String](
        <&(_ == "n0",
            -?>(_ == "e0", <&(_ == "n1"))
        )
      )
```
We can perform many operations on it like Scala collections:
```scala
 g.containsNode("n0") //check if it contains a node
 g.containsEdge("e0") //check if it contains an edge
   
 g.contains(q) //check if it contains a subgraph that matches the q query
 
 g.mapByNodes(_.toInt) //convert the graph by mapping nodes
 g.mapByEdges(e => e.toInt + 1) //convert the graph by mapping edges
 g.map(_.toInt, _.toInt + 1) //convert the graph by two different map functions


 g.filterNodes(_.startWith("n0")) //filtering nodes
 g.filterEdges(_.startWith("e0")) //filtering edges
 g.filter(q) // return List[DGraph[String,String]] of sub-graphs that match the query
```
All the functions above use `GraphMatch.mtch(g, q)` that returns all possible matches between graph `g` and query `q`.

If you want to extract some information from graph and convert it to a record (case class) you can do the following:

Let's say you have the following graph and you want to extract some specific items from it.
```scala
 import DGraphDSL._

      case class SimpleExtract(n0:String, n3:String, e3:String)

      val g = DGraph.from[String,String](
        Nd("n0",
          --("e0")->Nd("n1"),
          --("e1")->Nd("n2"),
          --("e2")->Nd("n3",
            --("e3")->Nd("n4")))
      )
```

We can use `queryAndExtract` that will give us two graphs: A query graph and an Extractor graph

```scala

      val res = queryAndExtract[String, String, SimpleExtract](
        <-&(n => n == "n0", (n, p) => p.copy(n0 = n),
          --?>(_ == "e2", (e,p) => p,
            <-&(_ == "n3", (n, p) => p.copy(n3 = n),
              --?>(_ == "e3", (e,p) => p.copy(e3 = e), <-&(_ == "n4",(n,p) => p))))
        ),
        g, SimpleExtract("", "", "")
      )


      
      println(res.head._2)
      assert(res.head._2 == SimpleExtract("n0", "n3", "e3"))
```
As you can see, it will extract the information and updates the `SimpleExtract("", "", "")`.