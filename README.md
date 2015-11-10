# DGraph
An implementation of directed graph with powerful graph matching property

**Anything could change in next releases** 
A graph has two main parts: `Node` and `DEdge`. `Node[N]` contains `id:Int` and a value of type `N` .`DEdge[E]` has `from:Int` and `to:Int` and value of type `E`.
 
Using:
```scala
 resolvers += Resolver.sonatypeRepo("snapshots")

 libraryDependencies += "com.github.omidb" %%% "dgraph" % "0.1.0-SNAPSHOT"
```

##Create Graph
 
 There are two main ways to create graphs:
 
 ###Using Nodes and Edges
 
```scala
val nodes = Map(0 -> Node("n0", 0), 1 -> Node("n1", 1), 2 -> Node("n2", 2))
val edges = TreeMap((0,1) -> DEdge("e0",0,1), (1,0) -> DEdge("e1",1,0), (1,2) -> DEdge("e2",1,2))
val g:Dgraph[String,String] = DGraph.from(nodes,edges)
```
###Using a recursive representation
Using `QNodeLike` and `HalfEdgeLike` you can represent different recursive graph but it will convert to `Dgraph`.
Following all are `QNodeLike` :
`QNode` is a simple node
`QNodeMarker` is a node with a mark for future refrences
`QNodeRef` is a referense to a marker node
Following all are `HalfEdgeLike` :
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
This library provides algorithmes for graph matching. A pattern for matching on graphs is a graph of `DGraph[NodeMatchLike[N],EdgeMatchLike[E]]` which both `NodeMatchLike[N]` and `EdgeMatchLike[E]` contains an eval function of `T=> Boolean`
Following all are `NodeMatchLike[N]`:
`NodeMatchAND[N]` all the output edge-nodes should be true  
`NodeMatchANDCons[N]` all the output edge-nodes should be true (order is important)
`NodeMatchOR[N]` one of the output edge-nodes is enough to be true

We can use our DSL to define the pattern graph:
```scala
val q = query[String,String](
        <&(_ == "n0",
            -?>(_ == "e0", <&(_ == "n1"))
        )
      )
```

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
we can perform many operation on it like Scala colections:
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

