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
