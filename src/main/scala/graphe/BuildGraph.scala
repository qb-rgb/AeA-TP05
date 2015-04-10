object BuildGraph {

  def main(args: Array[String]): Unit = {
    val graph = GraphBuilder.buildStringGraph("graph1000.txt")
    println(graph.edges.size)
    println("==========")
    println(graph.getKruskalMST.edges.size)
  }

}
