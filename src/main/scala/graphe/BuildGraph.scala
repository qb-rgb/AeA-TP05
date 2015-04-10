object BuildGraph {

  def main(args: Array[String]): Unit = {
    val graph = GraphBuilder.buildIntGraph("graph1000.txt")
    println(graph.edges.size)
    println("==========")
    println(graph.getPrimMST.edges.size)
  }

}
