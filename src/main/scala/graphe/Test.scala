object Test {

  def main(args: Array[String]): Unit = {
    val n = args(0).toInt
    val p = 0.8f

    val graph = RandomGraphGenerator.generateErdosRenyiGraph(n, p)

    println("===== GRAPHE =====")
    println(graph)

    println("===== PRIM MST =====")
    val prim = graph.getPrimMST
    println(prim)

    println("===== KRUSKAL MST =====")
    val kruskal = graph.getKruskalMST
    println(kruskal)
  }

}
