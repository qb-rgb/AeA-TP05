object Main {

  def main(args: Array[String]): Unit = {
    val vertexes = (for (id <- 'A' to 'H') yield Vertex(id)).toSet
    val vertexesId = (vertexes map (v => v.id -> v)).toMap
    val edges = Set(
      Edge(vertexesId('A'), vertexesId('B'), 6),
      Edge(vertexesId('A'), vertexesId('H'), 14),
      Edge(vertexesId('A'), vertexesId('C'), 5),
      Edge(vertexesId('A'), vertexesId('G'), 8),
      Edge(vertexesId('B'), vertexesId('C'), 12),
      Edge(vertexesId('C'), vertexesId('F'), 7),
      Edge(vertexesId('C'), vertexesId('D'), 9),
      Edge(vertexesId('E'), vertexesId('F'), 15),
      Edge(vertexesId('F'), vertexesId('G'), 10),
      Edge(vertexesId('G'), vertexesId('H'), 3)
    )

    val graphe = new Graph(vertexes, edges)

    println(graphe.toString)
    println(graphe.containsCycle)

    println("\n----- PRIM MST -----\n")

    val prim = graphe.getPrimMST
    println(prim)
    println(prim.containsCycle)

    println("\n----- KRUSKAL MST -----\n")

    val kruskal = graphe.getKruskalMST
    println(kruskal)
    println(kruskal.containsCycle)
  }

}
