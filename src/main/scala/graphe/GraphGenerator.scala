import scala.util.Random

/**
 * Permet de générer des graphes aléatoirement
 *
 * @author Quentin Baert
 */
object RandomGraphGenerator {

  /**
   * Génère un graphe aléatoirement selon le modèle d'Erdos-Renyi
   *
   * @param n nombre de sommets du graphe
   * @param p probabilité avec laquelle une arête va être crée entre deux sommets
   * @return graphe généré aléatoirement selon n et p
   */
  def generateErdosRenyiGraph(n: Int, p: Float): Graph[Int] = {
    val vertexes = (1 to n) map (n => Vertex(n))
    val N = scala.math.pow(n, 4).toInt
    val r = new Random
    val edges = for {
      v1 <- vertexes
      v2 <- vertexes
      if (v1.id < v2.id)
      if (r.nextFloat > p)
    } yield new Edge(v1, v2, r.nextInt(N))

    new Graph(vertexes.toSet, edges.toSet)
  }

}
