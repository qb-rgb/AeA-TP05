/**
 * Affiche les moyennes de couleurs et de temps d'éxécution des algorithmes
 * de coloration implémentés
 *
 * @author Quentin Baert
 */
object TestPerfColo {

  // Éxécute une coloration en fonction d'une chaîne passée en paramètre
  private def executeColoration(graph: Graph, colorationName: String): (ColorationResult, Long) =
    colorationName match {
      case "wp" => {
        val start = System.currentTimeMillis
        (graph.getWelshPowellColoration, System.currentTimeMillis - start)
      }
      case "dsatur" => {
        val start = System.currentTimeMillis
        (graph.getDSATURColoration, System.currentTimeMillis - start)
      }
      case _ => {
        val start = System.currentTimeMillis
        (graph.getGreedyColoration, System.currentTimeMillis - start)
      }
    }

  // Génère x graphes aléatoirement
  private def generateXGraphs(x: Int, n: Int, proba: Float): List[Graph] =
    (
      for (i <- 1 to x)
        yield RandomGraphGenerator.generateErdosRenyiGraph(n, proba)
    ).toList

  /*
   * Affiche la moyenne du nombre de couleurs trouvés par un algo ainsi que son
   * temps d'éxécution
   */
  private def printAverage(graphs: List[Graph], title: String, id: String): Unit = {
    val l = graphs.size

    println("----- " + title + " -----")

    val resultList = for (g <- graphs) yield executeColoration(g, id)
    val (colorNb, time) =
      resultList.foldLeft((0 : Int, 0 : Long))((acc, c) => (acc._1 + c._1.nbOfColors, acc._2 + c._2))

    println("Nombre de couleurs en moyenne : " + colorNb / l)
    println("Temps moyen                   : " + time / l + " ms")
  }

  def main(args: Array[String]): Unit = {
    // Nombre de graphes sur lesquels faire les moyennes
    val graphNb = args(0).toInt
    // Nombre de sommets par graphe
    val graphVertices = args(1).toInt
    // Probabilité avec lesquelles seront générées les graphes
    val probas = List(0.1f, 0.3f, 0.5f, 0.7f, 0.9f)

    // Affichage des moyennes
    for (p <- probas) {
      val graphs = generateXGraphs(graphNb, graphVertices, p)
      println("===== PROBA : " + p + " =====")
      printAverage(graphs, "GREEDY", "greedy")
      printAverage(graphs, "WELSH-POWELL", "wp")
      printAverage(graphs, "DSATUR", "dsatur")
      println()
    }
  }

}
