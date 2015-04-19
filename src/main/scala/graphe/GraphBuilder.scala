import scala.io.Source

import java.util.regex.Pattern
import java.util.regex.Matcher

/**
 * Construit un graphe depuis un fichier texte
 *
 * @author Quentin Baert
 */
object GraphBuilder {

  // Construit une liste d'arêtes depuis une ligne du fichier servant à générer le graphe
  private def listOfEdgesFromLine(line: String): Set[Edge] = {
    def findCouples(matcher: Matcher, index: Int, res: List[(String, Int)]): List[(String, Int)] =
      if (matcher.find) {
        val stringCouple = matcher group 1
        val couple = stringCouple split " "
        findCouples(matcher, index + 1, (couple(0), couple(1).toInt) :: res)
      } else res

    val splitPattern = Pattern compile "([\\d\\w]+)\\s+([[\\d\\w]+\\s+\\d+\\s*]+)"
    val splitMatcher = splitPattern matcher line

    if (splitMatcher.matches) {
      val vertex = Vertex(splitMatcher group 1)
      val vertexesAndWeight = splitMatcher group 2

      val vAndWPattern = Pattern compile "([\\d\\w]+\\s+\\d+)"
      val vAndWMatcher = vAndWPattern matcher vertexesAndWeight

      val couples = findCouples(vAndWMatcher, 1, Nil)

      (couples map (x => new Edge(vertex, Vertex(x._1), x._2))).toSet
    } else
      throw new Error("Erreur à la lecture du fichier")
  }

  // Construit un graphe de n'importe quel type
  private def buildTGraph(path: String): Graph = {
    // Récupération du fichier sous forme de chaine de caractèresj
    val text = (Source fromFile path).mkString
    // Liste de lignes
    val lines = (text split "\n").toList
    val edges = lines flatMap (l => this listOfEdgesFromLine l)
    val vertices = edges.foldLeft(Set[Vertex]())((a, e) => a + e.v1 + e.v2)

    new Graph(vertices, edges.toSet)
  }

  /**
   * Construit un graphe de type String à partir d'un fichier
   *
   * @param path chemin vers le fichier
   * @return graphe généré à partir du fichier
   */
  def buildGraph(path: String): Graph =
    this.buildTGraph(path)

}
