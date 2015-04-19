/**
 * Représente le résultat d'une coloration de graphe
 *
 * @author Quentin Baert
 */
class ColorationResult(val graph: Graph, val verticiesColor: Map[Vertex, Color], val nbOfColors: Int) {

  override def toString: String =
    this.nbOfColors + " colors : " + "\n" + (this.verticiesColor mkString "\n")

}
