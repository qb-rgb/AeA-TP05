/**
 * Représente le résultat d'une coloration de graphe
 *
 * @author Quentin Baert
 */
class ColorationResult[T](val graph: Graph[T], val verticiesColor: Map[Vertex[T], Color], val nbOfColors: Int) {

  override def toString: String =
    this.nbOfColors + " colors : " + "\n" + (this.verticiesColor mkString "\n")

}
