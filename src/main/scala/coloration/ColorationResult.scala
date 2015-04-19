/**
 * Représente le résultat d'une coloration de graphe
 *
 * @constructor Construit un nouveau résultat de coloration
 * @param verticesColor association sommet -> couleur donnée par la coloration
 * @param nbOfColors nombre de couleurs différentes de la coloration
 *
 * @author Quentin Baert
 */
class ColorationResult(val verticiesColor: Map[Vertex, Color], val nbOfColors: Int) {

  override def toString: String =
    this.nbOfColors + " colors : " + "\n" + (this.verticiesColor mkString "\n")

}
