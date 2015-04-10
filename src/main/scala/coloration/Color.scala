/**
 * Représente une couleur lors de la coloration d'un graphe
 *
 * @author Quentin Baert
 */
case class Color(val id: Int) {

  //////////////
  // MÉTHODES //
  //////////////

  override def equals(other: Any): Boolean = other match {
    case that: Color => that.id == this.id
    case _ => false
  }

  def canEqual(other: Any) = other.isInstanceOf[Color]

  override def toString: String = "Color " + this.id

}
