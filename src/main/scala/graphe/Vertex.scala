/**
 * Représente un noeud d'un graphe
 *
 * @constructor construit un nouveau noeud
 * @param id identifiant du noeud
 *
 * @author Quentin Baert
 */
case class Vertex[T](val id: T) {

  override def toString: String =
    this.id.toString

  override def equals(other: Any): Boolean = other match {
    case that: Vertex[_] => (that canEqual this) && (this.id == that.id)
    case _ => false
  }

  def canEqual(other: Any) = other.isInstanceOf[Vertex[_]]

  override def hashCode: Int = this.id.hashCode

}

