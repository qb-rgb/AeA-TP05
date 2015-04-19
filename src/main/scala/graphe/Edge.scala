/**
 * Représente une arête valuée d'un graphe
 *
 * @constructor construit une nouvelle arête
 * @param v1 première extrémité de l'arête
 * @param v2 seconde extrémité de l'arête
 * @param weight poids de l'arête
 *
 * @author Quentin Baert
 */
case class Edge(
  val v1: Vertex,
  val v2: Vertex,
  val weight: Int) {

  // Les deux extrémités d'une arête doivent être différentes
  assert(v1 != v2)

  /**
   * Donne une extrémité de l'arête à partir de l'autre extrémité
   *
   * @param v une des extrémité de l'arête
   * @return autre extrémité de l'arête
   */
  def other(v: Vertex): Vertex =
    if (v == this.v1)
      this.v2
    else if (v == this.v2)
      this.v1
    else
      throw new Error(
        "Edge.other : le sommet " + v + " n'appartient pas à l'arête " + this
      )

  override def toString: String =
    this.v1.toString + " -- " + this.v2.toString + " (" + this.weight + ")"

  override def equals(other: Any): Boolean = other match {
    case that: Edge => {
      val bool = (that canEqual this) && (this.weight == that.weight)

      if (this.v1 == that.v1)
        bool && (this.v2 == that.v2)
      else if (this.v1 == that.v2)
        bool && (this.v2 == that.v1)
      else
        false
    }
    case _ => false
  }

  def canEqual(other: Any) = other.isInstanceOf[Edge]

  override def hashCode: Int =
    (41 * (41 + weight)) + v1.hashCode + v2.hashCode

}

