/**
 * Représente un graphe valué
 *
 * @constructor construit un nouveau graphe valué
 * @param vertices noeuds du graphe
 * @param edges arêtes du graphe
 *
 * @author Quentin Baert
 */
class Graph[T](val vertices: Set[Vertex[T]], val edges: Set[Edge[T]]) {

  ///////////////
  // ATTRIBUTS //
  ///////////////

  /**
   * Permet d'accéder aux noeuds du graphe par leur identifiant
   */
  val verticesId: Map[T, Vertex[T]] =
    (this.vertices map (x => x.id -> x)).toMap

  //////////////
  // MÉTHODES //
  //////////////

  /**
   * Donne toutes les arêtes du graphe reliées à un sommet donné
   *
   * @param vertex noeud dont on souhaite récupérer les arêtes
   * @return arêtes du graphe reliées au sommet vertex
   */
  def getVertexEdges(vertex: Vertex[T]): Set[Edge[T]] =
    this.edges filter (e => (e.v1 == vertex) || (e.v2 == vertex))

  /**
   * Donne toutes les arêtes du graphe reliées à un sommet donné
   *
   * @param vertexId identifiant du noeud dont on souhaite récupérer les arêtes
   * @return arêtes du graphe reliées au sommet d'identifiant vertexId
   */
  def getVertexEdges(vertexId: T): Set[Edge[T]] =
    this getVertexEdges this.verticesId(vertexId)

  /**
   * Donne les succésseurs d'un sommet du graphe
   *
   * @param vertex sommet dont on souhaite récupérer les succésseurs
   * @return ensemble des succéseurs de vertex
   */
  def getVertexNeighbours(vertex: Vertex[T]): Set[Vertex[T]] =
    (this getVertexEdges vertex) map (e => e other vertex)

  def getVertexDegree(vertex: Vertex[T]): Int =
    (this getVertexEdges vertex).size

  /**
   * Détermine si le graphe est un graphe connexe ou non
   *
   * @return true si le graphe est connexe, false sinon
   */
  def isConnex: Boolean =
    !(this.vertices exists (v => (this getVertexEdges v).isEmpty))

  /**
   * Retourne un nouveau graphe avec un noeud supplémentaire
   *
   * @param vertex noeud à ajouter au graphe
   * @return nouveau graphe avec un noeud supplémentaire
   */
  def addVertex(vertex: Vertex[T]): Graph[T] =
    new Graph[T](this.vertices + vertex, this.edges)

  /**
   * Retourne un nouveau graphe avec une arête supplémentaire
   *
   * @param edge arête à ajouter au graphe
   * @return nouveau graphe avec une arête supplémentaire
   */
  def addEdge(edge: Edge[T]): Graph[T] =
    if ((this.vertices contains edge.v1) &&
        (this.vertices contains edge.v2))
      new Graph[T](this.vertices, this.edges + edge)
    else
      throw new Error("Graph.addEdge : Impossible d'ajouter l'arête")

  /**
   * Retourne un nouveau graphe avec une arête supplémentaire
   *
   * @param v1 première extrémitée de l'arête
   * @param v2 seconde extrémitée de l'arête
   * @param weight poids de l'arête
   * @return nouveau graphe avec une arête supplémentaire
   */
  def addEdgeBetween(v1: Vertex[T], v2: Vertex[T], weight: Int): Graph[T] =
    this addEdge (new Edge(v1, v2, weight))

  /**
   * Retourne un nouveau graphe avec une arête supplémentaire
   *
   * @param v1 première extrémitée de l'arête
   * @param v2 seconde extrémitée de l'arête
   * @param weight poids de l'arête
   * @return nouveau graphe avec une arête supplémentaire
   */
  def addEdgeBetween(v1Id: T, v2Id: T, weight: Int): Graph[T] = {
    val edge = new Edge(this.verticesId(v1Id), this.verticesId(v2Id), weight)
    this addEdge edge
  }

  override def toString: String =
    this.edges mkString "\n"

  ////////////////
  // COLORATION //
  ////////////////

  /*
   * coloredVertices : sommets du graphe déjà colorés
   * colors          : couleurs pour l'instant utilisées dans le graphe
   * vertices        : sommets du graphe qu'il reste à colorer
   */
  private def baseColoration(
    coloredVertices: Map[Vertex[T], Color],
    colors: List[Color],
    vertices: List[Vertex[T]]
  ): ColorationResult[T] =
    // Si tous les sommets ont été colorés, la coloration est retournée
    if (vertices.isEmpty)
      new ColorationResult(this, coloredVertices, colors.size)
    else {
      // Sommet à colorer
      val vertex = vertices.head
      // Voisins du sommet à colorer
      val neighbours = this getVertexNeighbours vertex
      // Couleurs des voisins du sommet à colorer
      val neighboursColors = neighbours map (n =>
        if (coloredVertices contains n) coloredVertices(n) else Color(-1)
      )
      // Couleurs avec lesquelles le sommet à colorer peut l'être
      val accessibleColors = colors filterNot neighboursColors.contains

      // Si aucune couleur n'est disponible
      if (accessibleColors.isEmpty) {
        // Il faut en créer une nouvelle pour colorer le sommet
        val newColor = colors.head.next

        this.baseColoration(
          coloredVertices + (vertex -> newColor),
          newColor :: colors,
          vertices.tail
        )
      } else {
        // Sinon le sommet est colorer avec la plus petite couleur
        val vertexColor = accessibleColors.last

        this.baseColoration(
          coloredVertices + (vertex -> vertexColor),
          colors,
          vertices.tail
        )
      }
    }

  /**
   * Donne une coloration du graphe avec un algorithme greedy
   *
   * @return coloration du graphe
   */
  def getGreedyColoration: ColorationResult[T] = {
    /*
     * coloredVertices : sommets du graphe déjà colorés
     * colors          : couleurs pour l'instant utilisées dans le graphe
     * vertices        : sommets du graphe qu'il reste à colorer
     */
    def greedyColoration(coloredVertices: Map[Vertex[T], Color], colors: List[Color], vertices: Set[Vertex[T]]): ColorationResult[T] =
      // Si tous les sommets ont été colorés, la coloration est retournée
      if (vertices.isEmpty)
        new ColorationResult(this, coloredVertices, colors.size)
      else {
        // Sommet à colorer
        val vertex = vertices.head
        // Voisins du sommet à colorer
        val neighbours = this getVertexNeighbours vertex
        // Couleurs des voisins du sommet à colorer
        val neighboursColors = neighbours map (n => if (coloredVertices contains n) coloredVertices(n) else Color(-1))
        // Couleurs avec lesquelles le sommet à colorer peut l'être
        val accessibleColors = colors filterNot neighboursColors.contains

        // Si aucune couleur n'est disponible
        if (accessibleColors.isEmpty) {
          // Il faut en créer une nouvelle pour colorer le sommet
          val newColor = colors.head.next

          greedyColoration(coloredVertices + (vertex -> newColor), newColor :: colors, vertices - vertex)
        } else {
          // Sinon le sommet est colorer avec la plus petite couleur
          val vertexColor = accessibleColors.last

          greedyColoration(coloredVertices + (vertex -> vertexColor), colors, vertices - vertex)
        }
      }

  }

}
