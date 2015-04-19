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
   * Donne les succésseurs d'un sommet du graphe
   *
   * @param vertex sommet dont on souhaite récupérer les succésseurs
   * @return ensemble des succéseurs de vertex
   */
  def getVertexNeighbours(vertex: Vertex[T]): Set[Vertex[T]] =
    (this getVertexEdges vertex) map (e => e other vertex)

  /**
   * Donne le degrès d'un sommet
   */
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

      val (vertexColor, newColors) =
        // Si aucune couleur n'est accessible
        if (accessibleColors.isEmpty) {
          // Une nouvelle couleur est crée et ajoutée aux couleurs disponibles
          val newColor = colors.head.next
          (newColor, newColor :: colors)
        } else
          // Sinon la plus petite couleur est choisie
          (accessibleColors.last, colors)

      this.baseColoration(
        coloredVertices + (vertex -> vertexColor),
        newColors,
        vertices.tail
      )
    }

  /**
   * Donne une coloration du graphe avec un algorithme greedy
   *
   * @return coloration du graphe
   */
  def getGreedyColoration: ColorationResult[T] =
    this.baseColoration(
      Map[Vertex[T], Color](),
      List(Color(1)),
      this.vertices.toList
    )

  /**
   * Donne une coloration du graphe avec l'algorithme de Welsh-Powel
   *
   * @return coloration du graphe
   */
  def getWelshPowerColoration: ColorationResult[T] = {
    // Les sommets du graphe sont triés dans l'ordre décroissant de leur degrès
    val orderedVertices = this.vertices.toList sortWith (
      (v1, v2) => this.getVertexDegree(v1) > this.getVertexDegree(v2)
    )

    this.baseColoration(
      Map[Vertex[T], Color](),
      List(Color(1)),
      orderedVertices
    )
  }

  /**
   * Donne une coloration du graphe avec l'algorithme DSATUR
   *
   * @return coloration du graphe
   */
  def getDSATURColoration: ColorationResult[T] = {
    // Donne le score DSAT d'un sommet
    def vertexDSAT(vertex: Vertex[T], coloredVertices: Map[Vertex[T], Color]) = {
      val neighbours = this getVertexNeighbours vertex
      val coloredNeighbours = neighbours filter coloredVertices.contains

      // Si le sommet ne possède pas de voisins colorés
      if (coloredNeighbours.isEmpty)
        // Son degrès est retourné
        this getVertexDegree vertex
      else
        /*
         * Sinon c'est le nombre de couleurs différentes utilisées dans son
         * voisinnage
         */
        coloredVertices.
        filter(c => coloredNeighbours contains c._1).
        map(c => c._2).
        toList.
        distinct.
        size
    }

    // Fonction d'ordre utilisé dans l'algorithme
    def orderFunction(
      v1: Vertex[T],
      v2: Vertex[T],
      coloredVertices: Map[Vertex[T], Color]
    ): Boolean = {
      val dsat1 = vertexDSAT(v1, coloredVertices)
      val dsat2 = vertexDSAT(v2, coloredVertices)

      // Si le score DSAT des deux sommets est différent
      if (dsat1 != dsat2)
        // Le sommet à placé en premier est celui qui a le score le plus élevé
        dsat1 > dsat2
      else
        /*
         * Si le score est le même, c'est le sommet de plus haut degrès qui est
         * placé en premier
         */
        (this getVertexDegree v1) > (this getVertexDegree v2)
    }

    // Ordonne une liste selon la fonction d'ordre
    def orderList(l: List[Vertex[T]], map: Map[Vertex[T], Color]): List[Vertex[T]] =
      l sortWith ((v1, v2) => orderFunction(v1, v2, map))

    /*
     * coloredVertices : sommets du graphe déjà colorés
     * colors          : couleurs pour l'instant utilisées dans le graphe
     * vertices        : sommets du graphe qu'il reste à colorer
     */
    def dsaturColoration(
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

        val (vertexColor, newColors) =
          // Si aucune couleur n'est accessible
          if (accessibleColors.isEmpty) {
            // Une nouvelle couleur est crée et ajoutée aux couleurs disponibles
            val newColor = colors.head.next
            (newColor, newColor :: colors)
          } else
            // Sinon la plus petite couleur est choisie
            (accessibleColors.last, colors)

        // Map mise à jour
        val newMap = coloredVertices + (vertex -> vertexColor)
        // Liste nouvellement ordonnée
        val newList = orderList(vertices.tail, newMap)

        dsaturColoration(newMap, newColors, newList)
      }

    // Sommets du graphe ordonnés selon leur degrès
    val orderedVertices = this.vertices.toList sortWith (
      (v1, v2) => (this getVertexDegree v1) > (this getVertexDegree v2)
    )
    // Première couleur
    val firstColor = Color(1)
    // La première couleur est assigné au sommet de plus haut degrès
    val firstMap = Map(orderedVertices.head -> firstColor)

    // Appel initial
    dsaturColoration(
      firstMap,
      List(firstColor),
      orderList(orderedVertices.tail, firstMap)
    )
  }

}
