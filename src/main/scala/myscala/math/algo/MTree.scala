package myscala.math.algo

class MTree[D <: PhysicalVector](distanceFuntion: (D, D) => Double) {

  var newRoot: NewRoot = new NewEmptyRoot
  /*var root: Leaf = new Leaf
  root.isRoot = true*/

  //def insert(id: String, pos: D): Unit = this.insertEntry(root, new DataEntry(id, pos))
  def insert(id: String, pos: D): Unit = this.newInsertEntry(newRoot, new NewDataEntry(id, pos))
  def insertAll(nodes: Iterable[(String, D)]): Unit = nodes.foreach(o => this.newInsertEntry(newRoot, new NewDataEntry(o._1, o._2)))



  //private def dMTree(a: DataEntry, b: DataEntry): Double = { distanceFuntion(a.position, b.position) }
  private def dMTree(a: NewDataEntry, b: NewDataEntry): Double = {
    distanceFuntion(a.position, b.position)
  }

  /*class DataEntry(val id: String, val position: D) {

    def distanceToThis(de: DataEntry): Double = {distanceFuntion(this.position, de.position)}

    var isLeaf: Boolean = true
    var distanceToParent: Option[Double] = None
    var coveringRadius: Option[Double] = None
    var subTree: Option[Leaf] = None

    def copyDataEntry(st: Option[Leaf]): DataEntry = {
      val newEntry: DataEntry = new DataEntry(this.id, this.position)
      newEntry.isLeaf = this.isLeaf
      newEntry.distanceToParent = this.distanceToParent
      newEntry.coveringRadius = this.coveringRadius
      newEntry.subTree = st
      newEntry
    }

    def leafToRouting(st: Leaf): Unit = {
      this.isLeaf = false
      this.subTree = Some(st)
      this.coveringRadius = Some(st.contents.map(distanceToThis).max)
    }

    /** Checks whether another object equals this one
      *
      * @param other another object to test equality for
      * @return boolean indicating if the two objects are the same
      */
    override def equals(other: Any): Boolean =
      other match {
        case that: DataEntry => that.canEqual(this) && this.id == that.id
        case _ => false
      }

    /** Checks whether we are allowed to compare this object to another
      *
      * @param other are we allowed to compare to this object
      * @return true if we can compare and flase otherwise
      */
    def canEqual(other: Any): Boolean = other.isInstanceOf[DataEntry]


    /** Definition of equality.
      *
      * @return Int representing the object
      */
    override def hashCode: Int = {
      this.id.##
    }

    override def toString: String = "id=" + id + ", pos=" + position// + ", isLeaf=" + isLeaf //+ ", subTree=" + subTree
  }*/

  val LEAF_SIZE: Int = 4


  final class NewDataEntry(val id: String, val position: D, private var _subTree: Option[NewNode], val distanceToParent: Double) {

    def this(id: String, pos: D) = this(id, pos, None, 0.0)

    var coveringRadius: Option[Double] = if (_subTree.nonEmpty) Some(_subTree.get.contents.map(distanceToThis).max) else Some(0.0)

    def subTree: NewNode = this._subTree.get

    def replaceSubTreeParentNode(parNode: NewNode): Unit = { if (this._subTree.nonEmpty) this._subTree.get.replaceParentNode(parNode)}

    def replaceSubTree(st: NewNode): Unit = {
      this._subTree = Some(st)
      this.coveringRadius = Some(st.contents.map(distanceToThis).max)
    }

    def distanceToThis(de: NewDataEntry): Double = {
      distanceFuntion(this.position, de.position)
    }

    //def copy(parNode: NewNode): NewDataEntry = new NewDataEntry(this.id, this.position, this.subTree, this.distanceToParent)
    def copy(st: Option[NewNode]): NewDataEntry = new NewDataEntry(this.id, this.position, st, this.distanceToParent)

    def copy: NewDataEntry = new NewDataEntry(this.id, this.position, this._subTree, this.distanceToParent)

    /** Checks whether another object equals this one
      *
      * @param other another object to test equality for
      * @return boolean indicating if the two objects are the same
      */
    override def equals(other: Any): Boolean =
      other match {
        case that: NewDataEntry => that.canEqual(this) && this.id == that.id
        case _ => false
      }

    /** Checks whether we are allowed to compare this object to another
      *
      * @param other are we allowed to compare to this object
      * @return true if we can compare and flase otherwise
      */
    def canEqual(other: Any): Boolean = other.isInstanceOf[NewDataEntry]


    /** Definition of equality.
      *
      * @return Int representing the object
      */
    override def hashCode: Int = {
      this.id.##
    }

    override def toString: String = "id=" + id + ", pos=" + position // + ", isLeaf=" + isLeaf //+ ", subTree=" + subTree
  }


  sealed class NewNode(protected var _contents: Vector[NewDataEntry], private var _parentNode: NewNode, val parentData: NewDataEntry) {
    this._contents.foreach(de => de.replaceSubTreeParentNode(this))

    def parentNode: NewNode = this._parentNode
    def replaceParentNode(st: NewNode): Unit = {this._parentNode = st}

    def contents: Vector[NewDataEntry] = this._contents

    def addContent(de: NewDataEntry): Unit = {
      this._contents = this._contents :+ de
    }

    def removeContent(de: NewDataEntry): Unit = {
      this._contents = this._contents.filterNot(_ == de)

    }

    def isFull: Boolean = this._contents.size >= LEAF_SIZE

    def copy(cont: Vector[NewDataEntry], parData: NewDataEntry): NewNode = new NewNode(cont, _parentNode, parData)

    //def copy(parNode: NewNode): NewNode = new NewNode(this._contents, Some(parNode), this.parentData)


    override def toString: String = {
      "NODE. contents size=" + this.contents.size + ", contents: " + this.contents.mkString(", ")
    }
  }

  final class NewLeaf(cont: Vector[NewDataEntry], parNode: NewNode, parData: NewDataEntry) extends NewNode(cont, parNode, parData) with LeafTrait {
    override def toString: String = {
      "LEAF. contents size=" + this.contents.size + ", contents: " + this.contents.mkString(", ")
    }
  }

  sealed class NewRoot(cont: Vector[NewDataEntry]) extends NewNode(cont, null, null) {

    def nodeify(parNode: NewNode): NewNode = new NewNode(this._contents, parNode, this.parentData)

    override def toString: String = {
      "ROOT. contents size=" + this.contents.size + ", contents: " + this.contents.mkString(", ")
    }
  }

  trait LeafTrait

  final class NewEmptyRoot extends NewRoot(Vector()) with LeafTrait {
    override def toString: String = {
      "LEAFROOT. contents size=" + this.contents.size + ", contents: " + this.contents.mkString(", ")
    }
  }

  /*

    sealed class Leaf {
      private var _contents: Vector[DataEntry] = Vector()
      var isRoot: Boolean = false
      var parentNode: Option[Leaf] = None
      var parentData: Option[DataEntry] = None

      def contents: Vector[DataEntry] = this._contents
      def addContent(de: DataEntry): Unit = {this._contents = this._contents :+ de}
      def removeContent(de: DataEntry): Unit = {
        println(de, this._contents, this._contents.size)
        this._contents = this._contents.filterNot(_ == de)
        println(this._contents.size)
      }

      def isFull: Boolean = this._contents.size >= LEAF_SIZE


      def replaceContents(newContents: Vector[DataEntry], newParentData: DataEntry): Unit = {
        this._contents = newContents
        this._contents.foreach(de => if (de.subTree.nonEmpty) de.subTree.get.parentNode=Some(this))
        this.parentData = Some(newParentData)
      }

      override def toString: String = {
        "LEAF. contents size=" + this.contents.size + ", contents: " + this.contents.mkString(", ")
      }
    }

    sealed class RoutingNode extends Leaf {
      override def toString: String = {
        "ROUTINGNODE. contents size=" + this.contents.size + ", contents: " + this.contents.mkString(", ")
      }
    }

    /*final class RootNode extends RoutingNode {
      override val isRoot: Boolean = true

      def asRoutingNode: RoutingNode
    }*/

    /** Insert function used in the M-tree.
      *
      * @param n node where to insert the object
      * @param data object to insert
      */
    private def insertEntry(n: Leaf, data: DataEntry): Unit = {
      n match {
        case r: RoutingNode => {
          // finds the index where the distance is minimum. Two cases can happen:
          // 1) if the distance is negative, it implies that the current node's covering radius is larger than the distance
          //    between the object to insert and the current node, hence no extension of the covering radius is needed.
          //    If multiple nodes can host the node closest to the data entry is selected.
          // 2) if the distance is positive, it means the covering radius needs to be increased. The node where the
          //    increase in covering radius is minimum  is selected.
          val idxOfChild = r.contents.map(node => {dMTree(node, data)-node.coveringRadius.get}).zipWithIndex.minBy(_._1)
          if (idxOfChild._1 > 0.0) r.contents(idxOfChild._2).coveringRadius = Some(r.contents(idxOfChild._2).coveringRadius.get - idxOfChild._1) // if convering radius is too small, then extend it.
          insertEntry(r.contents(idxOfChild._2).subTree.get, data) // insert into subtree
        }
        case l: Leaf => {
          if (!l.isFull) { l.addContent(data) }
          else { split(l, data) }
        }
      }
    }

    /** Split function used in the M-tree.
      *
      * @param n node to split
      * @param data data entry which is to be inserted
      */
    private def split(n: Leaf, data: DataEntry): Unit = {

      val N: Vector[DataEntry] = n.contents :+ data // groups all data entries to classify

      val (op, np) = (n.parentData, n.parentNode) // stores parent information which is used for promoting

      // two DataEntry object to be promoted as routing nodes
      val (op1, op2) = if (n.isRoot){ promote(N) } // if root, promote two nodes furthest away from each other
      else { promote(N, op.get) } // if not root, promote node furthest away from current parent DataEntry

      val (n1, n2) = partition(N, op1, op2) // allocate each data point to one of two new routing objects

      val nPrime = n match { // new node of same type as current node to split
        case b: RoutingNode => new RoutingNode
        case a: Leaf => new Leaf
      }

      n.replaceContents(n1, op1) // allocate the contents from partitioning to n
      op1.leafToRouting(n) // update data entry

      nPrime.replaceContents(n2, op2) // allocate the contents from partitioning to nPrime
      op2.leafToRouting(nPrime) // update data entry

      n.isRoot match {
        case true => {
          val Np = new RoutingNode // allocate new root node Np
          Np.isRoot = true // set new node as root
          n.isRoot = false // set old node as non root

          n.parentNode = Some(Np) // set parent of old node to root
          Np.addContent(op1) // add node as routing ndoe in root

          nPrime.parentNode = Some(Np) // set entry of node to root
          Np.addContent(op2) // add node as routing node in root

          this.root = Np // set new root as root
        }
        case false => {
          np.get.removeContent(op.get) // remove old routing node from contents
          np.get.addContent(op1) // add new node

          nPrime.parentNode = np // set parent of new node to parent of old node

          if (!np.get.isFull) { np.get.addContent(op2) } // if not full then add data entry to contents
          else { split(np.get, op2) } // if node is full, split it
        }
      }
    }

    private def promote(entries: Vector[DataEntry]): (DataEntry, DataEntry)  = {
      entries.combinations(2).maxBy(l => dMTree(l.head, l.tail.head)) match { case Vector(a,b) => (a.copyDataEntry(None),b.copyDataEntry(None)) }
    }

    private def promote(entries: Vector[DataEntry], currentNode: DataEntry): (DataEntry, DataEntry)  = {
      (currentNode.copyDataEntry(None), entries.maxBy(l => dMTree(l, currentNode)).copyDataEntry(None))
    }
  */
  private def newPromote(entries: Vector[NewDataEntry]): (NewDataEntry, NewDataEntry) = {
    entries.combinations(2).maxBy(l => dMTree(l.head, l.tail.head)) match {
      case Vector(a, b) => (a.copy, b.copy)
    }
  }

  private def newPromote(entries: Vector[NewDataEntry], currentNode: NewDataEntry): (NewDataEntry, NewDataEntry) = {
    (currentNode.copy, entries.maxBy(l => dMTree(l, currentNode)).copy)
  }

  /*
    private def partition(entries: Vector[DataEntry], Op1: DataEntry, Op2: DataEntry): (Vector[DataEntry], Vector[DataEntry]) = {
      def helper(entries: Vector[DataEntry], N1: Vector[DataEntry], N2: Vector[DataEntry]): (Vector[DataEntry], Vector[DataEntry]) = {
        if (entries.isEmpty) (N1, N2)
        else if (dMTree(entries.head, Op1) <= dMTree(entries.head, Op2)) { helper(entries.tail, N1 :+ entries.head, N2) }
        else { helper(entries.tail, N1, N2 :+ entries.head) }
        }
      helper(entries, Vector(), Vector())
      }
  */
  private def partition(entries: Vector[NewDataEntry], Op1: NewDataEntry, Op2: NewDataEntry): (Vector[NewDataEntry], Vector[NewDataEntry]) = {
    def helper(entries: Vector[NewDataEntry], N1: Vector[NewDataEntry], N2: Vector[NewDataEntry]): (Vector[NewDataEntry], Vector[NewDataEntry]) = {
      if (entries.isEmpty) (N1, N2)
      else if (dMTree(entries.head, Op1) <= dMTree(entries.head, Op2)) {
        helper(entries.tail, N1 :+ entries.head, N2)
      }
      else {
        helper(entries.tail, N1, N2 :+ entries.head)
      }
    }

    helper(entries, Vector(), Vector())
  }

  override def toString: String = {

    //"root is:" + this.root.toString + "\n" + root.contents.map(_.subTree.get.toString).mkString("\n")
    "TODO"
  }

  def printTree(n: NewNode): Unit = {
    println(n)
    n match {
      case l: LeafTrait => {} // do nothing
      case r: NewNode => r.contents.foreach(de => {
        printTree(de.subTree)
      })
    }
  }

  private def newInsertEntry(n: NewNode, data: NewDataEntry): Unit = {
    n match {
      case l: LeafTrait => {
        if (!l.isFull) {
          l.addContent(data)
        }
        else {
          newSplit(l, data)
        }
      }
      case r: NewNode => {
        // finds the index where the distance is minimum. Two cases can happen:
        // 1) if the distance is negative, it implies that the current node's covering radius is larger than the distance
        //    between the object to insert and the current node, hence no extension of the covering radius is needed.
        //    If multiple nodes can host the node closest to the data entry is selected.
        // 2) if the distance is positive, it means the covering radius needs to be increased. The node where the
        //    increase in covering radius is minimum  is selected.
        val idxOfChild = r.contents.map(node => {
          dMTree(node, data) - node.coveringRadius.get
        }).zipWithIndex.minBy(_._1)
        if (idxOfChild._1 > 0.0) r.contents(idxOfChild._2).coveringRadius = Some(r.contents(idxOfChild._2).coveringRadius.get + idxOfChild._1) // if convering radius is too small, then extend it.
        newInsertEntry(r.contents(idxOfChild._2).subTree, data) // insert into subtree
      }
    }
  }

  /** Split function used in the M-tree.
    *
    * @param n    node to split
    * @param data data entry which is to be inserted
    */
  private def newSplit(n: NewNode, data: NewDataEntry): Unit = {

    val N: Vector[NewDataEntry] = n.contents :+ data // groups all data entries to classify

    // two DataEntry object to be promoted as routing nodes
    val (op1, op2) = n match {
      case r: NewRoot => newPromote(N) // if root, promote two nodes furthest away from each other
      case l: NewNode => newPromote(N, n.parentData) // if not root, promote node furthest away from current parent DataEntry
    }

    val (n1, n2) = partition(N, op1, op2) // allocate each data point to one of two new routing objects

    n match {
      case root: NewRoot => {
        val Np = new NewRoot(Vector(op1, op2)) // allocate new root node Np
        val (nOld, nPrime) = n match { // new node of same type as current node to split
          case a: LeafTrait => (new NewLeaf(n1, Np, op1), new NewLeaf(n2, Np, op2))
          case b: NewNode => (new NewNode(n1, Np, op1), new NewNode(n2, Np, op2))
        }
        op1.replaceSubTree(nOld)
        op2.replaceSubTree(nPrime)

        this.newRoot = Np // set new root as root
      }
      case _ => {
        val np = n.parentNode
        np.removeContent(n.parentData) // remove old routing node from contents
        np.addContent(op1) // add new node

        val (nOld, nPrime) = n match { // new node of same type as current node to split
          case a: LeafTrait => (new NewLeaf(n1, np, op1), new NewLeaf(n2, np, op2))
          case b: NewNode => (new NewNode(n1, np, op1), new NewNode(n2, np, op2))
        }
        op1.replaceSubTree(nOld)
        op2.replaceSubTree(nPrime)

        if (!np.isFull) {
          np.addContent(op2)
        } // if not full then add data entry to contents
        else {
          newSplit(np, op2)
        } // if node is full, split it
      }
    }
  }
}
