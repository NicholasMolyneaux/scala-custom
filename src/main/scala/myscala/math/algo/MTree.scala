package myscala.math.algo

class MTree[D <: PhysicalVector](distanceFuntion: (D, D) => Double) {

  var root: Leaf = new Leaf
  root.isRoot = true

  def insert(id: String, pos: D): Unit = this.insertEntry(root, new DataEntry(id, pos))

  private def dMTree(a: DataEntry, b: DataEntry): Double = { distanceFuntion(a.position, b.position) }

  class DataEntry(val id: String, val position: D) {

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
  }

  val LEAF_SIZE: Int = 4


  sealed class NewLeaf(val contents: Vector[DataEntry], val parentNode: Option[NewLeaf], val parentData: Option[DataEntry]) {

  }

  sealed class Leaf {

    def this() = this()

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
    var coveringRadius: Option[Double] = None
    override def toString: String = {
      "ROUTINGNODE. covRad=" + this.coveringRadius + ", contents size=" + this.contents.size + ", contents: " + this.contents.mkString(", ")
    }
  }

  final class RootNode extends RoutingNode {
    override val isRoot: Boolean = true

    def asRoutingNode: RoutingNode
  }

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

    val nPrime = n match { // new node of same type as current node to split
      case b: RoutingNode => new RoutingNode
      case a: Leaf => new Leaf
    }

    // two DataEntry object to be promoted as routing nodes
    val (op1, op2) = if (n.isRoot){ promote(N) } // if root, promote two nodes furthest away from each other
    else { promote(N, op.get) } // if not root, promote node furthest away from current parent DataEntry

    val (n1, n2) = partition(N, op1, op2) // allocate each data point to one of two new routing objects

    n.replaceContents(n1, op1) // allocate the contents from partitioning to n
    op1.leafToRouting(n) // update data entry

    nPrime.replaceContents(n2, op2) // allocate the contents from partitioning to nPrime
    op2.leafToRouting(nPrime) // update data entry

    if (n.isRoot){
      val Np = new RoutingNode // allocate new root node Np
      Np.isRoot = true // set new node as root
      n.isRoot = false // set old node as non root

      n.parentNode = Some(Np) // set parent of old node to root
      Np.addContent(op1) // add node as routing ndoe in root

      nPrime.parentNode = Some(Np) // set entry of node to root
      Np.addContent(op2) // add node as routing node in root

      this.root = Np // set new root as root
    } else {
      np.get.removeContent(op.get) // remove old routing node from contents
      np.get.addContent(op1) // add new node

      nPrime.parentNode = Some(np.get) // set parent of new node to parent of old node

      if (!np.get.isFull) { np.get.addContent(op2) } // if not full then add data entry to contents
      else { split(np.get, op2) } // if node is full, split it
    }
  }

  private def promote(entries: Vector[DataEntry]): (DataEntry, DataEntry)  = {
    entries.combinations(2).maxBy(l => dMTree(l.head, l.tail.head)) match { case Vector(a,b) => (a.copyDataEntry(None),b.copyDataEntry(None)) }
  }

  private def promote(entries: Vector[DataEntry], currentNode: DataEntry): (DataEntry, DataEntry)  = {
    (currentNode.copyDataEntry(None), entries.maxBy(l => dMTree(l, currentNode)).copyDataEntry(None))
  }

  private def partition(entries: Vector[DataEntry], Op1: DataEntry, Op2: DataEntry): (Vector[DataEntry], Vector[DataEntry]) = {
    def helper(entries: Vector[DataEntry], N1: Vector[DataEntry], N2: Vector[DataEntry]): (Vector[DataEntry], Vector[DataEntry]) = {
      if (entries.isEmpty) (N1, N2)
      else if (dMTree(entries.head, Op1) <= dMTree(entries.head, Op2)) { helper(entries.tail, N1 :+ entries.head, N2) }
      else { helper(entries.tail, N1, N2 :+ entries.head) }
      }
    helper(entries, Vector(), Vector())
    }

  override def toString: String = {

    "root is:" + this.root.toString + "\n" + root.contents.map(_.subTree.get.toString).mkString("\n")
  }

  def printTree(n: Leaf): Unit = {
    println(n)
    n match {
      case r: RoutingNode => r.contents.foreach(de => {printTree(de.subTree.get)})
      case l: Leaf => {} // do nothing
    }
  }
}
