package myscala.math.algo

class MTree[D](distanceFuntion: (D, D) => Double, leafSize: Int = 10) {

  var newRoot: NewRoot = new NewEmptyRoot

  def insert(id: String, pos: D): Unit = this.newInsertEntry(newRoot, new NewDataEntry(id, pos))
  def insertAll(nodes: Iterable[(String, D)]): Unit = nodes.foreach(o => this.newInsertEntry(newRoot, new NewDataEntry(o._1, o._2)))


  def findInRange(id: String, pos: D, r: Double): List[String] = {
    val obj: NewDataEntry = new NewDataEntry(id, pos)
    val neighbours: collection.mutable.ArrayBuffer[String] = collection.mutable.ArrayBuffer()

    def RangeSearch(n: NewNode): Unit = {
      n match {
        case leafRoot: NewEmptyRoot => {
          neighbours.appendAll(leafRoot.contents.filter(de => dMTree(de, obj) < r).map(_.id))
        }
        case root: NewRoot => {
          root.contents.filter(l => dMTree(l, obj) <= r + l.coveringRadius.get).filter(l => dMTree(l, obj) <= r + l.coveringRadius.get).foreach(de => RangeSearch(de.subTree))
        }
        case leaf: LeafTrait => {
          neighbours.appendAll(leaf.contents.filter(l => dMTree(n.parentData, obj) - dMTree(l, n.parentData) <= r).filter(l => dMTree(l, obj) <= r).map(_.id))
        }
        case node: NewNode => {
          node.contents.filter(l => dMTree(n.parentData, obj) - dMTree(l, n.parentData) <= r + l.coveringRadius.get).filter(l => dMTree(l, obj) <= r + l.coveringRadius.get).foreach(de => RangeSearch(de.subTree))
        }
      }
    }
    RangeSearch(this.newRoot)
    neighbours.toList
  }

  private def dMTree(a: NewDataEntry, b: NewDataEntry): Double = {
    distanceFuntion(a.position, b.position)
  }


  val LEAF_SIZE: Int = leafSize


  class NewDataEntry(val id: String, val position: D, private var _subTree: Option[NewNode], val distanceToParent: Double) {

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

    //def copy(st: Option[NewNode]): NewDataEntry = new NewDataEntry(this.id, this.position, st, this.distanceToParent)

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
      "LEAF-ROOT. contents size=" + this.contents.size + ", contents: " + this.contents.mkString(", ")
    }
  }

  private def newPromote(entries: Vector[NewDataEntry]): (NewDataEntry, NewDataEntry) = {
    entries.combinations(2).maxBy(l => dMTree(l.head, l.tail.head)) match {
      case Vector(a, b) => (a.copy, b.copy)
    }
  }

  private def newPromote(entries: Vector[NewDataEntry], currentNode: NewDataEntry): (NewDataEntry, NewDataEntry) = {
    (currentNode.copy, entries.maxBy(l => dMTree(l, currentNode)).copy)
  }


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
