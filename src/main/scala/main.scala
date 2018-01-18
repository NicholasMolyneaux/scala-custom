import myscala.math.algo.{MTree, Vector2D, norm}

import scala.util.Random.nextInt


object main {
  def main(args: Array[String]): Unit = {

    val treeTest = new MTree[Vector2D](norm: (Vector2D, Vector2D) => Double)

    /*for (i <- 0 to 5000) {
      treeTest.insert(i.toString, new Vector2D(nextInt(5000), nextInt(5000)))
    }*/


    treeTest.insert("1", new Vector2D(1.0,1.0))
    treeTest.insert("2", new Vector2D(1.1,1.1))
    treeTest.insert("3", new Vector2D(1.2,1.05))
    treeTest.insert("4", new Vector2D(4.0,4.0))
    treeTest.insert("5", new Vector2D(4.0,4.1))
    treeTest.insert("6", new Vector2D(4.1,4.0))
    treeTest.insert("7", new Vector2D(4.2,4.25))
    treeTest.insert("8", new Vector2D(8.1,1.0))
    treeTest.insert("9", new Vector2D(8.2,1.0))
    treeTest.insert("10", new Vector2D(8.3,1.23))
    treeTest.insert("11", new Vector2D(8.4,1.4))
    treeTest.insert("12", new Vector2D(8.5,0.6))
    treeTest.insert("13", new Vector2D(8.6,1.4))
    treeTest.insert("14", new Vector2D(8.34,1.1))
    treeTest.insert("15", new Vector2D(1.3,0.8))
    treeTest.insert("16", new Vector2D(1.5,1.34))
    treeTest.insert("17", new Vector2D(3.8,2.5))
    treeTest.insert("18", new Vector2D(7.5,2.0))
    treeTest.insert("19", new Vector2D(9.4,8.0))
    treeTest.insert("20", new Vector2D(2.1,2.3))
    treeTest.insert("21", new Vector2D(0.9,0.2))
    treeTest.insert("22", new Vector2D(0.8,0.3))


    //treeTest.insert("9", new Vector2D(9.0,1.0))

    //println(treeTest.root)
    //println(treeTest.root.contents(0).subTree)
    //println(treeTest.root.contents(1).subTree)

    //println(treeTest)
    treeTest.printTree(treeTest.root)
  }
}