import myscala.math.algo.{MTree, Vector2D, norm}

val treeTest = new MTree[Vector2D](norm: (Vector2D, Vector2D) => Double)

treeTest.insert("abc", new Vector2D(1.0,1.0))