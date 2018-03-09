package object myscala {

  /** Prints the time taken to execute a block of code. This function is used as
    * {{{
    * val g = timeBlock {
    *   buildGraph(conn3.head, conn3.tail, List())
    * }}}
    *
    * @param block code to evaluate
    * @tparam R return type of that code
    * @return returns the same object as the original block would of done
    */
  def timeBlock[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) / 1000000000.0 + " seconds")
    result
  }

}
