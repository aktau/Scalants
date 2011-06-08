package ParallelAnts

object Utility {
  def printMatrix[T](matrix: Seq[Array[T]]) =
    for (i <- 0 until matrix.length)
      printArray(matrix(i), i)

  def printArray[T](arr: Array[T], count: Int) =
    printf("%d: %s\n", count, arr.mkString("[", " ", "]"))

  def deepClone(matrix: MatrixIntVector): MatrixIntVector =
    matrix.map(_.clone)

  def deepClone(matrix: MatrixInt): MatrixInt =
    matrix.map(_.clone)

  def copyValues(original: MatrixInt, duplicate: MatrixInt): Unit =
    for (i <- 0 until original.length)
      Array.copy(original(i), 0, duplicate(i), 0, original(i).length)

  def invertedSign(key: Int): Int = if (key < 0) +1 else -1
  def toIntSign(team: Int): Int = if (team > 0) +1 else -1
  def isAway(team: Int): Boolean = if (team > 0) false else true

  def toIndex(team: Int): Int = math.abs(team) - 1
  def toTeam(key: Int): Int = math.abs(key) + 1

  // this is NOT a copy, changes have very real effects people!
  def row(solution: MatrixIntVector, i: Int) = solution(i)

  // this is a copy
  def column(solution: MatrixIntVector, i: Int) = (0 until solution.length).map(solution(_)(i))
  def allColumns(solution: MatrixIntVector) = {
    val rounds = (0 until solution.length)
    val teams = (0 until solution(0).length)

    teams.map(team => rounds.map(round => solution(round)(team)))
  }
}

object Timef {
  def apply[T, R](f: => T, res:(T, Long) => R) : R = {
    val startTime = System.currentTimeMillis
    res(f, System.currentTimeMillis - startTime)
  }
}