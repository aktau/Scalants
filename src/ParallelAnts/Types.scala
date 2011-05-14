package object ParallelAnts {
	type MatrixDouble = Array[Array[Double]]
	val MatrixDouble = Array
	
	type MatrixInt = Array[Array[Int]]
	type MatrixIntList = Array[Array[List[Int]]]
	type MatrixIntVector = Array[Array[Vector[Int]]]
	
	type Path[N <: Graph#Node] = scala.collection.mutable.ArrayBuffer[N]
	val Path = scala.collection.mutable.ArrayBuffer
}