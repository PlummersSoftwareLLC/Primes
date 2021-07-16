Imports BenchmarkDotNet.Attributes

<MemoryDiagnoser>
Public Class BenchmarkSieves
	<Params(1000000)>
	Public Property SieveSize As Integer

	<Benchmark>
	Public Sub ArrayPool8of30M()
		Call New PrimeSieve(SieveSize).Run()
	End Sub
End Class
