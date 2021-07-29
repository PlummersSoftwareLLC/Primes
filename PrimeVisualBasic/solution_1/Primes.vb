Imports System.Collections.Generic
Imports System.Collections

Module Primes
	Dim primeCounts As New Dictionary(Of Integer, Integer)()
	Dim sieveSize As Integer
	Dim sqrtSieveSize As Integer

	Dim rawBits As BitArray = New BitArray(500000)

	Sub InitGlobalVars()
		' I don't think VB.NET has any way to declare dictionary literals, so we just do this
		primeCounts.Add(10, 4)
		primeCounts.Add(100, 25)
		primeCounts.Add(1000, 168)
		primeCounts.Add(10000, 1229)
		primeCounts.Add(100000, 9592)
		primeCounts.Add(1000000, 78498)
		primeCounts.Add(10000000, 664579)
		primeCounts.Add(100000000, 576145)
	End Sub

	Sub CreateSieve(limit As Integer)
		sieveSize = limit
		sqrtSieveSize = Math.Sqrt(limit)

		rawBits.SetAll(True)
	End Sub

	Function GetBit(index As Integer) As Boolean
		If index Mod 2 = 0 Then
			Return False
		Else
			Return rawBits(index \ 2)
		End If
	End Function

	Sub ClearBit(index As Integer)
		If index Mod 2 = 0 Then
			Console.WriteLine("Why are you trying to clear an even number? {0}", index)
		Else
			rawBits(index \ 2) = False
		End If
	End Sub

	Sub RunSieve()
		Dim factor, num As Double
		factor = 3
		While factor <= sqrtSieveSize
			For num = factor To sieveSize Step 2
				If GetBit(num) Then
					factor = num
					' This only exits this inside for loop
					Exit For
				End If
			Next

			For num = factor * 3 To sieveSize Step factor * 2
				ClearBit(num)
			Next

			factor += 2
		End While
	End Sub

	Function ValidateResults(result As Integer) As Boolean
		Dim expectedPrimes As Integer
		' TryGetValue tries to get the value from the dictionary and stores it in the second argument
		If primeCounts.TryGetValue(sieveSize, expectedPrimes) Then
			Return expectedPrimes = result
		Else
			Return False
		End If
	End Function

	Function CountPrimes() As Integer
		Dim count As Integer = 0
		Dim bit As Boolean

		For Each bit In rawBits
			If bit Then
				count += 1
			End If
		Next
		Return count
	End Function

	Sub PrintResults(showResults As Boolean, duration As Decimal, passes As Integer)
		If showResults Then
			Console.Write("2, ")
		End If

		Dim count As Integer = 1
		Dim num As Integer
		For num = 3 To sieveSize - 1 Step 2
			If GetBit(num) Then
				count += 1

				If showResults Then
					Console.Write(CStr(num) & ", ")
				End If
			End If
		Next
		Console.WriteLine()

		If count <> CountPrimes() Then
			Console.WriteLine("count is not equal to CountPrimes()! Something's gone horribly wrong!")
		End If

		Console.WriteLine("Passes: {0}, Time: {1}, Avg: {2}, Limit: {3}, Count: {4}, Valid: {5}",
		CStr(passes), CStr(duration), CStr(duration / passes), CStr(sieveSize), CStr(count), CStr(ValidateResults(count)))

		Console.WriteLine()
		Console.WriteLine("DoctorDalek1963;{0};{1};1;algorithm=base,faithful=no,bits=1",
		CStr(passes), CStr(duration))

	End Sub

	Function UnixTime() As Double
		Return (DateTime.UtcNow - New DateTime(1970, 1, 1, 0, 0, 0)).TotalSeconds
	End Function

	Sub Main()
		InitGlobalVars()

		Dim passes As Integer = 0
		Dim tStart As Double = UnixTime()

		While UnixTime() - tStart < 5
			CreateSieve(1000000)
			RunSieve()
			passes += 1
		End While

		Dim time As Double = UnixTime() - tStart

		PrintResults(False, time, passes)
	End Sub
End Module
