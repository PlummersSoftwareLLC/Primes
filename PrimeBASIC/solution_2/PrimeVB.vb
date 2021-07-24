Imports System
Imports System.Globalization

Module PrimeVB
	Public Class PrimeSieve
		Private ReadOnly sieveSize As Integer
		Private ReadOnly primesArray As BitArray

		Public Sub New(size As Integer)
			sieveSize = size
			primesArray = New BitArray((size + 1) \ 2, True)
		End Sub

		Public Function CountPrimes() As Integer
			Dim Count = 0

			For Each bit In primesArray
				If bit Then Count += 1
			Next

			Return Count
		End Function

		Public Function RunSieve() As BitArray
			Dim sieveSqrt As Integer = Math.Sqrt(sieveSize)
			Dim number As Integer

			For factor = 3 To sieveSqrt Step 2

				For number = factor To sieveSqrt Step 2
					If primesArray(number \ 2) Then
						factor = number
						Exit For
					End If
				Next

				If number > sieveSqrt Then Exit For

				For number = factor * 3 To sieveSize Step factor * 2
					primesArray(number \ 2) = False
				Next

			Next

			Return primesArray

		End Function

	End Class

	Sub Main()

		CultureInfo.CurrentCulture = New CultureInfo("en-US", False)

		Dim referenceResults = New Dictionary(Of Integer, Integer) From
			{{10, 4},
			{100, 25},
			{1000, 168},
			{10000, 1229},
			{100000, 9592},
			{1000000, 78498},
			{10000000, 664579},
			{100000000, 5761455}}

		Dim sieveSize = 1000000
		Dim passCount = 0
		Dim sieve As PrimeSieve = Nothing
		Dim startTime = DateTime.UtcNow

		While (DateTime.UtcNow - startTime).TotalSeconds <= 5.0
			sieve = New PrimeSieve(sieveSize)
			sieve.RunSieve()
			passCount += 1
		End While

		Dim duration = (DateTime.UtcNow - startTime).TotalSeconds

		If sieve.CountPrimes <> referenceResults(sieveSize) Then
			Console.WriteLine("WARNING: result is incorrect!")
		End If

		Console.WriteLine("rbergen_vb;{0};{1};1;algorithm=base,faithful=yes,bits=1", passCount, duration)
	End Sub
End Module
