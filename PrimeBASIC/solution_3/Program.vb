Imports System.Globalization

Module Program
	Sub Main()
		CultureInfo.CurrentCulture = New CultureInfo("en-US", False)

		Dim referenceResults As New Dictionary(Of Integer, Integer) From {
			{10, 4},
			{100, 25},
			{1000, 168},
			{10000, 1229},
			{100000, 9592},
			{1000000, 78498},
			{10000000, 664579},
			{100000000, 5761455}
		}

		Dim sieveSize = 1000000
		Dim passCount = 0
		Dim sieve As PrimeSieve = Nothing
		Dim startTime = Date.UtcNow

		While (Date.UtcNow - startTime).TotalSeconds <= 5.0
			sieve = New PrimeSieve(sieveSize)
			sieve.Run()
			passCount += 1
		End While

		Dim duration = (Date.UtcNow - startTime).TotalSeconds

		If sieve.CountPrimes <> referenceResults(sieveSize) Then
			Console.WriteLine("WARNING: result is incorrect!")
		End If

		Console.WriteLine($"CopyOf_CS_Kinematics_ArrayPool8of30M_vb;{passCount};{duration};1;algorithm=wheel,faithful=no,bits=1")
	End Sub

End Module
