Imports Extension = System.Runtime.CompilerServices.ExtensionAttribute

Public Module Prime_sieve
	<Extension>
	Private Function check_prim(Primes As BitArray,
						   Sieve_sqrt As Integer,
						   ByRef Factor As Integer) As Integer
		Dim Number = Factor

		'Pure "Do" compile difference then "Do Unitl" or "While" in JIT ASM
		Do
			If Number > Sieve_sqrt Then Return Number
			If Primes(Number \ 2) Then
				Factor = Number
				Return Number
			End If
			Number = Number + 2
		Loop
	End Function

	<Extension>
	Private Sub find_prim(Primes As BitArray,
						  Size As Integer,
						  Factor As Integer)
		Dim Number = Factor * 3
		Do
			If Number > Size Then Return
			Primes(Number \ 2) = False
			Number = Number + Factor * 2
		Loop
	End Sub

	<Extension>
	Private Sub run_prim(Primes As BitArray,
						 Size As Integer,
						 Factor As Integer)
		Dim Sieve_sqrt = Math.Sqrt(Size)

		Do
			If Factor > Sieve_sqrt Then Return
			If Primes.check_prim(Sieve_sqrt, Factor) > Sieve_sqrt Then Return

			Primes.find_prim(Size, Factor)
			Factor = Factor + 2
		Loop
	End Sub

	<Extension>
	Public Sub prime_sieve(Primes As BitArray, Size As Integer)
		Primes.run_prim(Size, 3)
	End Sub

	<Extension>
	Public Function count_primes(Primes As BitArray) As Integer
		Dim Count = 0

		For Each Bit As Boolean In Primes
			Count += Bit And 1
		Next

		Return Count
	End Function
End Module

Module PrimeVB

	<Extension>
	Function is_correct_result(Result_count As Integer,
							   Sieve_size As Integer) As Boolean
		Select Case Sieve_size
			Case 10
				Return Result_count = 4
			Case 100
				Return Result_count = 25
			Case 1000
				Return Result_count = 168
			Case 10000
				Return Result_count = 1229
			Case 100000
				Return Result_count = 9592
			Case 1000000
				Return Result_count = 78498
			Case 10000000
				Return Result_count = 664579
			Case 100000000
				Return Result_count = 5761455
		End Select
		Return False
	End Function


	Private Const sieve_size = 1000000
	Sub primes()

		Dim Pass_count = 0,
			Sieve As New BitArray((sieve_size + 1) \ 2),
			Start_time = DateTime.UtcNow
		Do
			If (DateTime.UtcNow - Start_time).TotalSeconds > 5.0 Then
				Call (DateTime.UtcNow - Start_time).TotalSeconds.
					  report(Sieve, Pass_count)
				Return
			End If

			Sieve.SetAll(True)
			Sieve.prime_sieve(sieve_size)
			Pass_count += 1
		Loop
	End Sub
	<Extension>
	Sub report(Duration As Double, Sieve As BitArray, Pass_count As Integer)
		If Not Sieve.count_primes.is_correct_result(sieve_size) Then
			Console.WriteLine("WARNING: result is incorrect!")
		End If

		Console.WriteLine($"rbergen_vb;{Pass_count};{Duration};1;algorithm=base,faithful=no,bits=1")
	End Sub

	Sub Main()
		Globalization.CultureInfo.CurrentCulture = New Globalization.CultureInfo("en-US", False)

		primes()
	End Sub
End Module
