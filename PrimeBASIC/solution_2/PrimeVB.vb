Imports System
Imports Extension = System.Runtime.CompilerServices.ExtensionAttribute
Imports BitArray = System.Collections.BitArray

Module PrimeVB
	<Extension>
	Public Function is_correct_result(Result_count As Integer,
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


	Public Const sieve_size = 1000000

	Sub Main()
		Globalization.CultureInfo.CurrentCulture = New Globalization.CultureInfo("en-US", False)

		Dim Faithful = primes_faintful()
	End Sub
End Module

Public Module Faithful
	Public Function primes_faintful() As prime_sieve
		Dim Pass_count = 0,
			Sieve As prime_sieve = Nothing,
			Start_time = DateTime.UtcNow
		Do
			If (DateTime.UtcNow - Start_time).TotalSeconds > 5.0 Then
				Call (DateTime.UtcNow - Start_time).TotalSeconds.
					  report(Sieve.count, Pass_count)
				Return Sieve
			End If

			Sieve = New prime_sieve(sieve_size)
			Pass_count += 1
		Loop
	End Function
	<Extension>
	Private Sub report(Duration As Double,
					   Sieve_count As Integer,
					   Pass_count As Integer)
		If Not Sieve_count.is_correct_result(sieve_size) Then
			Console.WriteLine("WARNING: result is incorrect!")
		End If

		Console.WriteLine($"rbergen_vb;{Pass_count};{Duration};1;algorithm=base,faithful=yes,bits=1")
	End Sub
End Module

Public Class prime_sieve
	Private ReadOnly primes As BitArray

	Public ReadOnly Property result(Index As Integer) As Boolean
		Get
			Return primes(Index)
		End Get
	End Property

	Public ReadOnly Property count As Integer
		Get
			Dim Counter = 0
			For Each Bit As Boolean In primes
				Counter += Bit And 1
			Next
			Return Counter
		End Get
	End Property

	Public Sub New(Size As Integer)
		primes = New BitArray((Size + 1) \ 2, True)
		process_prim(Size, 3)
	End Sub

	Private Function check_prim(Sieve_sqrt As Integer,
								ByRef Factor As Integer) As Integer
		Dim Number = Factor

		Do
			If Number > Sieve_sqrt Then Return Number
			If primes(Number \ 2) Then
				Factor = Number
				Return Number
			End If
			Number = Number + 2
		Loop
	End Function

	Private Sub find_prim(Size As Integer,
						  Factor As Integer)
		Dim Number = Factor * 3
		Do
			If Number > Size Then Return
			primes(Number \ 2) = False
			Number = Number + Factor * 2
		Loop
	End Sub

	Private Sub process_prim(Size As Integer,
							 Factor As Integer)
		Dim Sieve_sqrt = Math.Sqrt(Size)

		Do
			If Factor > Sieve_sqrt Then Return
			If check_prim(Sieve_sqrt, Factor) > Sieve_sqrt Then Return

			find_prim(Size, Factor)
			Factor = Factor + 2
		Loop
	End Sub
End Class
