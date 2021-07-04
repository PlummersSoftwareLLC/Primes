Option Explicit On
Option Strict On

Imports System.Globalization

Module PrimeVB
    Private Const PRINT_DEBUG As Boolean = False
    Private Const MAX_RUNTIME_SECONDS As Integer = 5
    Private Const SIEVE_SIZE As Integer = 1000000

    Sub Main(args As String())
        CultureInfo.CurrentCulture = New CultureInfo("en_US", False)

        Dim passes As Integer = 0
        Dim sieve As PrimeSieve = Nothing
        Dim stopwatch As Stopwatch = Stopwatch.StartNew()

        While stopwatch.Elapsed.TotalSeconds < MAX_RUNTIME_SECONDS
            sieve = New PrimeSieve(SIEVE_SIZE)
            sieve.RunSieve()

            passes += 1
        End While

        stopwatch.Stop()

        If PRINT_DEBUG Then
            Dim duration As Double = stopwatch.Elapsed.TotalSeconds
            Console.WriteLine(String.Join(", ", sieve?.GetPrimes()))
            Console.WriteLine($"Passes: {passes}, Time: {duration}, Avg: {duration / passes}, Limit: {SIEVE_SIZE}, Count: {sieve?.CountPrimes()}, Valid: {sieve?.ValidateResults()}")
            Console.WriteLine()
        End If

        Console.WriteLine($"jimbojim1997;{passes};{stopwatch.Elapsed.TotalSeconds:G6};1;algorithm=base,faithful=yes,bits=1")
    End Sub
End Module

Class PrimeSieve
    ''' <summary>
    ''' Historical data for validating our results - the number of primes to be found under some limit, such as 168 primes under 1000
    ''' </summary>
    Private Shared ReadOnly _knownPrimes As New Dictionary(Of Integer, Integer)() From {
        {10, 4},
        {100, 25},
        {1000, 168},
        {10000, 1229},
        {100000, 9592},
        {1000000, 78498},
        {10000000, 664579},
        {100000000, 5761455}
    }

    Public ReadOnly SieveSize As Integer
    Public ReadOnly BitArray As BitArray

    Public Sub New(siveSize As Integer)
        Me.SieveSize = siveSize
        BitArray = New BitArray(CInt((siveSize + 1) \ 2), True)
    End Sub

    ''' <summary>
    ''' Calculate the primes up to the specified limit
    ''' </summary>
    ''' <returns></returns>
    Public Function RunSieve() As BitArray
        Dim factor As Integer = 3
        Dim q As Integer = CInt(Math.Sqrt(SieveSize))

        While factor < q
            For num As Integer = factor To SieveSize
                If GetBit(num) Then
                    factor = num
                    Exit For
                End If
            Next

            ' If marking factor 3, you wouldn't mark 6 (it's a mult of 2) so start with the 3rd instance of this factor's multiple.
            ' We can then step by factor * 2 because every second one Is going to be even by definition

            For num As Integer = factor * 3 To SieveSize Step factor * 2
                ClearBit(num)
            Next

            factor += 2
        End While

        Return BitArray
    End Function

    Public Function ValidateResults() As Boolean
        If _knownPrimes.ContainsKey(SieveSize) Then
            Return _knownPrimes.Item(SieveSize) = CountPrimes()
        Else
            Return False
        End If
    End Function

    Public Function CountPrimes() As Integer
        Dim count As Integer = 0
        For i As Integer = 0 To BitArray.Count - 1
            If BitArray.Item(i) Then count += 1
        Next
        Return count
    End Function

    Public Iterator Function GetPrimes() As IEnumerable(Of Integer)
        For i As Integer = 3 To SieveSize
            If GetBit(i) Then Yield i
        Next
    End Function

    Private Sub ClearBit(index As Integer)
        If index Mod 2 = 0 Then
            Return
        Else
            BitArray.Item(CInt(index \ 2)) = False
        End If
    End Sub

    Private Function GetBit(index As Integer) As Boolean
        If index Mod 2 = 0 Then
            Return False
        Else
            Return BitArray.Item(CInt(index \ 2))
        End If
    End Function
End Class