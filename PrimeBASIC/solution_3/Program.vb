Option Compare Text

Imports System.ComponentModel.DataAnnotations
Imports System.Globalization

Module Program
	Sub Main(args As String())
		CultureInfo.CurrentCulture = New CultureInfo("en-US", False)
		CultureInfo.CurrentUICulture = CultureInfo.CurrentCulture
		Nukepayload2.ConsoleFramework.Application.Run(args, New Action(Of String)(AddressOf Startup))
	End Sub

    Sub Startup(
        <Display(ShortName:="p", Description:="Whether to use the ArrayPool implementation. Valid values: yes, no or all.")>
        arraypool As String)

        Select Case arraypool
            Case "all"
                RunTest(True)
                RunTest(False)
            Case "yes"
                RunTest(True)
            Case "no"
                RunTest(False)
            Case Else
                Console.Error.WriteLine($"The parameter '{NameOf(arraypool)}' is incorrect. Use /? to view help.")
                Environment.Exit(1)
        End Select
    End Sub

    Private Sub RunTest(useArrayPool As Boolean)
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
        Dim sieve As SieveBase

        Const TicksPerMillisecond = 10000L
        Const TicksPerSecond = TicksPerMillisecond * 1000
        Const FiveSeconds = TicksPerSecond * 5

        Dim startTime = Date.UtcNow.Ticks
        Dim implType As String
        If useArrayPool Then
            implType = "ArrayPool"
            Do
                Dim sieveInner As New PrimeArrayPoolSieve(sieveSize)
                sieveInner.Run()
                sieve = sieveInner
                passCount += 1
            Loop While Date.UtcNow.Ticks - startTime < FiveSeconds
        Else
            implType = "ReDim"
            Do
                Dim sieveInner As New PrimeReDimSieve(sieveSize)
                sieveInner.Run()
                sieve = sieveInner
                passCount += 1
            Loop While Date.UtcNow.Ticks - startTime < FiveSeconds
        End If

        Dim duration = (Date.UtcNow.Ticks - startTime) / TicksPerSecond

        If sieve.CountPrimes <> referenceResults(sieveSize) Then
            Console.WriteLine("WARNING: result is incorrect!")
        End If

        Console.WriteLine($"Nukepayload2_{implType}8of30M;{passCount};{duration};1;algorithm=wheel,faithful=yes,bits=1")
    End Sub
End Module
