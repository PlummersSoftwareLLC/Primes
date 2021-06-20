#ifndef NULL
    #define NULL CPtr(any Ptr, 0)
#endif

Type BitArray
    ValueArray(Any) As Boolean
    BitCount As Uinteger

    Declare Constructor(Size As Uinteger)
    Declare Function GetBit(Number As Uinteger) As Boolean
    Declare Sub SetBit(Number As Uinteger)
    Declare Sub ClearBit(Number As Uinteger)
    Declare Function CountBits() As Uinteger
End Type

Type PrimeSieve
    NotPrimeArray As BitArray Ptr
    SieveSize As Uinteger

    Declare Constructor(Size As Uinteger)
    Declare Destructor
    Declare Function CountPrimes() As Uinteger
    Declare Function RunSieve() As BitArray Ptr
End Type

Constructor BitArray(Size As Uinteger)
    This.BitCount = Size
    ReDim This.ValueArray(1 To Size) As Boolean
End Constructor

Function BitArray.GetBit(Number As Uinteger) As Boolean
    Return This.ValueArray(Number)
End Function

Sub BitArray.SetBit(Number As Uinteger)
    This.ValueArray(Number) = True
End Sub

Sub BitArray.ClearBit(Number As Uinteger)
    This.ValueArray(Number) = False
End Sub

Function BitArray.CountBits() As Uinteger
    Dim Counter As Uinteger = 0

    For Number As Integer = 1 To This.BitCount
        If This.ValueArray(Number) Then Counter = Counter + 1
    Next

    Return Counter
End Function

Constructor PrimeSieve(Size As Uinteger)
    This.SieveSize = Size
    This.NotPrimeArray = New BitArray((Size + 1) \ 2)
End Constructor

Destructor PrimeSieve
    Delete This.NotPrimeArray
End Destructor

Function PrimeSieve.CountPrimes() As Uinteger
    Return (This.SieveSize + 1) \ 2 - This.NotPrimeArray->CountBits()
End Function

Function PrimeSieve.RunSieve() As BitArray Ptr
    Dim As Uinteger Factor, Number
    Dim SieveSqrt As Uinteger = CUint(Sqr(This.SieveSize))

    For Factor = 3 To SieveSqrt Step 2
        Number = Factor

        For Number = Factor To SieveSqrt Step 2
            If Not This.NotPrimeArray->GetBit(Number \ 2) Then
                Factor = Number
                Exit For
            End If
        Next

        If (Number > SieveSqrt) Then Exit For

        For Number = Factor * Factor To SieveSize Step Factor * 2
            This.NotPrimeArray->SetBit(Number \ 2)
        Next
    Next

    Return This.NotPrimeArray
End Function

Function GetReferenceResult(SieveSize As UInteger) As Integer
    Dim Result as Integer = -1

    Rem The following hurts my eyes, but FreeBASIC doesn't natively support anything resembling a hash/dictionary type

    If SieveSize = 10 Then 
        Result = 4
    ElseIf SieveSize = 100 Then 
        Result = 25
    ElseIf SieveSize = 1000 Then 
        Result = 168
    ElseIf SieveSize = 10000 Then 
        Result = 1229
    ElseIf SieveSize = 100000 Then 
        Result = 9592
    ElseIf SieveSize = 1000000 Then 
        Result = 78498
    ElseIf SieveSize = 10000000 Then 
        Result = 664579
    ElseIf SieveSize = 100000000 Then 
        Result = 5761455
    End If

    Return Result
End Function

Dim SieveSize As Integer = 1000000
Dim PassCount As Uinteger = 0
Dim Sieve As PrimeSieve Ptr = NULL
Dim Duration As Double
Dim StartTime As Double = Timer

Do
    If Sieve <> NULL Then Delete Sieve
    Sieve = New PrimeSieve(SieveSize)
    Sieve->RunSieve()
    PassCount = PassCount + 1
Loop Until (Timer - StartTime) > 5.0

Duration = Timer - StartTime

If GetReferenceResult(SieveSize) <> Sieve->CountPrimes() Then Print "WARNING: result is incorrect!"

Delete Sieve

Print Using "rbergen__boolean;&;#.###;1;algorithm=base,faithful=yes"; PassCount; Duration