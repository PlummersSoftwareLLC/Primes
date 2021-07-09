#ifndef NULL
    #define NULL CPtr(any Ptr, 0)
#endif

#ifndef VALUE_BITCOUNT
    #define VALUE_BITCOUNT 64
#endif

#ifndef ValueIndex
    #define ValueIndex(number) ((number) \ (2 * VALUE_BITCOUNT))
#endif

#ifndef BitMask
    #define BitMask(number) (1 Shl (((number) \ 2) Mod VALUE_BITCOUNT))
#endif

#ifndef ValueCount
    #define ValueCount(number) (ValueIndex(number) + 1)
#endif

Type BitArray
    ValueArray(Any) As Uinteger<VALUE_BITCOUNT>

    Declare Constructor(Size As Uinteger)
End Type

Type PrimeSieve
    NotPrimeArray As BitArray Ptr
    SieveSize As Uinteger
    SieveSteps(0 to 7) As Uinteger
    Candidates(0 to 7) As Uinteger => {1, 7, 11 , 13, 17, 19, 23, 29}

    Declare Constructor(Size As Uinteger)
    Declare Destructor
    Declare Function CountPrimes() As Uinteger
    Declare Function RunSieve() As BitArray Ptr
End Type

Constructor BitArray(Size As Uinteger)
    ReDim This.ValueArray(0 To (Size - 1)) As Integer<VALUE_BITCOUNT>
End Constructor

Constructor PrimeSieve(Size As Uinteger)
    This.SieveSize = Size
    This.NotPrimeArray = New BitArray(ValueCount(Size))

    For Index As Integer = 0 To 7
        This.SieveSteps(Index) = (This.Candidates((Index + 1) Mod 8) - This.Candidates(Index) + 30) Mod 30
    Next

End Constructor

Destructor PrimeSieve
    Delete This.NotPrimeArray
End Destructor

Function PrimeSieve.CountPrimes() As Uinteger
    Dim Counter As Uinteger = 3
    Dim SieveStep As Uinteger = 1
    Dim Factor As Uinteger = 7

    While Factor <= This.SieveSize
        If (This.NotPrimeArray->ValueArray(ValueIndex(Factor)) And BitMask(Factor)) = 0 Then Counter = Counter + 1
        
        Factor = Factor + This.SieveSteps(SieveStep)
        SieveStep = (SieveStep + 1) Mod 8
    Wend

    Return Counter
End Function

Function PrimeSieve.RunSieve() As BitArray Ptr
    Dim As Uinteger Factor, Number, Index, SieveStep, BaseValueIndex, CumulativeOffset
    Dim As Integer UsedValueIndex, Offset, InnerIndex
    Dim StopIndex As Uinteger = ValueIndex(This.SieveSize)
    Dim MaxFactor As Uinteger = CUint(Sqr(This.SieveSize)) + 1
    Dim Masks(0 to (VALUE_BITCOUNT - 1)) As Integer<VALUE_BITCOUNT>
    Dim Offsets(0 to (VALUE_BITCOUNT - 1)) As Uinteger
 
    Factor = 7
    SieveStep = 1
    While Factor <= MaxFactor
        If (This.NotPrimeArray->ValueArray(ValueIndex(Factor)) And BitMask(Factor)) <> 0 Then 
            Factor = Factor + This.SieveSteps(SieveStep)
            SieveStep = (SieveStep + 1) Mod 8    
            Continue While
        End If

        For Index = 0 To (VALUE_BITCOUNT - 1)
            Masks(Index) = 0
        Next

        Number = Factor * Factor
        BaseValueIndex = ValueIndex(Number)
        CumulativeOffset = 0
        UsedValueIndex = 0
        Offset = 0
        Index = 0
        While Index < VALUE_BITCOUNT
            Masks(UsedValueIndex) = Masks(UsedValueIndex) Or BitMask(Number)
            Offset = ValueIndex(Number + Factor + Factor) - ValueIndex(Number)

            If Offset <> 0 Then
                Offsets(UsedValueIndex) = Offset
                UsedValueIndex = UsedValueIndex + 1
                CumulativeOffset = CumulativeOffset + Offset
            End If

            Index = Index + 1
            Number = Number + Factor + Factor
        Wend

        If Offset = 0 Then Masks(0) = Masks(0) Or Masks(UsedValueIndex)

        Index = BaseValueIndex
        While Index <= (StopIndex - CumulativeOffset)
            For InnerIndex = 0 To (UsedValueIndex - 1)
                This.NotPrimeArray->ValueArray(Index) = This.NotPrimeArray->ValueArray(Index) Or Masks(InnerIndex)
                Index = Index + Offsets(InnerIndex)
            Next
        Wend

        InnerIndex = 0
        While InnerIndex < UsedValueIndex And Index <= StopIndex
            This.NotPrimeArray->ValueArray(Index) = This.NotPrimeArray->ValueArray(Index) Or Masks(InnerIndex)
            Index = Index + Offsets(InnerIndex)
            InnerIndex = InnerIndex + 1
        Wend

        Factor = Factor + This.SieveSteps(SieveStep)
        SieveStep = (SieveStep + 1) Mod 8
    Wend

    This.NotPrimeArray->ValueArray(0) = This.NotPrimeArray->ValueArray(0) And Not (BitMask(7) Or BitMask(11))

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

Print Using "rbergen__8of30;&;#.###;1;algorithm=wheel,faithful=yes,bits=1"; PassCount; Duration