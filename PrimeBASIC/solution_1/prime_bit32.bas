#ifndef NULL
    #define NULL CPtr(any Ptr, 0)
#endif

#ifndef VALUE_BITCOUNT
    #define VALUE_BITCOUNT 32
#endif

Type UlongintBitAddress
    UlongintIndex As Uinteger
    BitIndex As Ubyte

    Declare Constructor(Number As Uinteger)
End Type

Type BitArray
    ValueArray(Any) As Uinteger<VALUE_BITCOUNT>
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

Constructor UlongintBitAddress(Number As Uinteger)
    This.UlongintIndex = Number \ VALUE_BITCOUNT
    This.BitIndex = Number Mod VALUE_BITCOUNT
End Constructor

Constructor BitArray(Size As Uinteger)
    This.BitCount = Size
    ReDim This.ValueArray(0 To Size \ VALUE_BITCOUNT) As Integer<VALUE_BITCOUNT>
End Constructor

Function BitArray.GetBit(Number As Uinteger) As Boolean
    Dim BitAddress As UlongintBitAddress = UlongintBitAddress(Number - 1)

    Return Bit(This.ValueArray(BitAddress.UlongintIndex), BitAddress.BitIndex)
End Function

Sub BitArray.SetBit(Number As Uinteger)
    Dim BitAddress As UlongintBitAddress = UlongintBitAddress(Number - 1)

    This.ValueArray(BitAddress.UlongintIndex) = BitSet(This.ValueArray(BitAddress.UlongintIndex), CInt(BitAddress.BitIndex))
End Sub

Sub BitArray.ClearBit(Number As Uinteger)
    Dim BitAddress As UlongintBitAddress = UlongintBitAddress(Number - 1)

    This.ValueArray(BitAddress.UlongintIndex) = BitReset(This.ValueArray(BitAddress.UlongintIndex), CInt(BitAddress.BitIndex))
End Sub

Function BitArray.CountBits() As Uinteger
    Dim LastBitAddress As UlongintBitAddress = UlongintBitAddress(This.BitCount - 1)
    Dim BitNumber As Byte = CByte(LastBitAddress.BitIndex)
    Dim UlongintNumber As Uinteger = LastBitAddress.UlongintIndex
    Dim Counter As Uinteger = 0
    Dim CurrentUlongint As Ulongint = This.ValueArray(LastBitAddress.UlongintIndex)

    Do
        Do
            If Bit(CurrentUlongint, BitNumber) Then Counter = Counter + 1
            BitNumber = BitNumber - 1
        Loop Until BitNumber < 0

        If UlongintNumber = 0 Then Exit Do
        
        UlongintNumber = UlongintNumber - 1
        CurrentUlongint = This.ValueArray(UlongintNumber)
        BitNumber = 63
    Loop

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

Print Using "rbergen__bit32;&;#.###;1;algorithm=base,faithful=yes,bits=1"; PassCount; Duration