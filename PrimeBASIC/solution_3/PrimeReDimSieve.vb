Imports System.Runtime.CompilerServices

Class PrimeReDimSieve
	Inherits SieveBase

	Public ReadOnly Property SieveSize As Integer

	' The primes data
	Private ReadOnly data() As ULong

	Private Const BitsPerWord As Integer = 8 * 8
	Private Const PowerScale As Integer = 6 ' 2^6 = 64 (BitsPerWord)
	Private Const MaskScale As Integer = 1 ' 0 for keeping all values, 1 for only odd numbers, etc
	Private Const IndexScale As Integer = PowerScale + MaskScale

	' Only numbers congruent to candidates mod 30 can be prime.
	' Only for informative purposes
	' Dim candidates As New Integer() = { 1, 7, 11, 13, 17, 19, 23, 29 }

	' Steps are the distances to the next candidate
	Private ReadOnly steps() As Integer = {6, 4, 2, 4, 2, 4, 6, 2}

	Public Sub New(sieveSize As Integer)
		If sieveSize < 10 Then
			Throw New ArgumentOutOfRangeException(NameOf(sieveSize), sieveSize, $"Sieve size must be at least 10.")
		End If

		Me.SieveSize = sieveSize
		ReDim data(Me.SieveSize >> IndexScale)
	End Sub

	Public Overrides Function CountPrimes() As Integer
		' Get 2, 3, and 5 for free
		Dim count As Integer = 3

		Dim bits = data.AsSpan()

		Dim index As Integer = 7
		Dim [step] As Integer = 1
		Dim inc As Integer = steps([step])
		Do While index <= SieveSize
			If GetBit(bits, index) = False Then
				count += 1
			End If
			index += inc
			[step] = ([step] + 1) Mod 8
			inc = steps([step])
		Loop

		Return count
	End Function

	Public Function GetFoundPrimes() As IEnumerable(Of Integer)
		' Get 2, 3, and 5 for free
		Dim result As New List(Of Integer) From {2, 3, 5}

		Dim bits = data.AsSpan()

		Dim index As Integer = 7
		Dim [step] As Integer = 1
		Dim inc As Integer = steps([step])
		Do While index <= SieveSize
			If GetBit(bits, index) = False Then
				result.Add(index)
			End If
			index += inc
			[step] = ([step] + 1) Mod 8
			inc = steps([step])
		Loop

		Return result
	End Function

	Public Property ClearCount As Integer

	Public Sub Run()
		Dim bits = data.AsSpan()
		bits.Fill(0)

		Dim q = CInt(Math.Truncate(Math.Sqrt(SieveSize)))

		' We can skip 2, 3, and 5, and start our checks with 7.
		' This will be index 1 in the steps list.

		' Cached bitmaps and index offsets for bit twiddling loop.
		Dim masks(BitsPerWord - 1) As ULong
		Dim offsets(BitsPerWord - 1) As Integer

		' Look for next prime
		Dim factor = 7
		Dim [step] = 1
		Dim inc As Integer = steps([step])
		Do While factor <= q
			' A set bit means it's composite - keep searching.
			If GetBit(bits, factor) Then
				factor += inc
				[step] = ([step] + 1) Mod 8
				inc = steps([step])
				Continue Do
			End If

			' The following loop is the hotspot for this algorithm.
			' No need to start less than p^2 since all those
			' multiples have already been marked.

			' Performance optimization: since the bit mask we `or`
			' into the word (and the index offset added to the base)
			' RECUR every 64 iterations of this loop (for 64-bit words), we
			' can precalculate them and use them over and over until the end
			' of the sieve array.

			Dim baseVal As Integer = IndexOf(factor * factor)
			Dim cumOffset = 0
			masks.AsSpan().Fill(0)
			Dim iUsed = 0
			Dim offset = 0

			Dim bitCount = 0
			Dim m As Integer = factor * factor
			Do While bitCount < BitsPerWord
				masks(iUsed) = masks(iUsed) Or MaskOf(m)

				offset = IndexOf(m + factor * 2) - IndexOf(m)

				' Don't advance to a new word unless the next
				' bit is in the same word!
				If offset <> 0 Then
					offsets(iUsed) = offset
					iUsed += 1
					cumOffset += offset
				End If
				bitCount += 1
				m += factor * 2
			Loop

			' In this case, the bits in the last word can just
			' be merged to the first.
			If offset = 0 Then
				masks(0) = masks(0) Or masks(iUsed)
			End If

			' In all cases, iUsed will be 1 BEYOND the last mask used.

			' Now just rip through the array or-ing in these masks in an
			' identical pattern.
			Dim iStop As Integer = IndexOf(SieveSize)
			Dim i As Integer = baseVal

			Do While i <= iStop - cumOffset
				For j2 As Integer = 0 To iUsed - 1
					bits(i) = bits(i) Or masks(j2)
					i += offsets(j2)
				Next j2
			Loop

			' Finish last few words being careful about array bounds.
			Dim j = 0
			Do While j < iUsed AndAlso i <= iStop
				bits(i) = bits(i) Or masks(j)
				i += offsets(j)
				j += 1
			Loop

			ClearBit(bits, factor)
			factor += inc
			[step] = ([step] + 1) Mod 8
			inc = steps([step])
		Loop
	End Sub

	<MethodImpl(MethodImplOptions.AggressiveInlining)>
	Private Shared Function IndexOf(n As Integer) As Integer
		Return n >> IndexScale
	End Function

	<MethodImpl(MethodImplOptions.AggressiveInlining)>
	Private Shared Function MaskOf(n As Integer) As ULong
		Return 1UL << ((n >> MaskScale) Mod BitsPerWord)
	End Function

	<Obsolete("Span")>
	<MethodImpl(MethodImplOptions.AggressiveInlining)>
	Private Shared Function GetBit(ByRef bits As Span(Of ULong), index As Integer) As Boolean
		Return (bits(index >> IndexScale) And (1UL << (index >> MaskScale))) <> 0
	End Function

	<Obsolete("Span")>
	<MethodImpl(MethodImplOptions.AggressiveInlining)>
	Private Shared Sub ClearBit(ByRef bits As Span(Of ULong), index As Integer)
		bits(index >> IndexScale) = bits(index >> IndexScale) And Not (1UL << (index >> MaskScale))
	End Sub
End Class
