*±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*   Procedure SIEVE.PRG
*
*      Dave's Garage Prime Sieve Speed Test Algorithm For Different
*      Computer Languages
*
*      Development Languge : Computer Associate's Clipper Version 5.2e
*
*      Authors : Andy Radford, Bradley Chatha
*
*      Date    : 20/9/2021   (DD/MM/CCYY)
*
*    
*±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*
*
*   Notes:
*      1. Clipper version 5.2e
*
*      2. No Additional Libraries have been used eg. (CA-Tools)
*
*      3. If runtime exceeds 1 day, the duration will be eroneous
*
*      4. Clipper 5.2e supports a maximum array size of 4096 in any
*         dimension, therefore a multi-deminsion array has been used
*
*      5. In order to speed up the algorithm the .T./.F. state of the
*         array is inverted.  There is no quick way to set the starting
*         state of the array to .T. for each element other than by
*         iterating through the each element in the array and setting the
*         value.  Therefore it is loaded in its default (.F. or NILL) and
*         set to .T. if number isn't prime.  Thus there is an inverted logic
*         compared to other implementations of the sieve algorithm
*
*      6. The sieve size is determined by the SieveSize variable (currently
*         set to 1000000
*
*      7. Clipper's array element size is 14 bytes
*
*      8. Due to the limition of 4096 elements in an array, it is not expected
*         that any value in excess of 1,000,000 be passed in.  Values in
*         excess of this are likely to result in an Out or Memory or Memory
*         Overflow error
*
*
*±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*
*   FUNCTIONS AND PROCEDURES
*
*      PROCEDURE Main()
*      FUNCTION ReferenceN()
*      FUNCTION RunSieve()
*      FUNCTION InitArray()
*      PROCEDURE SetElement()
*      FUNCTION GetElement()      
*
*±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±


Main()


*±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*    PROCEDURE Main()
*±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*
PROCEDURE Main()
    LOCAL SieveSize := 1000000
    LOCAL PassCount := 0
    LOCAL StartTime := SECONDS()
    LOCAL FinishTime := 0
    LOCAL Duration := 0
    LOCAL NumberFound := 0
    LOCAL Now := Seconds()
    LOCAL OutputString := ""

    DO WHILE Now - StartTime <= 5
     
       NumberFound = RunSieve(SieveSize)
       PassCount = Passcount + 1
       Now = SECONDS()

   ENDDO

   //CA Clipper 5.2e SECOND() Function returns number of seconds since midnight
   //0-86399  
   //
   //So rather than end up with a negative duration (which would be unfair on
   //the competitors and time travel is not yet possible, I will add 86399
   //(seconds in a day) to the finish time if the duration works out as
   //negative
   //
   //Limition : This does not consider the possibility of it running multiple
   //Days

   FinishTime = SECONDS()
   Duration = FinishTime - StartTime

   IF Duration < 0

      FinishTime = FinishTime + 86399
      Duration = FinishTime - StartTime

   ENDIF

   IF NumberFound <> ReferenceN(SieveSize)

      ? "WARNING: result is incorrect!"

   ENDIF

   OutputString = "AndyRadford,Clip5.2e"
   OutputString = OutputString + ";" + ALLTRIM(STR(PassCount))
   OutputString = OutputString + ";" + ALLTRIM(STR(Duration))
   OutputString = OutputString + ";" + ALLTRIM(STR(1))
   OutputString = OutputString + ";" + ALLTRIM("algorithm=base")
   OutputString = OutputString + "," + ALLTRIM("faithful=no")
   OutputString = OutputString + "," + ALLTRIM("bits=112")

   ? OutputString

RETURN


*±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*    FUNCTION ReferenceN()
*±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*
FUNCTION ReferenceN(SieveSize)

   LOCAL Primecounts := {{10,4},{100,25},{1000,168},{10000,1229},{100000,9592},{1000000,78498},{10000000,664579},{100000000,5761455}}
   LOCAL ReferenceCount := 0

   FOR Looop = 1 TO LEN(PrimeCounts)

       IF PrimeCounts[looop,1] = SieveSize
           ReferenceCount = PrimeCounts[Looop,2]
           EXIT
       ENDIF

   NEXT

RETURN (ReferenceCount)


*±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*    FUNCTION RunSieve()
*±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*
FUNCTION RunSieve (SieveSize)

   LOCAL ResultCount := 0
   LOCAL SieveSqrt := SQRT(SieveSize)
   LOCAL Number := 0
   LOCAL Factor := 0
   LOCAL PArray := (SieveSize+1)/2
   LOCAL PrimesArrayCL := {}

   PrimesArrayCL := InitArray(PArray,.F.)

   FOR Factor = 3 TO SieveSqrt Step 2

      FOR Number = Factor TO SieveSqrt STEP 2

         IF GetElement(PrimesArrayCL,INT(Number / 2)) = .F.
            Factor = Number
            EXIT
         ENDIF

      NEXT Number

      IF Number > SieveSqrt
         EXIT
      ENDIF

      FOR Number = Factor * 3 TO SieveSize STEP Factor * 2
         SetElement(PrimesArrayCL,INT(Number / 2),.T.)
      NEXT

   NEXT

   //Count the number of Primes (inverted logic warning)

   FOR Counter = 1 TO PArray

      IF GetElement(PrimesArrayCL,Counter) = .F.
         ResultCount = ResultCount + 1
      ENDIF

   NEXT

RETURN (ResultCount)


*±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*    FUNCTION InitArray()
*±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*
FUNCTION InitArray(ElementCount,initvalue)

   // Replacement for Clipper AARRAY() function to support > 4096 elements

   LOCAL Array := ARRAY(4096)
   LOCAL ArraysNeeded := INT(ElementCount / 4096) + 1
   LOCAL Subarray
   LOCAL I:=0
   LOCAL J:=0

   FOR I = 1 TO ArraysNeeded

      Array[I] = ARRAY(4096)
      FOR J = 1 TO 4096
         Subarray := ARRAY[I]
         Subarray[J] = InitValue
      NEXT

   NEXT

RETURN (array)


*±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*    PROCEDURE SetElement()
*±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*
PROCEDURE SetElement(Array, Index, Value)

  // Support of setting array element when the array size > 4096 elements
 
  LOCAL WhichArray := (INT(Index / 4096)) + 1
  LOCAL WhichElement := (Index % 4096) + 1

  Array[WhichArray][WhichElement] := Value

RETURN

*±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*    FUNCTION GetElement()
*±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*
FUNCTION GetElement(Array, Index)

  // Retrieve element for array > 4096 elements

  LOCAL WhichArray := (INT(Index / 4096)) + 1
  LOCAL WhichElement := (Index  % 4096) + 1

RETURN (Array[WhichArray][WhichElement])

***
* VERSION HISTORY :
*   1.0 : Initial Release (20/09/2021 ACR)
*         Credit Jovan Bulajic, Yogoslavia :
*         Solution to Clippers 4096 single array limit
*         Concept of Multi-Dimensional Array
*   1.1 : Correct output (22/09/2021 ACR)
*
* EoF: SIEVE.PRG
