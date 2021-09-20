

DO Main


*±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*    Procedure Main()
*±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*
PROCEDURE Main()


    Local rawbits := .F.
    //Local sieveSize := 0



    Local sieveSize := 1000000  // This needs to be passed in
    Local passCount := 0

    //Dim sieve As PrimeSieve = Nothing
    Local sieve := 0



    //Dim startTime = DateTime.UtcNow
    Local OkToStart := .F.
    LOCAL StartTime := Seconds()
    Local StartTicks := GetTicks()
    LOCAL Duration := 0
    LOCAL NumberFound := 0
    LOCAL Now := Seconds()
    LOCAL NowTicks := GetTicks()


    IF MidnightCheck() = .F. // Check to make sure we are not too close to midnight
   

       // While (DateTime.UtcNow - startTime).TotalSeconds <= 5.0


       DO WHILE Now - StartTime <= 5 .AND. NumberFound < 1
       
          // sieve = New PrimeSieve(sieveSize)
          // sieve.RunSieve()
          NumberFound = RunSieve(sieveSize)


          // passCount += 1
          PassCount = Passcount + 1
          Now = Seconds()


           // End While
       ENDDO




       // Dim duration = (DateTime.UtcNow - startTime).TotalSeconds
       Duration = Seconds() - StartTime
       DurationTicks = GetTicks() - StartTicks


        // If sieve.CountPrimes <> referenceResults(sieveSize) Then

       IF NumberFound <> ReferenceN()

          // Console.WriteLine(@WARNING: result is incorrect!@)
          ?  "WARNING: result is incorrect!"

       ENDIF

       // End If

       // Console.WriteLine(@rbergen_vb;{0};{1};1;algorithm=base,faithful=yes,bits=1@, passCount, duration)

       ? "algorithm=base, faithful=yes, bits<>nibbles, Pass Count : " + ALLTRIM(STR(passCount)) + ",Duration : " + ALLTRIM(STR(duration))

       ? "Seconds : " + ALLTRIM(STR(Duration))

       ? "Ticks : " + ALLTRIM(STR(DurationTicks))


    ENDIF

RETURN


*±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*    Function MidnightCheck()
*±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*
FUNCTION MidnightCheck()


   LOCAL lNoWhereNearMidnight := .F.
   LOCAL StartTime := Seconds()

   //CA Clipper 5.2e Second Function returns number of seconds since midnight
   //0-86399  If we are too close to midnight we will hang around a bit till just gone midnight
   //to ensure that we don't get a negative duration (which would be unfair on some of the other programming languages :)

   IF StartTime > 86300    // 99 Seconds should be enough.....


      // To be implemented

      ? "Too close to midnight please wait until tomorrow and try again....."
      lNoWhereNeedMidnight = .T.

   ENDIF

   lNoWhereNearMidnight = .F.

RETURN lNoWhereNearMidnight

*±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*    Function GETTICKS()
*±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*
STATIC FUNCTION GETTICKS()

   LOCAL nTicks := 0
   //nTicks := FT_PEEK( 0, 1132 )  // &H46C - timer ticks..

RETURN (nTicks)


*±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*    Function ReferenceN()
*±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*
FUNCTION ReferenceN(SieveSize)

   LOCAL ReferenceCount := 0
LOCAL PrimeCounts := {{10,4},{100,25},{1000,168},{10000,1229},{100000,9592},{1000000,78498},{10000000,664579},{100000000,5761455}}


   FOR looop = 1 to LEN(PrimeCounts)
       IF PrimeCounts[looop,1] = SieveSize
           ReferenceCount = PrimeCounts[Looop,2]
           EXIT
       ENDIF
   NEXT


RETURN (ReferenceCount)




*±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*    Function RunSieve()
*±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*
//Public Function RunSieve() As BitArray
FUNCTION RunSieve (sieveSize)



   //  Dim sieveSqrt As Integer = Math.Sqrt(sieveSize)
   //  Dim number As Integer

   LOCAL ResultCount := 0
   LOCAL SieveSqrt := SQRT(sieveSize)
   LOCAL number := 0
   LOCAL factor := 0
   //LOCAL PrimesArray := Array((SieveSize+1)/2)
   LOCAL PrimesArrayCl := {}

   //IF we use a multi dimension array (eeek) and int(/1000) this would be the
   //seed value which can then be used to determine which dimension to use???


//   ALTD()


   PrimesArrayCL := initArray(SieveSize,.F.)




   //AFILL(PrimesArray,.T.)

 


   // For factor = 3 To sieveSqrt STEP 2

   FOR factor = 3 TO SieveSqrt Step 2

      // For number = factor To sieveSqrt Step 2
      FOR number = factor TO SieveSqrt STEP 2


//ALTD()

         // If primesArray(number \ 2) Then
         IF GetElement(PrimesArrayCL,INT(number / 2)) = .F.

            // factor = number
            factor = number


            // Exit For
            EXIT

         // End If
         ENDIF

      //Next
      NEXT number


      // If number > sieveSqrt Then Exit For
      IF number > sieveSqrt
         EXIT
      ENDIF

      //For number = factor * 3 To sieveSize Step factor * 2
      FOR number = factor * 3 TO sieveSize Step factor * 2


         //primesArray(number \ 2) = False
         //primesArray[number / 2] = .F.

         SetElement(PrimesArrayCL,number / 2,.F.)


      //Next
      NEXT

   //Next
   NEXT

//RETURN PrimesArray

RETURN (ResultCount)




*±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*    Function initArray()
*±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*
FUNCTION initArray(ElementCount,initvalue)

   // Replacement for Array() Function to support > 4096 Elements

   LOCAL array := Array(4096)
   LOCAL arraysNeeded := Int(ElementCount / 4096) + 1
   LOCAL subarray



   FOR i = 1 TO ArraysNeeded
      array[i] = Array(4096)
      FOR j = 1 TO 4096
         subarray := array[i]
         subarray[j] = initvalue
      NEXT
   NEXT

RETURN array


*±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*    PROCEDURE SetElement()
*±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*
PROCEDURE SetElement(Array, index, value)

  LOCAL whichArray := (INT(index / 4096)) + 1
  LOCAL whichElement := (index % 4096) + 1

  array[whichArray][whichElement] := value

RETURN

*±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*    Function getElement()
*±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*
FUNCTION getElement(array, index)

  LOCAL WhichArray := (INT(index / 4096)) + 1
  LOCAL WhichElement := (index  % 4096) + 1

//  ALTD()

RETURN array[whichArray][whichElement]


*±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*    Function aElement()
*±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*
FUNCTION aElement(aArray,nElement)
   // Returns Required Element
RETURN aArray[INT(nElement/4096),nElement%4096]


*±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*    Function aAAdd()
*±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*
Function aAAdd(aArray, uElement)
    // Replacement for AADD
    IF LEN(aTail(aArray)) = 4096
       AADD(aArray,{})
    ENDIF

RETURN AAdd(aTail(aArray),uElement)

// Credit Jovan Bulajic, Yogoslavia




// Old Stuff

//        LOCAL aArray
//        LOCAL nLen
//
//        aArray := {}
//
//        ASIZE(aArray,0)
//
//        nLen := INT(nElements/4096)
//
//        FOR i := 1 to nLen
//           AADD(aArray,Array(4096))
//        NEXT
//
//        nLen := (nElements % 4096)

//        ALTD()


//        ASIZE(aArray,nLen)

//        FOR i := 1 to nLen
           //AADD(ATAIL(aArray),NIL)
//           AADD(aArray,ATAIL(aArray))
//
//        NEXT

//RETURN aArray