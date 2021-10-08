
*±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*   Procedure SIEVEDB.PRG
*
*      Dave's Garage Prime Sieve Speed Test Algorithm For Different
*      Computer Languages
*
*      Development Languge : Computer Associate's Clipper Version 5.2e
*
*      Version : Table Version
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
*         dimension, therefore this version can be used as a comparison of
*         multi dimension array verses creation of a temporary table and
*         populating the table with the required values
*
*      5. In order to speed up the algorithm the .T./.F. state of the
*         array is inverted.  There is no quick way to set the starting
*         state of the array to .T. for each element other than by
*         iterating through the each element in the array and setting the
*         value.  Therefore it is loaded in its default (.F. or NILL) and
*         set to .T. if number isn't prime.  Thus there is an inverted logic
*         compared to other implementations of the sieve algorithm.  Although
*         the table version explicitly sets the values at creation, therefore
*         the logic could revert back standard with no effect on the speed,
*         I have left it the same as the array verion rather than introduce
*         a discrepency between the two
*
*      6. The sieve size is determined by the SieveSize variable (currently
*         set to 1000000
*
*      7. Clipper's array element size is 14 bytes (but this is irrelevant as
*         this version uses tables to hold the values
*
*      8. There is a limitiation of Clipper's maximum table size
*
*      9. The table version will take considerable time to initialise the
*         table by creating it, however the processing time is relatively
*         short
*
*     10. The table version will create a table of approx 8,500,098 bytes for
*         a 1,000,000 sieve size
*
*     11. The table version will also create an index file of approx
*         26,948,608 bytes for a sieve size of 1,000,000
*
*     12. Bits Value : I have used the disk space for each record (17 bytes)
*         However, this can be reduced as the field width has been set to
*         10^14 which is significantly more than is required for 1,000,000
*         I have also not included Clippers Database header overhead in the
*         17 bytes calculation.  Included in the overall is the
*         index for the table.  This works out at about 19 bytes per record.
*         This gives a total size per record of 36 bytes per record.
*        
*
*
*±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*                                        
*   FUNCTIONS AND PROCEDURES
*
*      PROCEDURE Main()
*      FUNCTION ReferenceN()
*      FUNCTION RunSieve()
*      FUNCTION InitTables()
*      PROCEDURE SetTElement()
*      FUNCTION GetTElement()      
*      FUNCTION ADDFIELD()
*      PROCEDURE DInitTable()
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

       ALTD()

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

   OutputString = "AndyRadford,Clip5.2e,Db"
   OutputString = OutputString + ";" + ALLTRIM(STR(PassCount))
   OutputString = OutputString + ";" + ALLTRIM(STR(Duration))
   OutputString = OutputString + ";" + ALLTRIM(STR(1))
   OutputString = OutputString + ";" + ALLTRIM("algorithm=base")
   OutputString = OutputString + "," + ALLTRIM("faithful=yes")
   OutputString = OutputString + "," + ALLTRIM("bits=288")

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

   InitTables(PArray)

   FOR Factor = 3 TO SieveSqrt Step 2

      FOR Number = Factor TO SieveSqrt STEP 2

         IF GetTElement(INT(Number/2)) = .F.
            Factor = Number
            EXIT
         ENDIF

      NEXT Number

      IF Number > SieveSqrt
         EXIT
      ENDIF

      FOR Number = Factor * 3 TO SieveSize STEP Factor * 2
         SetTElement(INT(Number / 2),.T.)
      NEXT

   NEXT

   //Count the number of Primes (inverted logic warning)

   FOR Counter = 1 TO PArray

      IF GetTelement(Counter) = .F.
         ResultCount = ResultCount + 1
      ENDIF

   NEXT

   DInitTable()


RETURN (ResultCount)


*±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*    FUNCTION InitTables()
*±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*
FUNCTION InitTables(ElementCount)

   LOCAL Looop := 0

   IF FILE("SIEVEDB.DBF")
      DELETE FILE("SIEVEDB.DBF")
   ENDIF

   IF FILE("TEMPSTRU.DBF")
      DELETE FILE("TEMPSTRU.DBF")
   ENDIF


   IF FILE("SIEVE.NTX")
      DELETE FILE("SIEVEDB.NTX")
   ENDIF

   CREATE ("TEMPSTRU.DBF")
   ADDFIELD("NUMBER","N",15,0)
   ADDFIELD("VALUE","L",1,0)
   CLOSE TEMPSTRU

   CREATE SIEVEDB.DBF FROM TEMPSTRU.DBF NEW

   USE ("SIEVEDB.DBF") EXCLUSIVE NEW

   SELECT SIEVEDB

   INDEX ON NUMBER TO ("SIEVEDB.NTX")

   SET INDEX TO ("SIEVEDB.NTX")

   FOR Looop = 1 TO ElementCount
      APPEND BLANK
      REPLACE NUMBER WITH Looop
      REPLACE VALUE WITH .F.
   NEXT

RETURN



*±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*    PROCEDURE DInitTable()
*±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*
PROCEDURE DInitTable()

   //Clean up tables and indexes after use

   IF SELECT("SIEVEDB") > 0
      CLOSE SIEVEDB
   ENDIF

   IF FILE("SIEVEDB.DBF")
      DELETE FILE("SIEVEDB.DBF")
   ENDIF

   IF FILE("TEMPSTRU.DBF")
      DELETE FILE("TEMPSTRU.DBF")
   ENDIF

   IF FILE("SIEVE.NTX")
      DELETE FILE("SIEVEDB.NTX")
   ENDIF

RETURN


*±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*    PROCEDURE SetTElement()
*±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*
PROCEDURE SetTElement(Index, NewValue)

  // set the appropriate record to the required value

  SELECT 1
  GO TOP
  SEEK (Index)
  REPLACE VALUE WITH NewValue

RETURN


*±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*    FUNCTION GetTElement()
*±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*
FUNCTION GetTElement(Index)

  // return the value from the appropriate record

  LOCAL lValue := .F.

  SELECT 1
  GO TOP
  SEEK(Index)
  lValue = SIEVEDB->Value


RETURN (lValue)


*±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*    Function ADDFIELD()
*±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*
* Add the given field to the database structure
*
FUNCTION ADDFIELD ( FName , FType , FLen , FDec )

   LOCAL Result := .F.

   LOCATE FOR FIELD_NAME = FName
   IF FOUND()
      REPLACE FIELD_TYPE WITH FType
      REPLACE FIELD_LEN WITH FLen
      REPLACE FIELD_DEC WITH FDec
      Result := .F.
   ELSE
      APPEND BLANK
      REPLACE FIELD_NAME WITH FName
      REPLACE FIELD_TYPE WITH FType
      REPLACE FIELD_LEN WITH FLen
      REPLACE FIELD_DEC WITH FDec
      Result := .T.
   ENDIF

RETURN (Result)



***
* VERSION HISTORY :
*   1.0 : Initial Release (22/09/2021 ACR)
*   1.1 : Fix : Support for ADDFIELD() (22/09/2021 ACR)
*   1.2 : Fix : Remove Table/Indexes after calculations (22/09/2021 ACR)
*
* EoF: SIEVEDB.PRG