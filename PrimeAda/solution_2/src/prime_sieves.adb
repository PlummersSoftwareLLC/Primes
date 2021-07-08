with Ada.Calendar;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Text_IO; use Ada.Text_IO;

package body Prime_Sieves is
   package IIO is new Integer_IO (Integer);
   use IIO;

   package Index_IO is new Integer_IO (Index_Type);
   use Index_IO;

   package DIO is new Fixed_IO (Duration);
   use DIO;

   package BIO is new Enumeration_IO (Boolean);
   use BIO;

   use type Result_Maps.Cursor;

   package Float_Ops is new Ada.Numerics.Generic_Elementary_Functions (Float);
   use Float_Ops;

   procedure Run (Sieve : in out Prime_Sieve) is
      Factor : Integer := 3;
      Q      : Integer := Integer (Sqrt (Float (Sieve.Size)));
   begin
      while Factor <= Q loop
         declare
            Number : Integer := Factor;
         begin
            --  Ada's for loops don't have a by keyword like Pascal does.
            Is_Prime : while Index_Type (Number) < Sieve.Size loop
               if Sieve.Bits (Index_Type (Number)) then
                  Factor := Number;

                  exit Is_Prime;
               end if;

               Number := Number + 2;
            end loop Is_Prime;

            --  Probably not a good idea to re-use this loop counter!
            Number := Factor * Factor;

            Is_Not_Prime : while Index_Type (Number) < Sieve.Size loop
               Sieve.Bits (Index_Type (Number)) := False;

               Number := Number + (Factor * 2);
            end loop Is_Not_Prime;
         end;

         Factor := Factor + 2;
      end loop;
   end Run;

   procedure Print_Results
      (Sieve          : Prime_Sieve;
       Total_Duration : Duration;
       Total_Passes   : Integer;
       Verbose        : Boolean := False) is
   begin
      if Verbose then
         Put ("2, ");
      end if;

      declare
         Count  : Integer := (if Sieve.Size >= 2 then 1 else 0);
         Number : Integer := 3;
      begin
         while Index_Type (Number) <= Sieve.Size loop
            if Sieve.Bits (Index_Type (Number)) then
               if Verbose then
                  Put (Number, Width => 0);
               end if;

               Count := Count + 1;
            end if;

            Number := Number + 2;
         end loop;

         if Verbose then
            New_Line;
         end if;

         Put ("Passes: ");
         Put (Total_Passes, Width => 0);
         Put (", Time: ");
         Put (Total_Duration, Fore => 2, Aft => 6);
         Put (", Avg: ");
         Put (Total_Duration / Duration (Total_Passes), Fore => 2);
         Put (", Limit: ");
         Put (Sieve.Size, Width => 0);
         Put (", Count1: ");
         Put (Count, Width => 0);
         Put (", Count2: ");
         Put (Sieve.Count_Primes, Width => 0);
         Put (", Valid: ");
         Put (Sieve.Validate_Results);

         New_Line;

         Put ("Lucretia;");
         Put (Total_Passes, Width => 0);
         Put (";");
         Put (Total_Duration, Fore => 2, Aft => 6);
         Put (";algorithm=base,faithful=yes,bits=");
         IIO.Put (Sieve.Bits (Sieve.Bits'First)'Size, Width => 0);
         New_Line;
      end;
   end Print_Results;

   function Count_Primes (Sieve : Prime_Sieve) return Integer is
      Count : Integer := (if Sieve.Size >= 2 then 1 else 0);
   begin
      declare
         Prime : Integer := 3;
      begin
         while Index_Type (Prime) < Sieve.Size loop
            if Sieve.Bits (Index_Type (Prime)) then
               Count := Count + 1;
            end if;

            Prime := Prime + 2;
         end loop;
      end;

      return Count;
   end Count_Primes;

   function Validate_Results (Sieve : Prime_Sieve) return Boolean is
      Result : Result_Maps.Cursor := Results.Find (Long_Long_Integer (Sieve.Size));
   begin
      if Result = Result_Maps.No_Element then
         return False;
      end if;

      return (Result_Maps.Element (Result) = Sieve.Count_Primes);
   end Validate_Results;

   procedure Generate is
      use Ada.Calendar;

      Seconds_To_Loop  : constant Duration := 5.0;
      Passes_Completed :          Natural  := Natural'First;
      Start_Time       : constant Time     := Clock;
   begin
      Main_Loop : loop
         declare
            Sieve : Prime_Sieve (Size => Sieve_Size);
         begin
            Sieve.Run;

            Passes_Completed := Passes_Completed + 1;

            if Clock - Start_Time >= Seconds_To_Loop then
               Sieve.Print_Results
                  (Verbose        => False,
                  --  TODO: Is this correct??
                  --  The C++ version of this seem to convert it's time from secons to ms and then back to seconds.
                  Total_Duration => Clock - Start_Time,
                  Total_Passes   => Passes_Completed);
               exit Main_Loop;
            end if;
         end;
      end loop Main_Loop;
   end Generate;
begin
   Results.Insert (            10,           4);
   Results.Insert (           100,          25);
   Results.Insert (         1_000,         168);
   Results.Insert (        10_000,       1_229);
   Results.Insert (       100_000,       9_592);
   Results.Insert (     1_000_000,      78_498);
   Results.Insert (    10_000_000,     664_579);
   Results.Insert (   100_000_000,   5_761_455);
   Results.Insert ( 1_000_000_000,  50_847_534);
   Results.Insert (10_000_000_000, 455_052_511);
end Prime_Sieves;
