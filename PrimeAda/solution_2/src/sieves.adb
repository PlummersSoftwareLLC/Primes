with Ada.Calendar;
with Prime_Sieves;

procedure Sieves is
   package Cal renames Ada.Calendar;
   package PS renames Prime_Sieves;

   use type Cal.Time;

   Seconds_To_Loop  : constant Duration := 5.0;
   Sieve_Size       : constant          := 1_000_000;
   Passes_Completed :          Natural  := Natural'First;
   Start_Time       : constant Cal.Time := Cal.Clock;
begin
   Main_Loop : loop
      declare
         Sieve : PS.Prime_Sieve (Size => Sieve_Size);
      begin
         Sieve.Run;

         Passes_Completed := Passes_Completed + 1;

         if Cal.Clock - Start_Time >= Seconds_To_Loop then
            Sieve.Print_Results
               (Verbose        => False,
                --  The C++ version of this seem to convert it's time from secons to ms and then back to seconds.
                Total_Duration => Cal.Clock - Start_Time,
                Total_Passes   => Passes_Completed);
            exit Main_Loop;
         end if;
      end;
   end loop Main_Loop;

   --  Put_Line (Duration (Cal.Clock - Start_Time)'Image);
end Sieves;
