package Results is
   No_Element : constant Long_Integer := 0;

   function Find (Size : Long_Integer) return Long_Integer with
     Inline;

   procedure Print (Name           : String;
                    Total_Passes   : Integer;
                    Total_Duration : Duration;
                    Sieve_Size     : Long_Integer;
                    Count_1        : Integer;
                    Count_2        : Long_Integer;
                    Valid          : Boolean;
                    Num_Tasks      : Positive;
                    Bit_Size       : Positive) with
     Inline;
end Results;
