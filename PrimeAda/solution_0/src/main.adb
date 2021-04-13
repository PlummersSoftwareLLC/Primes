with Ada.Containers.Ordered_Maps;
with Ada.Text_IO;   use Ada.Text_IO;
with Ada.Numerics.Long_Long_Elementary_Functions;
with Ada.Real_Time; use Ada.Real_Time;
procedure Main is
   package Long_Long_Set is new Ada.Containers.Ordered_Maps
     (Key_Type => Long_Long_Integer, Element_Type => Long_Long_Integer);

   type Bit_List is array (Long_Long_Integer range <>) of Boolean;
   type Prime_Count is array (Natural range <>) of Long_Long_Integer;

   function Create_Bit_List (Size : Long_Long_Integer := 0) return Bit_List is
      Ret : Bit_List (0 .. Size) := (others => True);
   begin
      return Ret;
   end Create_Bit_List;

   Sieve_Size        : Long_Long_Integer    := 1_000_000;
   Bits              : Bit_List             := Create_Bit_List (Sieve_Size);
   Validate_Map      : Long_Long_Set.Map;
   Know_Prime_Counts : constant Prime_Count :=
     (4, 25, 168, 1_229, 9_592, 78_498, 664_579, 5_761_455, 50_847_534,
      455_052_511);

   procedure Fill_Validate_Map is
      Loop_Counter : Natural := 1;
   begin
      for I of Know_Prime_Counts loop
         Validate_Map.Insert (Key => 10**Loop_Counter, New_Item => I);
         Loop_Counter := Loop_Counter + 1;
      end loop;
   end Fill_Validate_Map;

   procedure Run_Sieve is
      use Ada.Numerics.Long_Long_Elementary_Functions;

      Factor : Long_Long_Integer := 3;
      Q      : Long_Long_Integer :=
        Long_Long_Integer
          (Ada.Numerics.Long_Long_Elementary_Functions.Sqrt
             (Long_Long_Float (Sieve_Size)));
      Num : Long_Long_Integer := Factor;
   begin
      while Factor <= Q loop
         while Num < Sieve_Size loop
            if Bits (Num) then
               Factor := Num;
               exit;
            end if;
            Num := Num + 2;
         end loop;
         Num := Factor * Factor;
         while Num < Sieve_Size loop
            Bits (Num) := False;
            Num        := Num + Factor * 2;
         end loop;
         Factor := Factor + 2;
      end loop;
   end Run_Sieve;

   function Count_Primes return Long_Long_Integer is
      Count        : Long_Long_Integer := 1;
      Loop_Counter : Long_Long_Integer := 3;
   begin
      while Loop_Counter < Sieve_Size loop
         if Bits (Loop_Counter) then
            Count := Count + 1;
         end if;
         Loop_Counter := Loop_Counter + 2;
      end loop;
      return Count;
   end Count_Primes;

   function Validate_Results return Boolean is
   begin
      return Validate_Map.Element (Sieve_Size) = Count_Primes;
   end Validate_Results;

   procedure Print_Result
     (Show_Result : Boolean; Dur : Duration; Passes : Integer)
   is
      count : Long_Long_Integer := 1;
      Num   : Long_Long_Integer := 3;
      Avg   : Duration          := Dur / Passes;
   begin
      if Show_Result then
         Put ("2, ");
      end if;
      while Num < Sieve_Size loop
         if Bits (Num) then
            if Show_Result then
               Put (Num'Img & ", ");
            end if;
            count := count + 1;
         end if;
         Num := Num + 2;
      end loop;
      if Show_Result then
         Put_Line ("");
      end if;

      Put_Line
        ("Passes:" & Passes'Image & ", Time:" & Dur'Image & ", Avg: " &
         Avg'Image & ", Limit :" & Sieve_Size'Image & ", Count1 :" &
         count'Image & ", Count2:" & Count_Primes'Image & ", Valid :" &
         Validate_Results'Image);

   end Print_Result;
   Passes     : Integer := 0;
   Time_Start : Time    := Clock;
   Time_End   : Time    := Time_Start + Seconds (5);

begin
   Fill_Validate_Map;
   while Clock < Time_End  loop

      Bits := Create_Bit_List (Sieve_Size);
      Run_Sieve;
      Passes := Passes + 1;

   end loop;
   Print_Result (False, To_Duration (Clock - Time_Start), Passes);
end Main;
