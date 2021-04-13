with Ada.Text_IO;   use Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Numerics.Long_Long_Elementary_Functions;

procedure Main is

   type Int_64 is new Long_Long_Integer;
   type Bit_Array is array (Int_64 range <>) of Boolean;

   subtype Power_Of_Ten is Natural range 1 .. 10;
   type Results_Array is array (Power_Of_Ten) of Int_64;

   -- Settings

   Test_Duration : constant Time_Span    := Seconds (5);
   Test_Power    : constant Power_Of_Ten := 6;
   Show_Primes   : constant Boolean      := False; 

   procedure Run_Sieve (Bits : in out Bit_Array; Sieve_Size : in Int_64) is
      use Ada.Numerics.Long_Long_Elementary_Functions;
      Factor : Int_64 := 3;
      Q      : Int_64 := Int_64 (Sqrt (Long_Long_Float (Sieve_Size)));
      Num    : Int_64;
   begin
      while Factor <= Q loop
         Num := Factor;
         Inner : while Num < Sieve_Size loop
            if Bits (Num) then
               Factor := Num;
               exit Inner;
            end if;
            Num := Num + 2;
         end loop Inner;
         Num := Factor * Factor;
         while Num < Sieve_Size loop
            Bits (Num) := False;
            Num        := Num + Factor * 2;
         end loop;
         Factor := Factor + 2;
      end loop;
   end Run_Sieve;

   function Count_Primes (Bits : in Bit_Array) return Int_64 is
      Count : Int_64 := 1;
   begin
      for I in Bits'Range loop
         if 1 = (I mod 2) and Bits (I) then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Count_Primes;

   procedure Print_Result (
      Bits     : in Bit_Array; 
      Passes   : in Int_64;
      Limit    : in Int_64;
      Elapsed  : in Time_Span; 
      Expected : in Int_64) 
   is
      Average : Duration := To_Duration (Elapsed) / Integer (Passes);
   begin
      Put_Line ("Limit   :" & Limit'Image);
      Put_Line ("Time    :" & Duration'Image (To_Duration (Elapsed)));
      Put_Line ("Passes  :" & Passes'Image);
      Put_Line ("Average :" & Duration'Image (Average));
      Put_Line ("Expected:" & Expected'Image);
      Put_Line ("Actual  :" & Int_64'Image (Count_Primes (Bits)));
   end Print_Result;

   procedure Print_Primes (Bits : in Bit_Array) is begin
      Put ("[ 2");
      for I in Bits'Range loop
         if 1 = (I mod 2) and Bits (I) then
            Put ("," & I'Image);
         end if;
      end loop;
      Put (" ]");
      New_Line;
   end Print_Primes;

   Expected_Array : constant Results_Array := (
       1 => 4,            -- 10 **  1 =>             10
       2 => 25,           -- 10 **  2 =>            100
       3 => 168,          -- 10 **  3 =>          1_000
       4 => 1_229,        -- 10 **  4 =>         10_000
       5 => 9_592,        -- 10 **  5 =>        100_000
       6 => 78_498,       -- 10 **  6 =>      1_000_000
       7 => 664_579,      -- 10 **  7 =>     10_000_000
       8 => 5_761_455,    -- 10 **  8 =>    100_000_000
       9 => 50_847_534,   -- 10 **  9 =>  1_000_000_000
      10 => 455_052_511); -- 10 ** 10 => 10_000_000_000
   Sieve_Size : constant Int_64 := 10 ** Test_Power;
   Expected   : constant Int_64 := Expected_Array (Test_Power);

   Passes   : Int_64 := 0;
   Bits     : Bit_Array (3 .. Sieve_Size);
   Start    : Time := Clock;
   Finish   : Time := Start + Test_Duration;
begin

   while Clock < Finish loop
      Bits := (others => True);
      Run_Sieve (Bits, Sieve_Size);
      Passes := Passes + 1;
   end loop;
   
   Print_Result (Bits, Passes, Sieve_Size, Test_Duration, Expected);
   if Show_Primes then Print_Primes (Bits); end if;
end Main;
