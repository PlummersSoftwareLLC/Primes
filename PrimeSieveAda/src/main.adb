with Ada.Unchecked_Deallocation;
with Ada.Text_IO;   use Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Numerics.Long_Long_Elementary_Functions;
use Ada.Numerics.Long_Long_Elementary_Functions;

procedure Main is

   type Int_64 is new Long_Long_Integer;
   type Bit_Array is array (Int_64 range <>) of Boolean;
   type Bit_Array_Access is access all Bit_Array;

   procedure Free is new Ada.Unchecked_Deallocation (Bit_Array, Bit_Array_Access);

   subtype Power_Of_Ten is Natural range 1 .. 10;
   type Results_Array is array (Power_Of_Ten) of Int_64;

   -- Settings

   Test_Duration : constant Time_Span    := Seconds (5);
   Test_Power    : constant Power_Of_Ten := 6;
   Show_Primes   : constant Boolean      := False; 

   -- Constants

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
   Q          : constant Int_64 := Int_64 (Sqrt (Long_Long_Float (Sieve_Size)));

   procedure Run_Sieve (Bits : in Bit_Array_Access; Sieve_Size : in Int_64) is
      use Ada.Numerics.Long_Long_Elementary_Functions;
      Factor : Int_64 := 3;
      Num    : Int_64;
   begin
      while Factor <= Q loop
         Num := Factor;
         Inner : while Num < Sieve_Size loop
            if Bits.all (Num) then
               Factor := Num;
               exit Inner;
            end if;
            Num := Num + 2;
         end loop Inner;
         Num := Factor * Factor;
         while Num < Sieve_Size loop
            Bits.all (Num) := False;
            Num        := Num + Factor * 2;
         end loop;
         Factor := Factor + 2;
      end loop;
   end Run_Sieve;

   function Count_Primes (Bits : in Bit_Array_Access) return Int_64 is
      Count : Int_64 := 1;
   begin
      for I in Bits.all'Range loop
         if 1 = (I mod 2) and Bits.all (I) then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Count_Primes;

   procedure Print_Result (
      Passes   : in Int_64;
      Elapsed  : in Time_Span;
      Limit    : in Int_64;
      Expected : in Int_64;
      Actual   : in Int_64) 
   is
      Elapsed_Duration : Duration := To_Duration (Elapsed);
      Average          : Duration := Elapsed_Duration / Integer (Passes);
   begin
      Put ("Passes:" & Passes'Image & ", ");
      Put ("Time:" & Elapsed_Duration'Image & ", ");
      Put ("Average:" & Average'Image & ", ");
      Put ("Limit:" & Limit'Image & ", ");
      Put ("Expected:" & Expected'Image & ", ");
      Put ("Actual:" & Actual'Image & ", ");
      Put ("Valid: " & Boolean'Image (Expected = Actual));
      New_Line;
   end Print_Result;

   procedure Print_Primes (Bits : in Bit_Array_Access) is begin
      Put ("[ 2");
      for I in Bits.all'Range loop
         if 1 = (I mod 2) and Bits.all (I) then
            Put ("," & I'Image);
         end if;
      end loop;
      Put (" ]");
      New_Line;
   end Print_Primes;

   Passes : Int_64 := 0;
   Bits   : Bit_Array_Access := new Bit_Array(3 .. Sieve_Size);
   Start  : Time := Clock;
   Target : Time := Start + Test_Duration;
   Finish : Time;
begin

   while Clock < Target loop
      Bits.all := (others => True);
      Run_Sieve (Bits, Sieve_Size);
      Passes := Passes + 1;
   end loop;
   Finish := Clock;
   
   Print_Result (Passes, Finish - Start, Sieve_Size, Expected, Count_Primes (Bits));
   if Show_Primes then Print_Primes (Bits); end if;
   Free (Bits);
end Main;
