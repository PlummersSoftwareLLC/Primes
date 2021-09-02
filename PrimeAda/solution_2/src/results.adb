with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO;       use Ada.Text_IO;

package body Results is
   function Find (Size : Long_Integer) return Long_Integer is
   begin
      case Size is
         when 10 =>
            return 4;
         when 100 =>
            return 25;
         when 1_000 =>
            return 168;
         when 10_000 =>
            return 1_229;
         when 100_000 =>
            return 9_592;
         when 1_000_000 =>
            return 78_498;
         when 10_000_000 =>
            return 664_579;
         when 100_000_000 =>
            return 5_761_455;
         when 1_000_000_000 =>
            return 50_847_534;
         when 10_000_000_000 =>
            return 455_052_511;
         when others =>
            return No_Element;
      end case;
   end Find;

   procedure Print (Name           : String;
                    Total_Passes   : Integer;
                    Total_Duration : Duration;
                    Sieve_Size     : Long_Integer;
                    Count_1        : Integer;
                    Count_2        : Long_Integer;
                    Valid          : Boolean;
                    Num_Tasks      : Positive;
                    Bit_Size       : Positive) is

      package DIO is new Fixed_IO (Duration);
      use DIO;

      function Image (Dur : Duration) return String with
        Inline;

      use Ada.Strings;

      function Image (Dur : Duration) return String is
         Result : String (1 .. 9);
      begin
         DIO.Put (To => Result, Item => Dur, Aft => 6);

         return Trim (Result, Both);
      end Image;

      Passes : constant String := Trim (Total_Passes'Image, Both);
      Time   : constant String := Image (Total_Duration);
      S1     : constant String :=
         "Passes: "   & Passes &
         ", Time: "   & Time &
         ", Avg: "    & Image (Total_Duration / Duration (Total_Passes)) &
         ", Limit: "  & Trim (Sieve_Size'Image, Both) &
         ", Count1: " & Trim (Count_1'Image, Both) &
         ", Count2: " & Trim (Count_2'Image, Both) &
         ", Valid: "  & Valid'Image;
      S2     : constant String :=
         "Lucretia - " & Name &
         ";" & Passes &
         ";" & Time &
         ";" & Trim (Num_Tasks'Image, Both) &
         ";algorithm=base,faithful=yes,bits=" & Trim (Bit_Size'Image, Both);
   begin
      New_Line;
      Put_Line (S1);
      Put_Line (S2);
   end Print;
end Results;
