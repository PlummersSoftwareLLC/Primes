with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Ordered_Maps;

package Prime_Sieves is
   package IIO is new Integer_IO (Integer);
   use IIO;

   package DIO is new Fixed_IO (Duration);
   use DIO;

   package BIO is new Enumeration_IO (Boolean);
   use BIO;

   package Result_Maps is new Ada.Containers.Ordered_Maps
      (Key_Type     => Long_Long_Integer,
       Element_Type => Integer);

   Results : Result_Maps.Map;

   type Packed_Boolean_Array is array (Positive range <>) of Boolean with
     Pack;

   type Prime_Sieve (Size : Natural) is tagged record
      Bits : Packed_Boolean_Array (1 .. Size) := (others => True);
   end record;

   procedure Run (Sieve : in out Prime_Sieve);
   procedure Print_Results
      (Sieve          : Prime_Sieve;
       Total_Duration : Duration;
       Total_Passes   : Integer;
       Verbose        : Boolean := False);
   function Count_Primes (Sieve : Prime_Sieve) return Integer;
   function Validate_Results (Sieve : Prime_Sieve) return Boolean;
end Prime_Sieves;
