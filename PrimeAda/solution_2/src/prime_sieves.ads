with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Ordered_Maps;

generic
   type Index_Type is range <>;
   type Boolean_Array_Type is array (Index_Type range <>) of Boolean;

   with function "<" (LHS, RHS : Index_Type) return Boolean is <>;
package Prime_Sieves is
   package IIO is new Integer_IO (Integer);
   use IIO;

   package Index_IO is new Integer_IO (Index_Type);
   use Index_IO;

   package DIO is new Fixed_IO (Duration);
   use DIO;

   package BIO is new Enumeration_IO (Boolean);
   use BIO;

   package Result_Maps is new Ada.Containers.Ordered_Maps
      (Key_Type     => Long_Long_Integer,
       Element_Type => Integer);

   Results : Result_Maps.Map;

   -- type Packed_Boolean_Array is array (Positive range <>) of Boolean;-- with
   --   Pack;

   type Prime_Sieve (Size : Index_Type) is tagged record
      Bits : Boolean_Array_Type (Index_Type'First .. Size) := (others => True);
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
