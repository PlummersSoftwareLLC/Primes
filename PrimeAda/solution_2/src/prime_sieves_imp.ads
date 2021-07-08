--  Imperative version.
with Ada.Containers.Ordered_Maps;

generic
   type Index_Type is range <>;
   type Boolean_Array_Type is array (Index_Type range <>) of Boolean;

   with function "<" (LHS, RHS : Index_Type) return Boolean is <>;

   Sieve_Size : Index_Type := 1_000_000;
package Prime_Sieves_Imp is
   package Result_Maps is new Ada.Containers.Ordered_Maps
      (Key_Type     => Long_Long_Integer,
       Element_Type => Integer);

   Results : Result_Maps.Map;

   type Prime_Sieve (Size : Index_Type) is record
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
   procedure Generate;
end Prime_Sieves_Imp;
