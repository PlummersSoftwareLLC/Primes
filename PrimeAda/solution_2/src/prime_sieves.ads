--  Object-Oriented version.
with Ada.Containers.Ordered_Maps;
with Ada.Numerics.Generic_Elementary_Functions;

generic
   type Bit_Index_Type is range <>;
   type Boolean_Array_Type is array (Bit_Index_Type) of Boolean;

   -- with function "<" (LHS, RHS : Bit_Index_Type) return Boolean is <>;
package Prime_Sieves is
   package Long_Float_Ops is new Ada.Numerics.Generic_Elementary_Functions (Long_Float);
   use Long_Float_Ops;

   Sieve_Size :          Bit_Index_Type := Bit_Index_Type'Last;
   Q          : constant Bit_Index_Type := Bit_Index_Type (Sqrt (Long_Float (Sieve_Size)));

   package Result_Maps is new Ada.Containers.Ordered_Maps
      (Key_Type     => Long_Integer,
       Element_Type => Long_Integer);

   Results   :          Result_Maps.Map;
   True_Bits : constant Boolean_Array_Type := (others => True);

   type Prime_Sieve is tagged record
      Bits : Boolean_Array_Type := True_Bits;
   end record with
     Preelaborable_Initialization;

   procedure Run (Sieve : in out Prime_Sieve);
   procedure Print_Results
      (Sieve          : Prime_Sieve;
       Total_Duration : Duration;
       Total_Passes   : Integer;
       Verbose        : Boolean := False);
   function Count_Primes (Sieve : Prime_Sieve) return Long_Integer with
     Inline;
   function Validate_Results (Sieve : Prime_Sieve) return Boolean with
     Inline;
   procedure Generate;
end Prime_Sieves;
