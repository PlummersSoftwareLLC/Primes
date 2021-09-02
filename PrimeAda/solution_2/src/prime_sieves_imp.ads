--  Imperative version.
with Ada.Numerics.Generic_Elementary_Functions;

generic
   Loop_Duration : Duration;

   type Bit_Index_Type is range <>;
   type Boolean_Array_Type is array (Bit_Index_Type) of Boolean;
package Prime_Sieves_Imp is
   procedure Generate;
private
   package Long_Float_Ops is new Ada.Numerics.Generic_Elementary_Functions (Long_Float);
   use Long_Float_Ops;

   Sieve_Size :          Bit_Index_Type := Bit_Index_Type'Last;
   Q          : constant Bit_Index_Type := Bit_Index_Type (Sqrt (Long_Float (Sieve_Size)));

   True_Bits : constant Boolean_Array_Type := (others => True);

   type Prime_Sieve is record
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
end Prime_Sieves_Imp;
