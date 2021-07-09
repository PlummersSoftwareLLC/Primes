with Prime_Sieves;
with Prime_Sieves_Imp;

procedure Sieves is
   Sieve_Size : Positive := 1_000_000;

   type Boolean_Array_Range is new Positive range 1 .. Sieve_Size;

   type Packed_Boolean_Array_Type is array (Boolean_Array_Range) of Boolean with
     Pack;

   type Unpacked_Boolean_Array_Type is array (Boolean_Array_Range) of Boolean;

   package Packed_Sieves is new Prime_Sieves
      (Bit_Index_Type     => Boolean_Array_Range,
       Boolean_Array_Type => Packed_Boolean_Array_Type);

   package Unpacked_Sieves is new Prime_Sieves
      (Bit_Index_Type     => Boolean_Array_Range,
       Boolean_Array_Type => Unpacked_Boolean_Array_Type);

   package Packed_Sieves_Imp is new Prime_Sieves_Imp
      (Bit_Index_Type     => Boolean_Array_Range,
       Boolean_Array_Type => Packed_Boolean_Array_Type);

   package Unpacked_Sieves_Imp is new Prime_Sieves_Imp
      (Bit_Index_Type     => Boolean_Array_Range,
       Boolean_Array_Type => Unpacked_Boolean_Array_Type);
begin
   Packed_Sieves.Generate;
   Unpacked_Sieves.Generate;
   Packed_Sieves_Imp.Generate;
   Unpacked_Sieves_Imp.Generate;
end Sieves;
