with Prime_Sieves;
with Prime_Sieves_Imp;

procedure Sieves is
   type Packed_Boolean_Array_Type is array (Positive range <>) of Boolean with
     Pack;

   type Unpacked_Boolean_Array_Type is array (Positive range <>) of Boolean;

   package Packed_Sieves is new Prime_Sieves
      (Index_Type         => Positive,
       Boolean_Array_Type => Packed_Boolean_Array_Type);

   package Unpacked_Sieves is new Prime_Sieves
      (Index_Type         => Positive,
       Boolean_Array_Type => Unpacked_Boolean_Array_Type);

   package Packed_Sieves_Imp is new Prime_Sieves_Imp
      (Index_Type         => Positive,
       Boolean_Array_Type => Packed_Boolean_Array_Type);

   package Unpacked_Sieves_Imp is new Prime_Sieves_Imp
      (Index_Type         => Positive,
       Boolean_Array_Type => Unpacked_Boolean_Array_Type);
begin
   Packed_Sieves.Generate;
   Unpacked_Sieves.Generate;
   Packed_Sieves_Imp.Generate;
   Unpacked_Sieves_Imp.Generate;
end Sieves;
