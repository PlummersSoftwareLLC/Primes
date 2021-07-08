with Prime_Sieves;

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
begin
   Packed_Sieves.Generate;
   Unpacked_Sieves.Generate;
end Sieves;
