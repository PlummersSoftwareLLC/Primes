with Prime_Sieves;
with Prime_Sieves_Imp;
with Prime_Sieves_Bitset;
with System.Storage_Elements;

procedure Sieves with CPU => 1 is
   Sieve_Size      : constant Long_Integer := 1_000_000;
   Seconds_To_Loop : constant Duration     := 5.0;

   type Boolean_Array_Range is new Long_Integer range 1 .. Sieve_Size;

   type Packed_Boolean_Array_Type is array (Boolean_Array_Range) of Boolean with
     Alignment => (128 * 1024) / System.Storage_Elements.Storage_Element'Size,
     Pack;

   type Unpacked_Boolean_Array_Type is array (Boolean_Array_Range) of Boolean with
     Alignment => (128 * 1024) / System.Storage_Elements.Storage_Element'Size;

   package Packed_Sieves is new Prime_Sieves
      (Loop_Duration      => Seconds_To_Loop,
       Bit_Index_Type     => Boolean_Array_Range,
       Boolean_Array_Type => Packed_Boolean_Array_Type);

   package Unpacked_Sieves is new Prime_Sieves
      (Loop_Duration      => Seconds_To_Loop,
       Bit_Index_Type     => Boolean_Array_Range,
       Boolean_Array_Type => Unpacked_Boolean_Array_Type);

   package Packed_Sieves_Imp is new Prime_Sieves_Imp
      (Loop_Duration      => Seconds_To_Loop,
       Bit_Index_Type     => Boolean_Array_Range,
       Boolean_Array_Type => Packed_Boolean_Array_Type);

   package Unpacked_Sieves_Imp is new Prime_Sieves_Imp
      (Loop_Duration      => Seconds_To_Loop,
       Bit_Index_Type     => Boolean_Array_Range,
       Boolean_Array_Type => Unpacked_Boolean_Array_Type);

   package Packed_Sieves_Bitset is new Prime_Sieves_Bitset
      (Loop_Duration      => Seconds_To_Loop,
       Sieve_Size         => Sieve_Size);
begin
   Packed_Sieves.Generate;
   Unpacked_Sieves.Generate;
   Packed_Sieves_Imp.Generate;
   Unpacked_Sieves_Imp.Generate;
   Packed_Sieves_Bitset.Generate;
end Sieves;
