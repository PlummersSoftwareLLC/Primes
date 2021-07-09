with Ada.Text_IO; use Ada.Text_IO;

procedure Bits is
   package IIO is new Integer_IO (Integer);
   use IIO;

   type Boolean_Array is array (1 .. 10) of Boolean;
   BA : constant Boolean_Array := (others => False);

   type Packed_Boolean_Array is array (1 .. 10) of Boolean with
     Pack;
   PBA : constant Packed_Boolean_Array := (others => False);
begin
   Put ("Integer'Size: ");
   Put (Integer'Size, Width => 54);
   New_Line;

   Put ("Long_Integer'Size: ");
   Put (Long_Integer'Size, Width => 49);
   New_Line;

   Put ("Long_Long_Integer'Size: ");
   Put (Long_Long_Integer'Size, Width => 44);
   New_Line;

   Put ("Float'Size: ");
   Put (Float'Size, Width => 56);
   New_Line;

   Put ("Long_Float'Size: ");
   Put (Long_Float'Size, Width => 51);
   New_Line;

   Put ("Boolean'Size: ");
   Put (Boolean'Size, Width => 53);
   New_Line;

   Put ("Boolean_Array (Boolean_Array'First)'Size: ");
   Put (BA (BA'First)'Size, Width => 25);
   New_Line;

   Put ("Packed_Boolean_Array (Packed_Boolean_Array'First)'Size: ");
   Put (PBA (PBA'First)'Size);
   New_Line;

   Put ("Duration'Size: ");
   Put (Duration'Size, Width => 53);
   New_Line;
end Bits;
