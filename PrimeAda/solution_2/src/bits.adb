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
