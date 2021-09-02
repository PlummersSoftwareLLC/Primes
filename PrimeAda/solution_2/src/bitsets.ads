with Interfaces;
with System.Storage_Elements;

package Bitsets with Pure is
   package I renames Interfaces;

   subtype Sub_Bitset is I.Unsigned_64;

   type Bitset is array (Long_Integer range <>) of Sub_Bitset with
     Alignment => (128 * 1024) / System.Storage_Elements.Storage_Element'Size;

   procedure Clear (Bits : in out Bitset; Bit : Long_Integer) with
     Inline;

   function Get (Bits : Bitset; Bit : Long_Integer) return Boolean with
     Inline;
end Bitsets;
