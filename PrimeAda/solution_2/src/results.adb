package body Results is
   function Find (Size : Long_Integer) return Long_Integer is
   begin
      case Size is
         when 10 =>
            return 4;
         when 100 =>
            return 25;
         when 1_000 =>
            return 168;
         when 10_000 =>
            return 1_229;
         when 100_000 =>
            return 9_592;
         when 1_000_000 =>
            return 78_498;
         when 10_000_000 =>
            return 664_579;
         when 100_000_000 =>
            return 5_761_455;
         when 1_000_000_000 =>
            return 50_847_534;
         when 10_000_000_000 =>
            return 455_052_511;
         when others =>
            return No_Element;
      end case;

      return No_Element; --  Won't ever get here.
   end Find;
end Results;
