-- Author:    David Haley
-- Created:   14/12/2020
-- Last Edit: 02/01/2021

-- solves Chinese Remainder where N (i) is the modulus and A (i) is the
-- remainder.

package body DJH.Chinese_Remainder is

   procedure Solve (Rem_Mod_Array : in Rem_Mod_Arrays;
                    R : out Positive_Type;
                    N : out Positive_Type) is

      function Inverse (Z, N : in Positive_Type) return Positive_Type is

         I : Positive_Type := Z;

      begin -- Inverse
         while I mod N /= 1 loop
            I := I + Z;
         end loop; -- Inverse
         return I;
      end Inverse;

      Sum : Integer_Type := 0;
      Z : Positive_Type;

   begin -- Solve
      N := 1;
      for I in Index_Type loop
         N := N * Rem_Mod_Array (I).N;
      end loop; -- I in Index_Type
      for I in Index_Type loop
         Z := N / Rem_Mod_Array (I).N;
         Sum := Sum + Rem_Mod_Array (I).A * Inverse (Z, Rem_Mod_Array (I).N);
      end loop; -- I in Index_Type
      R := Sum mod N;
   end Solve;

end DJH.Chinese_Remainder;
