-- solves Chinese Remainder where N (i) is the modulus and A (i) is the
-- remainder.

-- Author:    David Haley
-- Created:   14/12/2020
-- Last Edit: 02/01/2021
-- 20220325 : Natural_Type added.

package body DJH.Chinese_Remainder is

   procedure Solve (Rem_Mod_Array : in Rem_Mod_Arrays;
                    R : out Natural_Type;
                    N : out Positive_Type) is

      function Inverse (Z, N : in Positive_Type) return Positive_Type is

         I : Positive_Type := Z;

      begin -- Inverse
         while I mod N /= 1 loop
            I := I + Z;
         end loop; -- Inverse
         return I;
      end Inverse;

      Sum : Natural_Type := 0;
      Z : Positive_Type;

   begin -- Solve
      N := 1;
      for I in Rem_Mod_Array'Range loop
         N := N * Rem_Mod_Array (I).N;
      end loop; -- I in Rem_Mod_Array'Range
      for I in Rem_Mod_Array'Range loop
         Z := N / Rem_Mod_Array (I).N;
         Sum := Sum + Rem_Mod_Array (I).A * Inverse (Z, Rem_Mod_Array (I).N);
      end loop; -- I in Rem_Mod_Array'Range
      R := Sum mod N;
   end Solve;

end DJH.Chinese_Remainder;
