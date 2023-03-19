-- Author:    David Haley
-- Created:   14/12/2020
-- Last Edit: 20/12/2020
-- Code taken fron https://rosettacode.org/wiki/Modular_inverse#Ada and made
-- generic to allow for 64 bit integers to be used

package body DJH.Modular_Inverse is

   function Inverse_Mod (A : in Integer_Type; N : in Positive_Type)
                         return Integer_Type with
     REFINED_POST=> (A * Inverse_Mod'Result) mod N = 1 is
      -- To calculate the inverse we do as if we would calculate the GCD with
      -- the Euclid extended algorithm (but we just keep the coefficient on A)

      function Inverse (A, B, U, V : Integer_Type) return Integer_Type is
        (if B = 0 then
            U
         else
            Inverse (B, A mod B, V, U - (V * A) / B));

   begin -- Inverse_Mod
      return inverse (A, N, 1, 0);
   end Inverse_Mod;

end DJH.Modular_Inverse;
