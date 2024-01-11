
-- Author    : David Haley
-- Created   : 12/01/2024
-- Last Edit : 12/01/2024
-- Algorithm derived from Wikipedia (GCD) and Rsetta Code

package body DJH.Gcd_Lcm is

   function Gcd (A, B : in Numbers) return Numbers is

      -- Returns the Greatest Common Divisor of A and B.

      M : Numbers := A;
      N : Numbers := B;
      T : Numbers;

      begin -- Gcd
      while N /= 0 loop
         T := M;
         M := N;
         N := T mod N;
      end loop;
      return abs (M);
      -- Wikipedia recommends use of abs if negative arguments are permitted.
      -- Could raise an exception if the positive range of Numbers is smaller
      -- than the negative range.
   end Gcd;

   function Lcm (A, B : Numbers) return Numbers is

   -- Returns the Least Common Multiple of A and B.
   -- Uses Gcd, see Warning.

   begin -- Lcm
      if A = 0 or B = 0 then
         return 0;
      else
         return abs (A) * (abs (B) / Gcd (A, B));
      end if; -- A = 0 or B = 0
   end Lcm;

end DJH.Gcd_Lcm;
