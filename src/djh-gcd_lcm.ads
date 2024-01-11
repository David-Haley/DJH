-- Author    : David Haley
-- Created   : 12/01/2024
-- Last Edit : 12/01/2024
-- Algorithm derived from Wikipedia (GCD) and Rsetta Code

generic

   type Numbers is range <>;

package DJH.Gcd_Lcm is

   function Gcd (A, B : in Numbers) return Numbers;

   -- Returns the Least Common Multiple of A and B.
   -- Warning could raise an exception if the positive range of Numbers is
   -- smaller than the negative range.

   function Lcm (A, B : Numbers) return Numbers;

   -- Returns the Least Common Multiple of A and B.
   -- Uses Gcd, see Warning above.

end DJH.Gcd_Lcm;
