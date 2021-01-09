-- Author:    David Haley
-- Created:   14/12/2020
-- Last Edit: 20/12/2020
-- Code taken fron https://rosettacode.org/wiki/Modular_inverse#Ada and made
-- generic to allow for 64 bit integers to be used

generic

   type Integer_Type is range <>;

package DJH.Modular_Inverse is

   subtype Positive_Type is Integer_Type range 1 .. Integer_Type'Last;

   function Inverse_Mod (A : in Integer_Type; N : in Positive_Type)
                     return Integer_Type with
     post=> (A * Inverse_Mod'Result) mod N = 1;
      -- To calculate the inverse we do as if we would calculate the GCD with
      -- the Euclid extended algorithm (but we just keep the coefficient on A)

end DJH.Modular_Inverse;
