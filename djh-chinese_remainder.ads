-- Author:    David Haley
-- Created:   14/12/2020
-- Last Edit: 02/01/2021

-- solves Chinese Remainder where N (i) is the modulus and A (i) is the
-- remainder.

generic

   type Index_Type is range <>;
   type Integer_Type is range <>;

package DJH.Chinese_Remainder is

   subtype Positive_Type is Integer_Type range 1 .. Integer_Type'Last;

   type Rem_Mod_Pair is Record
      A, N : Positive_Type;
   end record; -- Rem_Mod_Pair

   type Rem_Mod_Arrays is array (Index_Type) of Rem_Mod_Pair;

   procedure Solve (Rem_Mod_Array : in Rem_Mod_Arrays;
                    R : out Positive_Type;
                    N : out Positive_Type);

end DJH.Chinese_Remainder;
