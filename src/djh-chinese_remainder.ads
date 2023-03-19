-- solves Chinese Remainder where N (i) is the modulus and A (i) is the
-- remainder.

-- Author :    David Haley
-- Created :   14/12/2020
-- Last Edit : 25/03/2021
-- 20220325 : Natural_Type added and used for A, remainder can be 0!

generic

   type Index_Type is (<>);
   type Natural_Type is range <>;

package DJH.Chinese_Remainder is

   subtype Positive_Type is Natural_Type range 1 .. Natural_Type'Last;

   type Rem_Mod_Pair is Record
      A : Natural_Type;
      N : Positive_Type;
   end record; -- Rem_Mod_Pair

   type Rem_Mod_Arrays is array (Index_Type) of Rem_Mod_Pair;

   procedure Solve (Rem_Mod_Array : in Rem_Mod_Arrays;
                    R : out Natural_Type;
                    N : out Positive_Type);

end DJH.Chinese_Remainder;
