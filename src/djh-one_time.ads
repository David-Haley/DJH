-- Author    : David Haley
-- Created   : 07/12/2020
-- Last Edit : 07/12/2020

package DJH.One_Time is

   function Generate (Pad_Length : in Positive) return String;
   -- Returns an OTP as string of specified length.

   function Encode (Plain_Text : in String;
                    OTP_Text : in String) return String;

   -- Encodes text from Plain_Text using OTP_Text and coded text is returned
   -- to Code_File. If the OTP_Text is shorter than Plain_Text the OTP_Text
   -- will be reused.

   function Decode (Coded_Text : in String;
                    OTP_Text : in String) return String;

   -- Decodes text from Code_Text using OTP_Text and plain text is returned.
   -- If the OTP_Text is shorter than Plain_Text the OTP_Text will be
   -- reused.

end DJH.One_Time;
