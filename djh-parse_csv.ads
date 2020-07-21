-- Author    : David Haley
-- Created   : 13/07/2020
-- Last Edit : 14/07/2020

-- Package for parsing CSV files. Assumes column headers in the first row which
-- can be read as a super set of the values of Header_Labels.

with Ada.Text_IO; use Ada.Text_IO;

generic

   type Header_Labels is (<>);

package DJH.Parse_CSV is

   Input_File : File_Type;

   procedure Read_Header;
   -- Reads header to identify columns and makes ready to retrive values from
   -- the data rows.

   function Next_Row return Boolean;
   -- Returns True if a new row of data is available.

   function Get_Value (Column : in Header_Labels) return String;
   -- Returns the string from the specied column;

   CSV_Error : exception;

end DJH.Parse_CSV;
