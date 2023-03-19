-- Package for parsing CSV files. Assumes column headers in the first row which
-- can be read as a super set of the values of Header_Labels.

-- Author    : David Haley
-- Created   : 13/07/2020
-- Last Edit : 15/05/2022
-- 20210606: Row_Number added.
-- 20220515 : File interface changed.

generic

   type Header_Labels is (<>);

package DJH.Parse_CSV is

   procedure Read_Header (CSV_File_Name : String);
   -- Opens the named file and reads header to identify columns and makes ready
   -- to retrive values from the data rows. Before re-reading the same file or
   -- another file of the same type Close_CSV must be called.

   function Next_Row return Boolean;
   -- Returns True if a new row of data is available.

   function Get_Value (Column : in Header_Labels) return String;
   -- Returns the string from the specied column;

   function Row_Number return Natural;
   -- Returns 0 before first row read and then the count of times New_Row has
   -- been called and returned a new row.

   procedure Close_CSV;
   -- Closes the named file, subsequent calls to Next_Row, Get_Value and
   -- Row_Number may raise exceptions.

   CSV_Error : exception;

end DJH.Parse_CSV;
