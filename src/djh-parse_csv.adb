-- Package for parsing CSV files. Assumes column headers in the first row which
-- can be read as a super set of the values of Header_Labels. No_Header allows
-- files without header rows to be processed. When using No_Header all columns
-- up to the last column to be read must be named in Header_Lables.

-- Author    : David Haley
-- Created   : 13/07/2020
-- Last Edit : 19/11/2023
-- 20231119: No Header added, to allow reading of files with no header row.
-- 20220519 : Allows for processing of Windows text files with CR in Linux.
-- 20220515 : File interface changed.
-- 20210606: Row_Number added.

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Indefinite_Vectors;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Exceptions; use Ada.Exceptions;

package body DJH.Parse_CSV is

   subtype Column_Numbers is Positive;
   package Column_Lists is new
     Ada.Containers.Indefinite_Vectors (Column_Numbers, String);
   use Column_Lists;

   type Field_Element is record
      Found : Boolean;
      Column : Column_Numbers;
   end record; -- Field_Element
   type Field_Arrays is array (Header_Labels) of Field_Element;

   Input_File : File_Type;

   procedure Find_Columns (Column_List : in out Column_Lists.Vector) is

      -- Finds the bounds of every column in line.

      Delimeter_Set : constant Character_Set := To_Set (',');

      Quote_Set : constant Character_Set := To_Set ('"');

      Start_At : Positive := 1;
      Delimeter, Quote : Natural;
      Text : Unbounded_String;

   begin -- Find_Columns
      Clear (Column_List);
      Get_Line (Input_File, Text);
      if Length (Text) > 0 and then Element (Text, Length (Text)) = CR then
         -- Windows text in Linux, remove CR
         Delete (Text, Length (Text), Length (Text));
      end if; -- Length (Text) > 0 and then Element (Text, Length (Text)) = CR
      while Start_At <= Length (Text) loop
         Delimeter := Index (Text, Delimeter_Set, Start_At);
         Quote := Index (Text, Quote_Set, Start_At);
         if Quote > 0 and then (Quote < Delimeter or
                                Delimeter = 0) then
            -- in quoted text
            Start_At := Quote + 1;
            Quote := Index (Text, Quote_Set, Start_At);
            if Quote = 0 then
               raise CSV_Error with "Closing '""' not found at line" &
               Positive_Count'image (Line (Input_File) - 1);
            end if; -- Quote = 0
            Append (Column_List, Slice (Text, Start_At, Quote - 1));
            Delimeter := Index (Text, Delimeter_Set, Quote);
            if Delimeter = 0 then
               -- end of line
               Start_At := Length (Text) + 1;
            else
               if Delimeter = Length (Text) then
                  -- implied last column is empty
                  Append (Column_List, "");
               end if; -- Delimeter = Length (Text)
               Start_At := Delimeter + 1;
            end if; -- Delimeter = 0
         else
            if Delimeter = 0 then
               Append (Column_List, Slice (Text, Start_At, Length (Text)));
               Start_At := Length (Text) + 1;
            else
               Append (Column_List, Slice (Text, Start_At, Delimeter - 1));
               if Delimeter = Length (Text) then
                  -- implied last column is empty
                  Append (Column_List, "");
               end if; -- Delimeter = Length (Text)
               Start_At := Delimeter + 1;
            end if;
         end if; -- Quote > 0 and then (Quote < Delimeter or ...
      end loop; -- Start_At < Length (Text)
   exception
      when CSV_Error =>
         raise;
      when E : others =>
         raise CSV_Error with "Error in Find_Column " & Exception_Message (E);
   end Find_Columns;

   Column_List : Column_Lists.Vector := Column_Lists.Empty_Vector;

   Field_Array : Field_Arrays;

   procedure Read_Header (CSV_File_Name : String) is

      -- Opens the named file and reads header to identify columns and makes
      -- ready to retrive values from the data rows. Before re-reading the same
      -- file or another file of the same type Close_CSV must be called.

      Header_Label : Header_Labels;

   begin -- Read_Header
      Open (Input_File, In_File, CSV_File_Name);
      Find_Columns (Column_List);
      Field_Array := (others => (False, 1));
      for I in Iterate (Column_List) loop
         begin -- label read
            Header_Label :=
              Header_Labels'Value (Column_List (I));
            if Field_Array (Header_Label).Found then
               raise CSV_Error with "Duplicate header label " &
                 Header_Labels' Image (Header_Label);
            else
               Field_Array (Header_Label).Found := True;
               Field_Array (Header_Label).Column := To_Index (I);
            end if; -- Field_Array (Header_Label).Found
         exception
            when CSV_Error =>
               raise;
            when others =>
               null;
         end; -- label read
      end loop; -- I in Iterate (Column_List)
      for L in Header_Labels loop
         if not Field_Array (L).Found then
            raise CSV_Error with "Missing header label " &
              Header_Labels' Image (L);
         end if; -- not Field_Array (L).Found
      end loop; -- L in Header_Labels
   exception
      when CSV_Error =>
         raise;
      when E : others =>
         raise CSV_Error with "Error in Read_Header " & Exception_Message (E);
   end Read_Header;

   procedure No_Header (CSV_File_Name : String) is
      -- Opens the named, identify columns based on order in Header_Lables and
      -- makes ready to retrive values starting from first row. Before
      -- re-reading the same file or another file of the same type Close_CSV
      -- must be called.

      Field : Field_Element;

   begin -- No_Header
      Open (Input_File, In_File, CSV_File_Name);
      Field :=  (Column => Column_Numbers'First, Found => True);
      for H in Header_Labels loop
         Field_Array (H) := Field;
         Field.Column := Field.Column + 1;
      end loop; -- H in Header_Labels
   end No_Header;

   Row_Count : Natural := 0;

   function Next_Row return Boolean is

      -- Returns True if a new row of data is available.

   begin -- Next_Row
      if End_Of_File (Input_File) then
         return False;
      else
         Find_Columns (Column_List);
         if Is_Empty (Column_List) then
            return False;
         else
            Row_Count := Row_Count + 1;
            return True;
         end if; -- Is_Empty (Column_List)
      end if; -- End_Of_File (Input_File)
   exception
      when E : others =>
         raise CSV_Error with "Error in Next_Row " & Exception_Message (E);
   end Next_Row;

   function Get_Value (Column : in Header_Labels) return String is

      -- Returns the string from the specied column

      Cc : Column_Lists.Cursor :=
        To_Cursor (Column_List, Field_Array (Column).Column);

   begin -- Get_Value
      if Cc /= No_Element then
         return Column_List (Cc);
      else
         raise CSV_Error with "Value for " & Header_Labels'Image (Column) &
           " missing in line " & Positive_Count'Image (Line (Input_File) - 1);
      end if; -- Cc /= No_Element
   end Get_Value;

   function Row_Number return Natural is
   -- Returns 0 before first row read and then the count of times New_Row has
   -- been called and returned a new row.

   begin -- Row_Number
      return Row_Count;
   end Row_Number;

   procedure Close_CSV is

   -- Closes the named file, subsequent calls to Next_Row, Get_Value and
   -- Row_Number may raise exceptions.

   begin -- Close_CSV
      Close (Input_File);
   exception
      when E : others =>
         raise CSV_Error with "Error in Close_CSV " & Exception_Message (E);
   end Close_CSV;

end DJH.Parse_CSV;
