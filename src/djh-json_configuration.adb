--  Package provides for retrieving program parameters from a JSON configuration
--  file. All parameters are returned as strings and some or all the parameters
--  may be encripted in the file.

--  Author    : David Haley
--  Created   : 08/06/2026
--  Last Edit : 12/06/2024

--  20260612: Optional indented format in configuration file.

with Ada.Directories; use Ada.Directories;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with GNATCOLL.JSON; use GNATCOLL.JSON;
with DJH.One_Time; use DJH.One_Time;

package body DJH.JSON_Configuration is

   Value_Array : array (Field_Labels) of Unbounded_String :=
     [others => Null_Unbounded_String];

   function Make_Key (Field : in Field_Labels) return String is

      Salt : constant String := Field'Img;
      Key : String (1 .. Salt'Length + File_Name'Length);
      I, F, S : Positive := 1;

   begin -- Make_Key
      while I <= Key'Length loop
         if S <= Salt'Length then
            Key (I) := Salt (S);
            I := @ + 1;
            S := @ + 1;
         end if; -- S <= Salt'Length then
         if F <= File_Name'Length then
            Key (I) := File_Name (F);
            I := @ + 1;
            F := @ + 1;
         end if; -- F <= File_Name'Length
      end loop; -- I <= Key'Length
      return Key;
   end Make_Key;

   procedure Read_Configuration is

      --  Reads the named file. Raises JSON_Configuration_Error if the file does
      --  not exist or is not a valid JSON.

      Result : Read_Result;
      Configuration : JSON_Value;
      Local : JSON_Value; 

   begin -- Read_Configuration
      Result := Read_File (File_Name);
      if Result.Success then
         Configuration := Clone (Result.Value);
      else
         raise Invalid_JSON_Stream with Format_Parsing_Error (Result.Error);
      end if; -- Result.Success;
      for F in Field_Labels loop
         if Has_Field (Configuration, F'Img) then
            Local := Get (Configuration, F'Img);
            if Kind (Local) = JSON_String_Type and not Encrypted (F) then
               declare -- Plain text field
                  Value_F : constant String :=Get (Local);
               begin -- Plain text field
                  Value_Array (F) := To_Unbounded_String (Value_F);
               end; -- Plain text field
            elsif Kind (Local) = JSON_Array_Type and Encrypted (F) then
               if Is_Empty (Local) then
                  Value_Array (F) := Null_Unbounded_String;
               else
                  declare -- encoded text field
                     Local_A : constant JSON_Array := Get (Local);
                     Encoded_Field : String (1 .. Length (Local_A));
                     Char_Int : Integer;
                  begin -- encoded text field
                     for I in Positive range 1 .. Length (Local_A) loop
                        Char_Int := Get (Get (Local_A, I));
                        Encoded_Field (I) := Character'Val (Char_Int);
                     end loop; -- I in Positive range 1 .. Length (Local_A)
                     Value_Array (F) :=
                        To_Unbounded_String (Decode (Encoded_Field,
                                                     Make_Key (F)));
                  end; -- encoded text field
               end if; -- Is_Empty (Local)
            elsif Kind (Local) = JSON_Null_Type then
               Value_Array (F) := Null_Unbounded_String;
            else
               raise JSON_Configuration_Error with "Read " & F'Img &
                 " is not a string, null or array (Encrypted)";
            end if; -- Kind (Local) = JSON_String_Type
         else
            raise JSON_Configuration_Error with "Read " & F'Img &
              "does not exist";
         end if; -- Has_Field (Configuration, F'Img)
      end loop; -- F in Field_Labels
   exception
      when JSON_Configuration_Error =>
         raise;
      when E: others =>
         raise JSON_Configuration_Error with "Read - " &
           Exception_Message (E);
   end Read_Configuration;

   function Configuration_File_Exists return Boolean is

      --  Returns True if the configuration file exists.

      (Exists (File_Name) and then Kind (File_Name) = Ordinary_File);

   function Get_Value (Field : in Field_Labels) return String is

      --  Returns the string from the specied field. Raises
      --  JSON_Configuration_Error if the field does not exist or is null.

   begin -- Get_Value
      if Value_Array (Field) = Null_Unbounded_String then
         raise JSON_Configuration_Error with "Get_Value - " & Field'Img &
           " is undefined";
      else
         return To_String (Value_Array (Field));
      end if; -- Value_Array (Field) = Null_Unbounded_String
   end Get_Value;

   procedure Set_Value (Field : in Field_Labels;
                        Value : in String) is

      -- Sets the value of the specified Field.

   begin -- Set_Value
      Value_Array (Field) := To_Unbounded_String (Value);
   end Set_Value;

   procedure Write_Configuration is

      -- Writes the ccurrent configuration to the named file. Raises
      -- JSON_Configuration_Error if the file cannot be written.

      To_Write : constant JSON_Value := Create_Object;
      Output_File : File_Type;

   begin -- Write_Configuration
      for F in Field_Labels loop
         if Value_Array (F) = Null_Unbounded_String then
            Set_Field (To_Write, F'Img, JSON_Null);
         else
            if Encrypted (F) then
               declare -- Encoded password
                  Key : constant String := Make_Key (F);
                  Encoded : constant String :=
                  Encode (To_String (Value_Array (F)), Key);
                  Field_Array : JSON_Array := Empty_Array;
               begin -- Encoded password
                  for I in Positive range 1 .. Encoded'Length loop
                     Append (Field_Array,
                             Create (Integer (Character'Pos (Encoded (I)))));
                  end loop; -- I in Positive range 1 .. Encoded'Length
                  Set_Field (To_Write, F'Img, Field_Array);
               end; -- Encoded password
            else
               Set_Field (To_Write, F'Img, To_String (Value_Array (F)));
            end if; -- Encrypted (F)
         end if; -- Value_Array (F) = Null_Unbounded_String
      end loop; -- F in Field_Labels
      Create (Output_File, Out_File, File_Name);
      Put (Output_File, Write (To_Write, not Indented));
      Close (Output_File);
   exception
      when E: others =>
         raise JSON_Configuration_Error with "Write - " &
           Exception_Message (E); 
   end  Write_Configuration;

end DJH.JSON_Configuration;
