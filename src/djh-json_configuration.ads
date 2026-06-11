--  Package provides for retrieving program parameters from a JSON configuration
--  file. All parameters are returned as strings and some or all the parameters
--  may be encripted in the file.

--  Author    : David Haley
--  Created   : 08/06/2026
--  Last Edit : 11/06/2024

generic

   type Field_Labels is (<>);
   File_Name : String;
   with function Encrypted (Field_Label : Field_Labels) return Boolean;

package DJH.JSON_Configuration is

   procedure Read_Configuration;
   --  Reads the named file. Raises JSON_Configuration_Error if the file does
   --  not exist or is not a valid JSON.

   function Configuration_File_Exists return Boolean;
   --  Returns True if the configuration file exists.

   function Get_Value (Field : in Field_Labels) return String;
   --  Returns the string from the specied field. Raises
   --  JSON_Configuration_Error if the field does not exist or is null.

   procedure Set_Value (Field : in Field_Labels;
                        Value : in String);
   -- Sets the value of the specified Field.

   procedure Write_Configuration;
   --  Writes the current configuration to the named file. Raises
   --  JSON_Configuration_Error if the file cannot be written.

   JSON_Configuration_Error : exception;

end DJH.JSON_Configuration;
