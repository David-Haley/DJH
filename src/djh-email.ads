with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package DJH.Email is

   -- Author    : David Haley
   -- Created   : 05/12/2020
   -- Last Edit : 06/12/2020

   -- Sends email using AWS package and is based on example code;

   procedure Setup (SMTP_Server : in String;
                    From_Name : in String;
                    From_Email : in String;
                    User_Name : in String;
                    Password : in String);

   -- Setup details for subsequent email sends

   procedure Send_Email (Message_File_Name : in String;
                         Attachment_File_Name : in String;
                         To_Name : in String;
                         To_Email : in String;
                         Subject : in String;
                         Status : out Boolean;
                         Status_Report : out Unbounded_String);

   -- Sends an email with body of text taken from text file with name
   -- Message_File_Name and an attachment with Attachment_File_Name. If
   -- successfully sent Result will be True, Status_Report contains error
   -- message if any.

end DJH.Email;
