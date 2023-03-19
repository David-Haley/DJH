with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Exceptions; use Ada.Exceptions;
with AWS.Attachments;
with AWS.MIME;
with AWS.SMTP.Client;
with AWS.SMTP.Authentication.Plain;

use AWS;

package body DJH.Email is

   -- Sends email using AWS package and is based on example code;

   -- Author    : David Haley
   -- Created   : 05/12/2020
   -- Last Edit : 01/02/2021
   -- 20210201: The sender email was not correctly transferred to Details;
   -- 20210117: Message file explicetly closed after the file is read, prevents
   -- file sharing exception.

   type Details is record
      Initalised : Boolean := False;
      SMTP_Server : Unbounded_String := Null_Unbounded_String;
      From_Name : Unbounded_String := Null_Unbounded_String;
      From_Email : Unbounded_String := Null_Unbounded_String;
      User_Name : Unbounded_String := Null_Unbounded_String;
      Password : Unbounded_String := Null_Unbounded_String;
   end record; -- Details

   Detail : Details;

   procedure Setup (SMTP_Server : in String;
                    From_Name : in String;
                    From_Email : in String;
                    User_Name : in String;
                    Password : in String) is

      -- Setup details for subsequent email sends

   begin -- Setup
      Detail.Initalised := True;
      Detail.SMTP_Server := To_Unbounded_String (SMTP_Server);
      Detail.From_Name := To_Unbounded_String (From_Name);
      Detail.From_Email := To_Unbounded_String (From_Email);
      Detail.User_Name := To_Unbounded_String (User_Name);
      Detail.Password := To_Unbounded_String (Password);
   end;

   procedure Send_Email (Message_File_Name : in String;
                         Attachment_File_Name : in String;
                         To_Name : in String;
                         To_Email : in String;
                         Subject : in String;
                         Status : out Boolean;
                         Status_Report : out Unbounded_String) is

      -- Sends an email with body of text taken from text file with name
      -- Message_File_Name and an attachment with Attachment_File_Name. If
      -- successfully sent Result will be True, Status_Report contains error
      -- message if any.

      Auth : aliased constant SMTP.Authentication.Plain.Credential :=
        SMTP.Authentication.Plain.Initialize (To_String (Detail.User_Name),
                                              To_String (Detail.Password));

      --  For authentication pass Auth to the server initialization below

      Server : SMTP.Receiver :=
        SMTP.Client.Initialize (To_String (Detail.SMTP_Server));
      --  For authenticating SMTP server pass :
      --  Credential => Auth'Unchecked_Access

      Stat : SMTP.Status;
      Message_File : File_Type;
      Message : Unbounded_String := To_Unbounded_String (To_Name & ',' & LF);
      Text : Unbounded_String;

   begin -- Send_Email
      if Detail.Initalised then
         Open (Message_File, In_File, Message_File_Name);
         while not End_Of_File (Message_File) loop
            Get_Line (Message_File, Text);
            Message := Message & Text & LF;
         end loop; -- not End_Of_File (Message_File)
         Close (Message_File);
         Message := Message & LF & "Regards" & LF & LF & Detail.From_Name & LF;
         SMTP.Client.Send
           (Server,
            From    => SMTP.E_Mail (To_String (Detail.From_Name),
              To_String (Detail.From_Email)),
            To      => (1 => SMTP.E_Mail (To_Name, To_Email)),
            Subject => Subject,
            Message => To_String (Message),
            Attachments => (1 => SMTP.Client.File (Attachment_File_Name)),
            Status  => Stat);
         Status := SMTP.Is_Ok (Stat);
         Status_Report := To_Unbounded_String (SMTP.Status_Message (Stat));
      else
         Status := False;
         Status_Report := To_Unbounded_String ("Details not initalised");
      end if; -- Detail.Initalised
   exception
      when E : others =>
         Status := False;
         Status_Report := To_Unbounded_String ("Exception Message: " &
                                                 Exception_Message(E));
   end Send_Email;

 end DJH.Email;
