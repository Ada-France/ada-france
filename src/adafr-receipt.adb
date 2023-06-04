-----------------------------------------------------------------------
--  adafr-receipt -- Receipt generation
--  Copyright (C) 2020-2023 Ada France
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.
-----------------------------------------------------------------------
with Ada.Text_IO;
with Ada.Directories;
with Util.Processes;
with Util.Log.Loggers;
with Util.Encoders.SHA256;
with Util.Encoders.HMAC.SHA256;
with Templates_Parser;
package body Adafr.Receipt is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Adafr.Receipt");

   Separator : constant String := ":";

   --  ------------------------------
   --  Sign the information with the key.
   --  ------------------------------
   procedure Sign (Info : in out Information;
                   Key  : in String) is
      use Util.Encoders;

      Ctx  : Util.Encoders.HMAC.SHA256.Context;
      Hash : Util.Encoders.SHA256.Base64_Digest;
   begin
      HMAC.SHA256.Set_Key (Ctx, Key);
      HMAC.SHA256.Update (Ctx, To_String (Info.Company));
      HMAC.SHA256.Update (Ctx, Separator);
      HMAC.SHA256.Update (Ctx, To_String (Info.First_Name));
      HMAC.SHA256.Update (Ctx, Separator);
      HMAC.SHA256.Update (Ctx, To_String (Info.Last_Name));
      HMAC.SHA256.Update (Ctx, Separator);
      HMAC.SHA256.Update (Ctx, To_String (Info.Address1));
      HMAC.SHA256.Update (Ctx, Separator);
      HMAC.SHA256.Update (Ctx, To_String (Info.Address2));
      HMAC.SHA256.Update (Ctx, Separator);
      HMAC.SHA256.Update (Ctx, To_String (Info.Address3));
      HMAC.SHA256.Update (Ctx, Separator);
      HMAC.SHA256.Update (Ctx, To_String (Info.Postal_Code));
      HMAC.SHA256.Update (Ctx, Separator);
      HMAC.SHA256.Update (Ctx, To_String (Info.City));
      HMAC.SHA256.Update (Ctx, Separator);
      HMAC.SHA256.Update (Ctx, To_String (Info.Country));
      HMAC.SHA256.Update (Ctx, Separator);
      HMAC.SHA256.Update (Ctx, To_String (Info.Amount));
      HMAC.SHA256.Update (Ctx, Separator);
      HMAC.SHA256.Update (Ctx, To_String (Info.Date));
      HMAC.SHA256.Update (Ctx, Separator);
      HMAC.SHA256.Update (Ctx, (if Info.Ada_Europe then "TRUE" else "FALSE"));
      HMAC.SHA256.Finish_Base64 (Ctx, Hash, True);
      Info.Signature := To_Unbounded_String (Hash);
   end Sign;

   --  ------------------------------
   --  Create the ConTeX receipt file by using the template and configuration.
   --  ------------------------------
   procedure Create (Path     : in String;
                     Template : in String;
                     Member   : in Information) is
      Dir     : constant String := Ada.Directories.Containing_Directory (Path);
      Cur_Dir : constant String := Ada.Directories.Current_Directory;
      Set     : Templates_Parser.Translate_Set;
      Output  : Ada.Text_IO.File_Type;
   begin
      Log.Info ("Create receipt for {0} in {1}", Member.Last_Name, Path);

      if not Ada.Directories.Exists (Dir) then
         Ada.Directories.Create_Path (Dir);
      end if;
      Templates_Parser.Insert (Set, Templates_Parser.Assoc ("COMPANY", Member.Company));
      Templates_Parser.Insert (Set, Templates_Parser.Assoc ("FIRST_NAME", Member.First_Name));
      Templates_Parser.Insert (Set, Templates_Parser.Assoc ("LAST_NAME", Member.Last_Name));
      Templates_Parser.Insert (Set, Templates_Parser.Assoc ("ADDRESS1", Member.Address1));
      Templates_Parser.Insert (Set, Templates_Parser.Assoc ("ADDRESS2", Member.Address2));
      Templates_Parser.Insert (Set, Templates_Parser.Assoc ("ADDRESS3", Member.Address3));
      Templates_Parser.Insert (Set, Templates_Parser.Assoc ("POSTAL_CODE", Member.Postal_Code));
      Templates_Parser.Insert (Set, Templates_Parser.Assoc ("CITY", Member.City));
      Templates_Parser.Insert (Set, Templates_Parser.Assoc ("COUNTRY", Member.Country));
      Templates_Parser.Insert (Set, Templates_Parser.Assoc ("AMOUNT", Member.Amount));
      Templates_Parser.Insert (Set, Templates_Parser.Assoc ("DATE", Member.Date));
      Templates_Parser.Insert (Set, Templates_Parser.Assoc ("ADA_EUROPE", Member.Ada_Europe));
      Templates_Parser.Insert (Set, Templates_Parser.Assoc ("SIGNATURE", Member.Signature));
      Templates_Parser.Insert (Set, Templates_Parser.Assoc ("INSTALLDIR", Cur_Dir));

      Ada.Text_IO.Create (Output, Ada.Text_IO.Out_File, Path);
      Ada.Text_IO.Put_Line (Output, Templates_Parser.Parse (Template, Set));
      Ada.Text_IO.Close (Output);
   end Create;

   --  ------------------------------
   --  Generate the receipt PDF file by running 'context --noconsole <path>'.
   --  ------------------------------
   procedure Generate (Path : in String) is
      Proc   : Util.Processes.Process;
      Status : Integer;
      Dir    : constant String := Ada.Directories.Containing_Directory (Path);
      Name   : constant String := Ada.Directories.Simple_Name (Path);
   begin
      Log.Info ("Generate PDF receipt with {0}", Path);

      Util.Processes.Set_Working_Directory (Proc, Dir);
      Util.Processes.Spawn (Proc, "context --noconsole " & Name);
      Util.Processes.Wait (Proc);

      Status := Util.Processes.Get_Exit_Status (Proc);
      if Status /= 0 then
         Log.Error ("Receipt PDF generation failed for {0}:{1}",
                    Path, Integer'Image (Status));
      else
         Log.Debug ("PDF generation succeeded for {0}", Path);
      end if;
   end Generate;

end Adafr.Receipt;
