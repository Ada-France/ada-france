-----------------------------------------------------------------------
--  adafr-members-modules -- Module members
--  Copyright (C) 2020, 2021, 2022 Stephane Carrez
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

with Ada.Calendar;
with Ada.Strings.Fixed;
with AWA.Modules.Beans;
with AWA.Modules.Get;
with AWA.Users.Models;
with Util.Log.Loggers;
with Util.Strings;
with Util.Files;
with Util.Beans.Objects;
with Util.Beans.Basic;
with Util.Encoders.HMAC.SHA256;
with Util.Dates.Formats;
with Adafr.Members.Beans;
with ADO.Sessions;
with ADO.Queries;
with ADO.Objects;
with ADO.Statements;
with ASF.Locales;
with AWA.Permissions;
with AWA.Services.Contexts;
with Adafr.Receipt;
package body Adafr.Members.Modules is

   use Ada.Strings.Unbounded;
   use Util.Encoders.HMAC.SHA256;
   use type Adafr.Members.Models.Status_Type;
   use type ADO.Identifier;

   package ASC renames AWA.Services.Contexts;

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Adafr.Members.Module");

   package Register_Bean is
     new AWA.Modules.Beans (Module => Member_Module,
                            Module_Access => Member_Module_Access);

   function Get_Next_Receipt_Id (DB : in ADO.Sessions.Session'Class) return ADO.Identifier;

   --  ------------------------------
   --  Job worker procedure to build the receipt and send it by e-mail.
   --  ------------------------------
   procedure Receipt_Worker (Job : in out AWA.Jobs.Services.Abstract_Job_Type'Class) is
      Module : constant Member_Module_Access := Get_Member_Module;
   begin
      Module.Do_Receipt_Job (Job);
   end Receipt_Worker;

   --  ------------------------------
   --  Initialize the members module.
   --  ------------------------------
   overriding
   procedure Initialize (Plugin : in out Member_Module;
                         App    : in AWA.Modules.Application_Access;
                         Props  : in ASF.Applications.Config) is
   begin
      Log.Info ("Initializing the members module");

      --  Register here any bean class, servlet, filter.
      Register_Bean.Register (Plugin => Plugin,
                              Name   => "Adafr.Members.Beans.Member_Bean",
                              Handler => Adafr.Members.Beans.Create_Member_Bean'Access);
      Register_Bean.Register (Plugin => Plugin,
                              Name   => "Adafr.Members.Beans.Member_List_Bean",
                              Handler => Adafr.Members.Beans.Create_Member_List_Bean'Access);

      App.Add_Servlet ("member-export", Plugin.Excel_Servlet'Unchecked_Access);

      AWA.Modules.Module (Plugin).Initialize (App, Props);
   end Initialize;

   --  ------------------------------
   --  Configures the module after its initialization and after having read
   --  its XML configuration.
   --  ------------------------------
   overriding
   procedure Configure (Plugin : in out Member_Module;
                        Props  : in ASF.Applications.Config) is
      use type AWA.Jobs.Modules.Job_Module_Access;
   begin
      Plugin.Sign_Key := Props.Get (PARAM_SIGN_KEY);
      Plugin.Receipt_Template := Props.Get (PARAM_RECEIPT_TEMPLATE);
      Plugin.Receipt_Directory := Props.Get (PARAM_RECEIPT_DIRECTORY);
      Plugin.Receipt_Sign_Key := Props.Get (PARAM_RECEIPT_KEY);
      Plugin.Job_Module := AWA.Jobs.Modules.Get_Job_Module;
      if Plugin.Job_Module = null then
         Log.Error ("Cannot find the AWA Job module for the receipt generation");
      else
         Plugin.Job_Module.Register (Definition => Receipt_Job_Definition.Factory);
      end if;
   end Configure;

   --  ------------------------------
   --  Get the members module.
   --  ------------------------------
   function Get_Member_Module return Member_Module_Access is
      function Get is new AWA.Modules.Get (Member_Module, Member_Module_Access, NAME);
   begin
      return Get;
   end Get_Member_Module;

   function Get_Secure_Key (Model  : in Member_Module;
                            Member : in Adafr.Members.Models.Member_Ref'Class)
                            return String is
      Ident : constant String := Util.Strings.Image (Integer (Member.Get_Id));
      Key   : constant String := To_String (Model.Sign_Key);
   begin
      return Ident & '.' & Sign_Base64 (Key  => Key,
                                        Data => Ident & Member.Get_Salt,
                                        URL  => True);
   end Get_Secure_Key;

   function Validate_Secure_Key (Model  : in Member_Module;
                                 Value  : in String;
                                 Member : in Adafr.Members.Models.Member_Ref'Class)
                                 return Boolean is
      V     : constant String := Ada.Strings.Fixed.Trim (Value, Ada.Strings.Both);
      Pos   : constant Natural := Util.Strings.Index (V, '.');
      Ident : constant String := V (V'First .. Pos - 1);
      Key   : constant String := To_String (Model.Sign_Key);
      Sign  : constant String := Sign_Base64 (Key  => Key,
                                              Data => Ident & Member.Get_Salt,
                                              URL  => True);
   begin
      return V (Pos + 1 .. V'Last) = Sign;
   end Validate_Secure_Key;

   --  ------------------------------
   --  Send_Mail_Validation
   --  ------------------------------
   procedure Send_Mail_Validation (Model  : in out Member_Module;
                                   Email  : in String;
                                   Entity : in out Adafr.Members.Models.Member_Ref'Class) is
      Ctx   : constant ASC.Service_Context_Access := ASC.Current;
      Salt  : constant String := Model.Random.Generate (256);
      DB    : ADO.Sessions.Master_Session := ASC.Get_Master_Session (Ctx);
      Query : ADO.Queries.Context;
      Found : Boolean;
      Mail  : AWA.Users.Models.Email_Ref;
      Event : AWA.Events.Module_Event;
      Ada_Europe : constant Boolean := Entity.Get_Ada_Europe;
   begin
      Log.Info ("Send mail validation to {0}", Email);

      Ctx.Start;

      --  Find existing member on the email address ignoring the case.
      Query.Set_Join ("INNER JOIN awa_email e ON e.id = o.email_id");
      Query.Set_Filter ("LOWER(e.email) = LOWER(?)");
      Query.Bind_Param (1, Email);
      Entity.Find (DB, Query, Found);
      if not Found then

         --  Look for the email only (not yet a member).
         Query.Clear;
         Query.Set_Filter ("LOWER(o.email) = LOWER(?)");
         Query.Bind_Param (1, Email);
         Mail.Find (DB, Query, Found);
         if not Found then
            Mail.Set_Email (Email);
            Mail.Set_User_Id (ADO.NO_IDENTIFIER);
            Mail.Save (DB);
         end if;

         --  Setup this new member with the email we found or we just created.
         Entity.Set_Email (Mail);
         Entity.Set_Status (Adafr.Members.Models.PENDING);
         Entity.Set_Create_Date (Ada.Calendar.Clock);
         Entity.Set_Country ("France");
         Entity.Set_Ada_Europe (Ada_Europe);
         Entity.Set_Amount (65);
      end if;
      Entity.Set_Salt (Salt);
      Entity.Save (DB);

      --  Send the email with the secure key.
      Event.Set_Event_Kind (Send_Validate_Event.Kind);
      Event.Set_Parameter ("email", Email);
      Event.Set_Parameter ("name", Email);
      Event.Set_Parameter ("secure_key", Model.Get_Secure_Key (Entity));
      Model.Send_Event (Event);
      Ctx.Commit;
   end Send_Mail_Validation;

   --  ------------------------------
   --  Validate the secure key
   --  ------------------------------
   procedure Validate_Key (Model  : in out Member_Module;
                           Key    : in String;
                           Entity : in out Adafr.Members.Models.Member_Ref'Class) is
      Ctx   : constant ASC.Service_Context_Access := ASC.Current;
      DB    : ADO.Sessions.Master_Session := ASC.Get_Master_Session (Ctx);
      Pos   : Natural;
      Id    : ADO.Identifier;
   begin
      Log.Info ("Validate the secure key {0}", Key);

      Pos := Util.Strings.Index (Key, '.');
      if Pos = 0 then
         Log.Warn ("Invalid member secure key {0}", Key);
         raise ADO.Objects.NOT_FOUND;
      end if;

      --  Load the member first so that we get its own salt.
      Id := ADO.Identifier'Value (Key (Key'First .. Pos - 1));
      Entity.Load (DB, Id);

      --  Verify the secure key and reject if it does not match.
      if not Model.Validate_Secure_Key (Key, Entity) then
         Log.Warn ("Invalid member secure key {0}", Key);
         raise ADO.Objects.NOT_FOUND;
      end if;

      --  Update the mail verification date the first time.
      if Entity.Get_Mail_Verify_Date.Is_Null then
         Ctx.Start;
         Entity.Set_Mail_Verify_Date ((Is_Null => False,
                                       Value => Ada.Calendar.Clock));
         Entity.Save (DB);
         Ctx.Commit;
      end if;

   exception
      when Constraint_Error =>
         Log.Warn ("Invalid member secure key {0}", Key);
         raise ADO.Objects.NOT_FOUND;
   end Validate_Key;

   --  ------------------------------
   --  Load the member information.
   --  ------------------------------
   procedure Load (Model   : in out Member_Module;
                   Id      : in ADO.Identifier;
                   Entity  : in out Adafr.Members.Models.Member_Ref'Class;
                   History : in out Adafr.Members.Models.Audit_Info_List_Bean) is
      pragma Unreferenced (Model);

      Ctx   : constant ASC.Service_Context_Access := ASC.Current;
      DB    : ADO.Sessions.Master_Session := ASC.Get_Master_Session (Ctx);
      Query : ADO.Queries.Context;
   begin
      --  Check that the user has the view member permission on the given member.
      AWA.Permissions.Check (Permission => ACL_View_Member.Permission,
                             Entity     => Id);

      Entity.Load (DB, Id);

      Query.Set_Query (Adafr.Members.Models.Query_Adafr_Member_History);
      Query.Bind_Param ("member_id", Id);
      Adafr.Members.Models.List (History, DB, Query);
   end Load;

   --  ------------------------------
   --  Save the member information after validating the secure key
   --  ------------------------------
   procedure Save (Model    : in out Member_Module;
                   Id       : in ADO.Identifier;
                   Key      : in String;
                   Inactive : in Util.Nullables.Nullable_Boolean;
                   Member   : in out Adafr.Members.Models.Member_Bean'Class) is
      Ctx   : constant ASC.Service_Context_Access := ASC.Current;
      DB    : ADO.Sessions.Master_Session := ASC.Get_Master_Session (Ctx);
      Current_Entity : Adafr.Members.Models.Member_Ref;
   begin
      if Id > 0 then
         --  Check that the user has the update member permission on the given member.
         AWA.Permissions.Check (Permission => ACL_Update_Member.Permission,
                                Entity     => Id);

         Current_Entity.Load (DB, Id);
      else
         Model.Validate_Key (Key, Current_Entity);
      end if;
      Current_Entity.Set_First_Name (Unbounded_String '(Member.Get_First_Name));
      Current_Entity.Set_Last_Name (Unbounded_String '(Member.Get_Last_Name));
      Current_Entity.Set_Company (Unbounded_String '(Member.Get_Company));
      Current_Entity.Set_Address1 (Unbounded_String '(Member.Get_Address1));
      Current_Entity.Set_Address2 (Unbounded_String '(Member.Get_Address2));
      Current_Entity.Set_Address3 (Unbounded_String '(Member.Get_Address3));
      Current_Entity.Set_Country (Unbounded_String '(Member.Get_Country));
      Current_Entity.Set_City (Unbounded_String '(Member.Get_City));
      Current_Entity.Set_Postal_Code (Unbounded_String '(Member.Get_Postal_Code));
      if not Inactive.Is_Null then
         if Inactive.Value then
            Current_Entity.Set_Status (Models.INACTIVE);
         else
            Current_Entity.Set_Status (Models.WAITING_PAYMENT);
         end if;
      end if;
      if not Current_Entity.Is_Modified then
         return;
      end if;

      if Id > 0 then
         Log.Info ("Save member with id {0}", ADO.Identifier'Image (Id));
      else
         Log.Info ("Save member with secure key {0}", Key);
      end if;
      Ctx.Start;
      Current_Entity.Set_Update_Date (Ada.Calendar.Clock);
      Current_Entity.Save (DB);
      Ctx.Commit;
   end Save;

   --  ------------------------------
   --  Register the member for the Ada-France or Ada-France+Ada-Europe
   --  after validating the secure key
   --  ------------------------------
   procedure Register (Model  : in out Member_Module;
                       Member : in out Adafr.Members.Models.Member_Bean'Class) is
      Key   : constant String := To_String (Member.Key);
      Ctx   : constant ASC.Service_Context_Access := ASC.Current;
      DB    : ADO.Sessions.Master_Session := ASC.Get_Master_Session (Ctx);
      Current_Entity : aliased Adafr.Members.Models.Member_Ref;
   begin
      Log.Info ("Register member with key {0}", Key);

      Model.Validate_Key (Key, Current_Entity);
      Current_Entity.Set_Ada_Europe (Member.Get_Ada_Europe);
      if not Member.Get_Ada_Europe then
         Current_Entity.Set_Amount (30);
      else
         Current_Entity.Set_Amount (65);
      end if;
      if Current_Entity.Get_Status = Models.PENDING or else Is_Expired (Member) then
         Current_Entity.Set_Status (Models.WAITING_PAYMENT);
      end if;

      --  Avoid sending the email again if there is no change.
      if not Current_Entity.Is_Modified then
         return;
      end if;

      --  Save the member information and send the event to trigger the emails.
      Ctx.Start;
      Current_Entity.Set_Update_Date (Ada.Calendar.Clock);
      Current_Entity.Save (DB);
      Ctx.Commit;

      declare
         Ptr   : constant Util.Beans.Basic.Readonly_Bean_Access
           := Current_Entity'Unchecked_Access;
         Bean  : constant Util.Beans.Objects.Object
           := Util.Beans.Objects.To_Object (Ptr, Util.Beans.Objects.STATIC);
         Email : constant AWA.Users.Models.Email_Ref'Class := Current_Entity.Get_Email;
         Event : AWA.Events.Module_Event;
      begin
         if Current_Entity.Get_Status = Models.WAITING_PAYMENT then
            --  First event to send the subscription email to the user.
            Event.Set_Event_Kind (Send_Subscribed_Event.Kind);
            Event.Set_Parameter ("email", Email.Get_Email);
            Event.Set_Parameter ("name", Email.Get_Email);
            Event.Set_Parameter ("member", Bean);
            Model.Send_Event (Event);

            --  Second event to send Ada-France admin an email.
            Event.Set_Event_Kind (Send_Pending_Member_Event.Kind);
            Model.Send_Event (Event);
         else
            --  Send the updated information mail.
            Event.Set_Event_Kind (Send_Updated_Member_Event.Kind);
            Event.Set_Parameter ("email", Email.Get_Email);
            Event.Set_Parameter ("name", Email.Get_Email);
            Event.Set_Parameter ("member", Bean);
            Model.Send_Event (Event);
         end if;
      end;
   end Register;

   function Is_Expired (Member : in Adafr.Members.Models.Member_Ref'Class) return Boolean is
      use type Ada.Calendar.Time;

      Now  : constant Ada.Calendar.Time := Ada.Calendar.Clock;
      Date : constant ADO.Nullable_Time := Member.Get_Subscription_Deadline;
   begin
      return Date.Is_Null or else Date.Value < Now;
   end Is_Expired;

   --  ------------------------------
   --  Update the status and payment information for the member.
   --  ------------------------------
   procedure Save_Payment (Model  : in out Member_Module;
                           Id     : in ADO.Identifier;
                           Member : in out Adafr.Members.Models.Member_Bean'Class) is
      Ctx            : constant ASC.Service_Context_Access := ASC.Current;
      DB             : ADO.Sessions.Master_Session := ASC.Get_Master_Session (Ctx);
      Current_Entity : aliased Adafr.Members.Models.Member_Ref;
      Receipt        : Adafr.Members.Models.Receipt_Ref;
      User           : AWA.Users.Models.User_Ref;
      Query          : ADO.Queries.Context;
      Found          : Boolean;
      Has_Receipt    : Boolean;
   begin
      Log.Info ("Update member's status and contribution with id {0}",
                ADO.Identifier'Image (Id));

      --  Check that the user has the update member permission on the given member.
      AWA.Permissions.Check (Permission => ACL_Update_Member.Permission,
                             Entity     => Id);

      Current_Entity.Load (DB, Id);
      if Member.Get_Ada_Europe then
         Current_Entity.Set_Status (Models.MEMBER_ADA_EUROPE);
      else
         Current_Entity.Set_Status (Models.MEMBER_ADA_FRANCE);
      end if;
      Current_Entity.Set_Amount (Member.Get_Amount);
      Current_Entity.Set_Payment_Date (Member.Get_Payment_Date);

      --  Id is now validated, update member.
      Member.Set_Id (Id);
      Ctx.Start;

      Has_Receipt := not Current_Entity.Get_Receipt.Is_Null;
      if not Has_Receipt or else Is_Expired (Current_Entity) then
         Receipt.Set_Id (Get_Next_Receipt_Id (DB));
         Receipt.Set_Create_Date (Ada.Calendar.Clock);
         Receipt.Set_Member (Id);
         Receipt.Set_Amount (Current_Entity.Get_Amount);
         Receipt.Save (DB);
         Current_Entity.Set_Receipt (Receipt);

         --  Subscription deadline is end of current year.
         declare
            Deadline : Util.Dates.Date_Record;
         begin
            Util.Dates.Split (Deadline, Receipt.Get_Create_Date);
            Deadline.Month := 12;
            Deadline.Month_Day := 31;
            Deadline.Hour := 23;
            Deadline.Minute := 59;
            Deadline.Second := 59;
            Current_Entity.Set_Subscription_Deadline ((Is_Null => False,
                                                       Value => Util.Dates.Time_Of (Deadline)));
         end;
      else
         Receipt := Models.Receipt_Ref (Current_Entity.Get_Receipt);
      end if;

      --  Check if this member has a user record.
      Query.Set_Join ("INNER JOIN awa_email e ON e.user_id = o.id");
      Query.Set_Filter ("LOWER(e.email) = LOWER(?)");
      Query.Bind_Param (1, String '(Current_Entity.Get_Email.Get_Email));
      User.Find (DB, Query, Found);
      if not Found then
         declare
            procedure Send_User_Created_Event;

            procedure Send_User_Created_Event is
               Event : AWA.Events.Module_Event;
            begin
               Event.Set_Event_Kind (Member_User_Created_Event.Kind);
               Model.Send_Event (Event);
            end Send_User_Created_Event;

            procedure Send_As_Created_User is
               new AWA.Services.Contexts.Run_As (Send_User_Created_Event);

            Email      : AWA.Users.Models.Email_Ref'Class := Current_Entity.Get_Email;
            First_Name : constant String := Current_Entity.Get_First_Name;
            Last_Name  : constant String := Current_Entity.Get_Last_Name;
            Country    : constant String := Current_Entity.Get_Country;
         begin
            --  Create the user in the database (we trust our paied members).
            User.Set_First_Name (First_Name);
            User.Set_Last_Name (Last_Name);
            User.Set_Name (First_Name & " " & Last_Name);
            User.Set_Email (Email);
            User.Set_Country (Country);
            User.Save (DB);

            --  Setup the user link.
            Email.Set_User_Id (User.Get_Id);
            Email.Save (DB);

            --  Send the event to finish the user setup (permission creation).
            Send_As_Created_User (User, Ctx.Get_User_Session);
         end;
      end if;

      --  Avoid sending the email again if there is no change.
      if not Current_Entity.Is_Modified then
         Ctx.Commit;
         return;
      end if;

      --  Save the member information.
      Current_Entity.Set_Update_Date (Value => Ada.Calendar.Clock);
      Current_Entity.Save (DB);
      Ctx.Commit;

      --  Schedule a job to format the receipt and send the email.
      declare
         J : AWA.Jobs.Services.Job_Type;
      begin
         J.Set_Parameter ("receipt_id", Receipt);
         J.Schedule (Receipt_Job_Definition.Factory.all);
      end;
   end Save_Payment;

   --  ------------------------------
   --  Create a new member with the given email address.
   --  ------------------------------
   procedure Create (Model  : in out Member_Module;
                     Email  : in String;
                     Member : in out Adafr.Members.Models.Member_Bean'Class) is
      pragma Unreferenced (Model);

      Ctx          : constant ASC.Service_Context_Access := ASC.Current;
      DB           : ADO.Sessions.Master_Session := ASC.Get_Master_Session (Ctx);
      Email_Entity : AWA.Users.Models.Email_Ref;
      Query        : ADO.Queries.Context;
      Check_Member : Adafr.Members.Models.Member_Ref;
      Found        : Boolean;
   begin
      Log.Info ("Create a new member {0}", Email);

      --  Check that the user has the create member permission.
      AWA.Permissions.Check (Permission => ACL_Create_Member.Permission,
                             Entity     => Ctx.Get_User_Identifier);

      Ctx.Start;

      --  Check that this member does not exist.
      Query.Set_Join ("INNER JOIN awa_email e ON e.id = o.email_id");
      Query.Set_Filter ("LOWER(e.email) = LOWER(?)");
      Query.Bind_Param (1, Email);
      Check_Member.Find (DB, Query, Found);
      if Found then
         Log.Info ("Member {0} already registered", Email);
         raise Member_Exist;
      end if;

      --  Check if the email address is known.
      Query.Clear;
      Query.Set_Filter ("LOWER(o.email) = LOWER(?)");
      Query.Bind_Param (1, Email);
      Email_Entity.Find (DB, Query, Found);
      if not Found then
         Email_Entity.Set_Email (Email);
         Email_Entity.Save (DB);
      end if;

      --  Create the new member in the waiting payment state.
      Member.Set_Status (Models.WAITING_PAYMENT);
      Member.Set_Email (Email_Entity);
      Member.Set_Create_Date (Ada.Calendar.Clock);
      Member.Set_Update_Date (Ada.Calendar.Clock);
      Member.Set_Amount ((if Member.Get_Ada_Europe then 65 else 30));
      Member.Save (DB);
      Ctx.Commit;
   end Create;

   function Get_Next_Receipt_Id (DB : in ADO.Sessions.Session'Class) return ADO.Identifier is
      Stmt : ADO.Statements.Query_Statement
        := DB.Create_Statement ("SELECT MAX(id) FROM adafr_receipt");
   begin
      Stmt.Execute;
      return ADO.Identifier (1 + Stmt.Get_Result_Integer);
   end Get_Next_Receipt_Id;

   function Get_Receipt_Path (Model   : in Member_Module;
                              Receipt : in Adafr.Members.Models.Receipt_Ref) return String is
      Ident : constant String := Util.Strings.Image (Integer (Receipt.Get_Id));
      Dir   : constant String := To_String (Model.Receipt_Directory);
   begin
      return Util.Files.Compose (Dir, Util.Files.Compose (Ident, "receipt"));
   end Get_Receipt_Path;

   function Create_Receipt (Model   : in Member_Module;
                            Member  : in Adafr.Members.Models.Member_Ref;
                            Receipt : in Adafr.Members.Models.Receipt_Ref) return String is
      Info    : Adafr.Receipt.Information;
      Bundle  : ASF.Locales.Bundle;
      Path    : constant String := Model.Get_Receipt_Path (Receipt);
   begin
      Model.Get_Application.Load_Bundle ("dates", "fr", Bundle);
      Info.Company := Member.Get_Company;
      Info.First_Name := Member.Get_First_Name;
      Info.Last_Name := Member.Get_Last_Name;
      Info.Address1 := Member.Get_Address1;
      Info.Address2 := Member.Get_Address2;
      Info.Address3 := Member.Get_Address3;
      Info.Postal_Code := Member.Get_Postal_Code;
      Info.City := Member.Get_City;
      Info.Country := Member.Get_Country;
      Info.Amount := To_Unbounded_String (Util.Strings.Image (Receipt.Get_Amount));
      Info.Ada_Europe := Member.Get_Ada_Europe;
      Util.Dates.Formats.Format (Into    => Info.Date,
                                 Pattern => "%A %d %B %Y",
                                 Date    => Member.Get_Payment_Date.Value,
                                 Bundle  => Bundle);

      Adafr.Receipt.Sign (Info, To_String (Model.Receipt_Sign_Key));
      Adafr.Receipt.Create (Path & ".tex", To_String (Model.Receipt_Template), Info);
      Adafr.Receipt.Generate (Path & ".tex");
      return Path & ".pdf";
   end Create_Receipt;

   --  ------------------------------
   --  Receipt job to format the receipt with conscript and send it by e-mail.
   --  ------------------------------
   procedure Do_Receipt_Job (Model  : in Member_Module;
                             Job    : in out AWA.Jobs.Services.Abstract_Job_Type'Class) is
      Receipt_Id : constant ADO.Identifier := Job.Get_Parameter ("receipt_id");
      Ctx        : constant ASC.Service_Context_Access := ASC.Current;
      DB         : ADO.Sessions.Master_Session := ASC.Get_Master_Session (Ctx);
      Member     : aliased Adafr.Members.Models.Member_Ref;
      Receipt    : Adafr.Members.Models.Receipt_Ref;
   begin
      Log.Info ("Receipt job for{0}", ADO.Identifier'Image (Receipt_Id));

      Receipt.Load (DB, Receipt_Id);
      Member.Load (DB, Receipt.Get_Member);

      declare
         Ptr   : constant Util.Beans.Basic.Readonly_Bean_Access
           := Member'Unchecked_Access;
         Bean  : constant Util.Beans.Objects.Object
           := Util.Beans.Objects.To_Object (Ptr, Util.Beans.Objects.STATIC);
         Email : constant AWA.Users.Models.Email_Ref'Class := Member.Get_Email;
         Receipt_Path : constant String := Model.Create_Receipt (Member, Receipt);
         Event : AWA.Events.Module_Event;
      begin
         Event.Set_Event_Kind (Send_Registered_Member_Event.Kind);
         Event.Set_Parameter ("email", Email.Get_Email);
         Event.Set_Parameter ("name", Email.Get_Email);
         Event.Set_Parameter ("member", Bean);
         Event.Set_Parameter ("receipt", Receipt_Path);
         Event.Set_Parameter ("receipt_id", Util.Strings.Image (Integer (Receipt.Get_Id)));
         Model.Send_Event (Event);
      end;
   end Do_Receipt_Job;

end Adafr.Members.Modules;
