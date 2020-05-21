-----------------------------------------------------------------------
--  adafr-members-modules -- Module members
--  Copyright (C) 2020 Stephane Carrez
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
with Util.Beans.Objects;
with Util.Beans.Basic;
with Util.Encoders.HMAC.SHA256;
with Adafr.Members.Beans;
with ADO.Sessions;
with ADO.Queries;
with ADO.Objects;
with AWA.Permissions;
with AWA.Services.Contexts;
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

      AWA.Modules.Module (Plugin).Initialize (App, Props);
   end Initialize;

   --  ------------------------------
   --  Configures the module after its initialization and after having read
   --  its XML configuration.
   --  ------------------------------
   overriding
   procedure Configure (Plugin : in out Member_Module;
                        Props  : in ASF.Applications.Config) is
   begin
      Plugin.Sign_Key := Props.Get (PARAM_SIGN_KEY);
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
   procedure Save (Model  : in out Member_Module;
                   Id     : in ADO.Identifier;
                   Key    : in String;
                   Member : in out Adafr.Members.Models.Member_Bean'Class) is
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
      if Current_Entity.Get_Status = Models.PENDING then
         Current_Entity.Set_Status (Models.WAITING_PAYMENT);
      end if;
      --  Member.Load (DB, Current_Entity.Get_Id);

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

   --  ------------------------------
   --  Update the status and payment information for the member.
   --  ------------------------------
   procedure Save_Payment (Model  : in out Member_Module;
                           Id     : in ADO.Identifier;
                           Member : in out Adafr.Members.Models.Member_Bean'Class) is
      Ctx   : constant ASC.Service_Context_Access := ASC.Current;
      DB    : ADO.Sessions.Master_Session := ASC.Get_Master_Session (Ctx);
      Current_Entity : aliased Adafr.Members.Models.Member_Ref;
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
      Current_Entity.Set_Payment_Date (Member.Get_Payment_Date);

      --  Id is now validated, update member.
      Member.Set_Id (Id);

      --  Avoid sending the email again if there is no change.
      if not Current_Entity.Is_Modified then
         return;
      end if;

      --  Save the member information and send the event to trigger the email.
      Ctx.Start;
      Current_Entity.Set_Update_Date (Value => Ada.Calendar.Clock);
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
         Event.Set_Event_Kind (Send_Registered_Member_Event.Kind);
         Event.Set_Parameter ("email", Email.Get_Email);
         Event.Set_Parameter ("name", Email.Get_Email);
         Event.Set_Parameter ("member", Bean);
         Model.Send_Event (Event);
      end;
   end Save_Payment;

end Adafr.Members.Modules;
