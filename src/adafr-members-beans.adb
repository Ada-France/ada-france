-----------------------------------------------------------------------
--  adafr-members-beans -- Beans for module members
--  Copyright (C) 2020, 2021, 2025 Stephane Carrez
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
with ADO.Utils;
with ADO.Objects;
with ADO.Sessions;
with ADO.Queries;
with Util.Beans.Objects.Time;
with ASF.Contexts.Faces;
with ASF.Contexts.Flash;
with ASF.Applications.Messages.Factory;
with AWA.Services.Contexts;
package body Adafr.Members.Beans is

   use ASF.Applications;
   use Ada.Strings.Unbounded;
   use type ADO.Identifier;
   use type Ada.Calendar.Time;
   use type Models.Status_Type;

   package ASC renames AWA.Services.Contexts;
   package UBO renames Util.Beans.Objects;

   procedure Invalid_Secure_Key (Outcome : out Ada.Strings.Unbounded.Unbounded_String);

   --  ------------------------------
   --  Add a message to the flash context so that it will be displayed on the next page.
   --  ------------------------------
   procedure Invalid_Secure_Key (Outcome : out Ada.Strings.Unbounded.Unbounded_String) is
      Ctx   : constant ASF.Contexts.Faces.Faces_Context_Access := ASF.Contexts.Faces.Current;
      Flash : constant ASF.Contexts.Faces.Flash_Context_Access := Ctx.Get_Flash;
   begin
      Flash.Set_Keep_Messages (True);
      Messages.Factory.Add_Message (Ctx.all, "members.secure_key_was_invalid_error",
                                    Messages.ERROR);
      Outcome := To_Unbounded_String ("failure");
   end Invalid_Secure_Key;

   --  ------------------------------
   --  Set the bean attribute identified by the name.
   --  ------------------------------
   overriding
   procedure Set_Value (Item  : in out Member_Bean;
                        Name  : in String;
                        Value : in UBO.Object) is
   begin
      if Name = "id" and not UBO.Is_Empty (Value) then
         Item.Id := ADO.Utils.To_Identifier (Value);
      elsif Name = "confirmEmail" then
         if not UBO.Is_Empty (Value) then
            raise Constraint_Error;
         end if;
      elsif Name = "inactive" then
         Item.Inactive.Is_Null := UBO.Is_Null (Value);
         if not Item.Inactive.Is_Null then
            Item.Inactive.Value := UBO.To_Boolean (Value);
         end if;
      else
         Models.Member_Bean (Item).Set_Value (Name, Value);
      end if;
   end Set_Value;

   --  ------------------------------
   --  Get the value identified by the name.
   --  ------------------------------
   overriding
   function Get_Value (From : in Member_Bean;
                       Name : in String) return UBO.Object is
   begin
      if Name = "history" then
         return UBO.To_Object (Value   => From.History_Bean,
                               Storage => Util.Beans.Objects.STATIC);

      elsif Name = "expired" then
         return UBO.To_Object (Modules.Is_Expired (From));

      elsif Name = "payment_date" and then Modules.Is_Expired (From) then
         return UBO.Time.To_Object (Ada.Calendar.Clock);

      elsif Name = "last_payment_date" then
         return Models.Member_Bean (From).Get_Value ("payment_date");

      elsif Name = "inactive" then
         if From.Inactive.Is_Null then
            return UBO.To_Object (From.Get_Status = Models.INACTIVE);
         else
            return UBO.To_Object (From.Inactive.Value);
         end if;
      else
         return Models.Member_Bean (From).Get_Value (Name);
      end if;
   end Get_Value;

   --  ------------------------------
   --  Send the member email validation request.
   --  ------------------------------
   overriding
   procedure Send (Bean    : in out Member_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
   begin
      Bean.Module.Send_Mail_Validation (To_String (Bean.Email), Bean);
      Outcome := To_Unbounded_String ("success");
   end Send;

   --  ------------------------------
   --  Save the member information.
   --  ------------------------------
   overriding
   procedure Save (Bean    : in out Member_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
   begin
      Bean.Module.Save (Bean.Id, To_String (Bean.Key), Bean.Inactive, Bean);
      Outcome := To_Unbounded_String ("saved");

   exception
      when ADO.Objects.NOT_FOUND =>
         Invalid_Secure_Key (Outcome);
   end Save;

   --  ------------------------------
   --  Validate the member information.
   --  ------------------------------
   overriding
   procedure Load (Bean    : in out Member_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
   begin
      if Bean.Id > 0 then
         Bean.Module.Load (Bean.Id, Bean, Bean.History);
      else
         Bean.Module.Validate_Key (To_String (Bean.Key), Bean);
      end if;
      Bean.Email := Bean.Get_Email.Get_Email;
      Outcome := To_Unbounded_String ("loaded");

   exception
      when ADO.Objects.NOT_FOUND =>
         Invalid_Secure_Key (Outcome);
   end Load;

   --  ------------------------------
   --  Validate the member information.
   --  ------------------------------
   overriding
   procedure Validate (Bean    : in out Member_Bean;
                       Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
   begin
      Bean.Module.Validate_Key (To_String (Bean.Key), Bean);
      Bean.Email := Bean.Get_Email.Get_Email;

   exception
      when ADO.Objects.NOT_FOUND =>
         Outcome := To_Unbounded_String ("failure");
         Messages.Factory.Add_Field_Message ("key", "members.secure_key_invalid_error",
                                             Messages.ERROR);
   end Validate;

   --  ------------------------------
   --  Register the member information.
   --  ------------------------------
   overriding
   procedure Register (Bean    : in out Member_Bean;
                       Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
   begin
      Bean.Module.Register (Bean);

   exception
      when ADO.Objects.NOT_FOUND =>
         Invalid_Secure_Key (Outcome);
   end Register;

   --  ------------------------------
   --  Update the member after receiving the contribution.
   --  ------------------------------
   overriding
   procedure Save_Payment (Bean    : in out Member_Bean;
                           Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
   begin
      Bean.Module.Save_Payment (Bean.Id, Bean);
      Outcome := To_Unbounded_String ("saved");
   end Save_Payment;

   --  ------------------------------
   --  Create a new member.
   --  ------------------------------
   overriding
   procedure Create (Bean    : in out Member_Bean;
                     Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
   begin
      Bean.Module.Create (To_String (Bean.Email), Bean);
      Outcome := To_Unbounded_String ("created");

   exception
      when Adafr.Members.Modules.Member_Exist =>
         Outcome := To_Unbounded_String ("failure");
         Messages.Factory.Add_Field_Message ("email", "members.member_already_registered");
   end Create;

   --  ------------------------------
   --  Create the Member_Bean bean instance.
   --  ------------------------------
   function Create_Member_Bean (Module : in Adafr.Members.Modules.Member_Module_Access)
      return Util.Beans.Basic.Readonly_Bean_Access is
      Object : constant Member_Bean_Access := new Member_Bean;
   begin
      Object.Module := Module;
      Object.History_Bean := Object.History'Access;
      return Object.all'Access;
   end Create_Member_Bean;

   --  ------------------------------
   --  Get the value identified by the name.
   --  ------------------------------
   overriding
   function Get_Value (From : in Member_List_Bean;
                       Name : in String) return UBO.Object is
      Pos : Natural;
   begin
      if Name = "members" then
         return UBO.To_Object (Value   => From.Members_Bean,
                               Storage => Util.Beans.Objects.STATIC);

      elsif Name = "page" then
         return UBO.To_Object (From.Page);

      elsif Name = "count" then
         return UBO.To_Object (From.Count);

      elsif Name = "filter" then
         return Models.Filter_Type_Objects.To_Object (From.Filter);

      elsif Name = "expired" then
         Pos := From.Members.Get_Row_Index;
         if Pos = 0 then
            return UBO.Null_Object;
         end if;
         declare
            Item : constant Models.Member_Info := From.Members.List.Element (Pos);
            Now  : constant Ada.Calendar.Time := Ada.Calendar.Clock;
            Date : constant ADO.Nullable_Time := Item.Subscription_Deadline;
         begin
            return UBO.To_Object (Date.Is_Null or else Date.Value < Now);
         end;

      else
         return From.Members.Get_Value (Name);
      end if;
   end Get_Value;

   --  ------------------------------
   --  Load the list of members.
   --  ------------------------------
   overriding
   procedure Load (List    : in out Member_List_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
      Ctx     : constant ASC.Service_Context_Access := ASC.Current;
      Now     : constant Ada.Calendar.Time := Ada.Calendar.Clock;
      Session : ADO.Sessions.Session := ASC.Get_Session (Ctx);
      Query   : ADO.Queries.Context;
   begin
      case List.Filter is
         when Models.LIST_ALL =>
            Query.Bind_Param ("status1", Integer (1));
            Query.Bind_Param ("status2", Integer (2));
            Query.Bind_Param ("status3", Integer (3));
            Query.Set_Query (Adafr.Members.Models.Query_Adafr_User_List);

         when Models.LIST_MEMBERS =>
            Query.Bind_Param ("status1", Integer (2));
            Query.Bind_Param ("status2", Integer (3));
            Query.Bind_Param ("date", Now);
            Query.Set_Query (Adafr.Members.Models.Query_Adafr_Member_List);

         when Models.LIST_AE_MEMBERS =>
            Query.Bind_Param ("status1", Integer (3));
            Query.Bind_Param ("status2", Integer (3));
            Query.Bind_Param ("date", Now);
            Query.Set_Query (Adafr.Members.Models.Query_Adafr_Member_List);

         when Models.LIST_AF_MEMBERS =>
            Query.Bind_Param ("status1", Integer (2));
            Query.Bind_Param ("status2", Integer (2));
            Query.Bind_Param ("date", Now);
            Query.Set_Query (Adafr.Members.Models.Query_Adafr_Member_List);

         when Models.LIST_PENDING =>
            Query.Bind_Param ("status1", Integer (1));
            Query.Set_Query (Adafr.Members.Models.Query_Adafr_Member_List_By_Status);

         when Models.LIST_EXPIRED =>
            Query.Bind_Param ("status1", Integer (2));
            Query.Bind_Param ("status2", Integer (3));
            Query.Bind_Param ("date", Now);
            Query.Set_Query (Adafr.Members.Models.Query_Adafr_Member_List_Expired);

         when Models.LIST_INACTIVE =>
            Query.Bind_Param ("status1", Integer (4));
            Query.Set_Query (Adafr.Members.Models.Query_Adafr_Member_List_By_Status);

      end case;
      Adafr.Members.Models.List (List.Members_Bean.all, Session, Query);
      Outcome := To_Unbounded_String ("success");
   end Load;

   --  ------------------------------
   --  Create the Members_List_Bean bean instance.
   --  ------------------------------
   function Create_Member_List_Bean (Module : in Adafr.Members.Modules.Member_Module_Access)
                                     return Util.Beans.Basic.Readonly_Bean_Access is
      Object : constant Member_List_Bean_Access := new Member_List_Bean;
   begin
      Object.Module := Module;
      Object.Page_Size := 20;
      Object.Page := 1;
      Object.Count := 0;
      Object.Filter := Adafr.Members.Models.LIST_ALL;
      Object.Members_Bean := Object.Members'Access;
      return Object.all'Access;
   end Create_Member_List_Bean;

end Adafr.Members.Beans;
