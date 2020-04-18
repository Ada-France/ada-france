-----------------------------------------------------------------------
--  adafr-members-beans -- Beans for module members
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

with Ada.Strings.Unbounded;

with Util.Beans.Objects;
with Util.Beans.Basic;
with ADO;
with Adafr.Members.Modules;
with Adafr.Members.Models;
package Adafr.Members.Beans is

   type Member_Bean is new Adafr.Members.Models.Member_Bean with record
      Module       : Adafr.Members.Modules.Member_Module_Access := null;
      Id           : ADO.Identifier := ADO.NO_IDENTIFIER;
      History      : aliased Adafr.Members.Models.Audit_Info_List_Bean;
      History_Bean : Adafr.Members.Models.Audit_Info_List_Bean_Access;
   end record;
   type Member_Bean_Access is access all Member_Bean'Class;

   --  Get the value identified by the name.
   overriding
   function Get_Value (From : in Member_Bean;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Set the bean attribute identified by the name.
   overriding
   procedure Set_Value (Item  : in out Member_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object);

   --  Send the member email validation request.
   overriding
   procedure Send (Bean    : in out Member_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String);

   --  Save the member information.
   overriding
   procedure Save (Bean    : in out Member_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String);

   --  Validate the member information.
   overriding
   procedure Load (Bean    : in out Member_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String);

   --  Validate the member information.
   overriding
   procedure Validate (Bean    : in out Member_Bean;
                       Outcome : in out Ada.Strings.Unbounded.Unbounded_String);

   --  Register the member information.
   overriding
   procedure Register (Bean    : in out Member_Bean;
                       Outcome : in out Ada.Strings.Unbounded.Unbounded_String);

   --  Update the member after receiving the contribution.
   overriding
   procedure Save_Payment (Bean    : in out Member_Bean;
                           Outcome : in out Ada.Strings.Unbounded.Unbounded_String);

   --  Create the Members_Bean bean instance.
   function Create_Member_Bean (Module : in Adafr.Members.Modules.Member_Module_Access)
      return Util.Beans.Basic.Readonly_Bean_Access;

   type Member_List_Bean is new Adafr.Members.Models.Member_List_Bean with record
      Module       : Adafr.Members.Modules.Member_Module_Access := null;
      Members      : aliased Adafr.Members.Models.Member_Info_List_Bean;
      Members_Bean : Adafr.Members.Models.Member_Info_List_Bean_Access;
   end record;
   type Member_List_Bean_Access is access all Member_List_Bean'Class;

   overriding
   function Get_Value (From : in Member_List_Bean;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Load the list of members.
   overriding
   procedure Load (List    : in out Member_List_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String);

   --  Create the Members_List_Bean bean instance.
   function Create_Member_List_Bean (Module : in Adafr.Members.Modules.Member_Module_Access)
      return Util.Beans.Basic.Readonly_Bean_Access;

end Adafr.Members.Beans;
