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
with Ada.Strings.Unbounded;
with ASF.Applications;

with ADO;
with Security.Permissions;
with AWA.Modules;
with AWA.Events;
with Adafr.Members.Models;
private with Security.Random;
package Adafr.Members.Modules is

   package Send_Validate_Event is
     new AWA.Events.Definition (Name => "member-send-validate");
   package Send_Subscribed_Event is
     new AWA.Events.Definition (Name => "member-subscribed");
   package Send_Pending_Member_Event is
     new AWA.Events.Definition (Name => "member-pending");
   package Send_Registered_Member_Event is
     new AWA.Events.Definition (Name => "member-registered");
   package Send_Updated_Member_Event is
     new AWA.Events.Definition (Name => "member-updated");

   package ACL_View_Member is
     new Security.Permissions.Definition ("member-view");
   package ACL_Update_Member is
     new Security.Permissions.Definition ("member-update");
   package ACL_Create_Member is
     new Security.Permissions.Definition ("member-create");

   --  The name under which the module is registered.
   NAME : constant String := "members";

   PARAM_SIGN_KEY          : constant String := "secret_key";
   PARAM_RECEIPT_TEMPLATE  : constant String := "receipt_template";
   PARAM_RECEIPT_DIRECTORY : constant String := "receipt_directory";
   PARAM_RECEIPT_KEY       : constant String := "receipt_key";

   Member_Exist : exception;

   --  ------------------------------
   --  Module members
   --  ------------------------------
   type Member_Module is new AWA.Modules.Module with private;
   type Member_Module_Access is access all Member_Module'Class;

   --  Initialize the members module.
   overriding
   procedure Initialize (Plugin : in out Member_Module;
                         App    : in AWA.Modules.Application_Access;
                         Props  : in ASF.Applications.Config);

   --  Configures the module after its initialization and after having read
   --  its XML configuration.
   overriding
   procedure Configure (Plugin : in out Member_Module;
                        Props  : in ASF.Applications.Config);

   --  Get the members module.
   function Get_Member_Module return Member_Module_Access;


   --  Send_Mail_Validation
   procedure Send_Mail_Validation (Model  : in out Member_Module;
                                   Email  : in String;
                                   Entity : in out Adafr.Members.Models.Member_Ref'Class);

   --  Validate the secure key
   procedure Validate_Key (Model  : in out Member_Module;
                           Key    : in String;
                           Entity : in out Adafr.Members.Models.Member_Ref'Class);

   --  Load the member information.
   procedure Load (Model   : in out Member_Module;
                   Id      : in ADO.Identifier;
                   Entity  : in out Adafr.Members.Models.Member_Ref'Class;
                   History : in out Adafr.Members.Models.Audit_Info_List_Bean);

   --  Save the member information after validating the secure key
   procedure Save (Model  : in out Member_Module;
                   Id     : in ADO.Identifier;
                   Key    : in String;
                   Member : in out Adafr.Members.Models.Member_Bean'Class);

   --  Register the member for the Ada-France or Ada-France+Ada-Europe
   --  after validating the secure key
   procedure Register (Model  : in out Member_Module;
                       Member : in out Adafr.Members.Models.Member_Bean'Class);

   --  Update the status and payment information for the member.
   procedure Save_Payment (Model  : in out Member_Module;
                           Id     : in ADO.Identifier;
                           Member : in out Adafr.Members.Models.Member_Bean'Class);

   --  Create a new member with the given email address.
   procedure Create (Model  : in out Member_Module;
                     Email  : in String;
                     Member : in out Adafr.Members.Models.Member_Bean'Class);

private

   type Member_Module is new AWA.Modules.Module with record
      Random            : Security.Random.Generator;
      Sign_Key          : Ada.Strings.Unbounded.Unbounded_String;
      Receipt_Sign_Key  : Ada.Strings.Unbounded.Unbounded_String;
      Receipt_Template  : Ada.Strings.Unbounded.Unbounded_String;
      Receipt_Directory : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   function Get_Secure_Key (Model : in Member_Module;
                            Member : in Adafr.Members.Models.Member_Ref'Class)
                            return String;

   function Validate_Secure_Key (Model  : in Member_Module;
                                 Value  : in String;
                                 Member : in Adafr.Members.Models.Member_Ref'Class)
                                 return Boolean;

   function Get_Receipt_Path (Model   : in Member_Module;
                              Receipt : in Adafr.Members.Models.Receipt_Ref) return String;

   function Create_Receipt (Model   : in Member_Module;
                            Member  : in Adafr.Members.Models.Member_Ref;
                            Receipt : in Adafr.Members.Models.Receipt_Ref) return String;

end Adafr.Members.Modules;
