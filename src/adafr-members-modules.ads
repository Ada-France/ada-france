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
with ASF.Applications;

with AWA.Modules;
package Adafr.Members.Modules is

   --  The name under which the module is registered.
   NAME : constant String := "members";

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

   --  Get the members module.
   function Get_Member_Module return Member_Module_Access;

private

   type Member_Module is new AWA.Modules.Module with null record;

end Adafr.Members.Modules;
