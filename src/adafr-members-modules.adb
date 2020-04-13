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

with AWA.Modules.Beans;
with AWA.Modules.Get;
with Util.Log.Loggers;
with Adafr.Members.Beans;
package body Adafr.Members.Modules is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Adafr.Members.Module");

   package Register is new AWA.Modules.Beans (Module => Member_Module,
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
      Register.Register (Plugin => Plugin,
                         Name   => "Adafr.Members.Beans.Members_Bean",
                         Handler => Adafr.Members.Beans.Create_Member_Bean'Access);

      AWA.Modules.Module (Plugin).Initialize (App, Props);

      --  Add here the creation of manager instances.
   end Initialize;

   --  ------------------------------
   --  Get the members module.
   --  ------------------------------
   function Get_Member_Module return Member_Module_Access is
      function Get is new AWA.Modules.Get (Member_Module, Member_Module_Access, NAME);
   begin
      return Get;
   end Get_Member_Module;

end Adafr.Members.Modules;
