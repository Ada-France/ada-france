-----------------------------------------------------------------------
--  adafr-members-servlets -- Export the members in Excel sheet
--  Copyright (C) 2021 Stephane Carrez
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
with Servlet.Core;
with ASF.Requests;
with ASF.Responses;

package Adafr.Members.Servlets is

   --  The `Excel_Servlet` represents the component that will handle
   --  an HTTP request received by the server.
   type Excel_Servlet is new Servlet.Core.Servlet with private;

   --  Called by the servlet container to indicate to a servlet that the servlet
   --  is being placed into service.
   overriding
   procedure Initialize (Server  : in out Excel_Servlet;
                         Context : in Servlet.Core.Servlet_Registry'Class);

   --  Called by the server (via the service method) to allow a servlet to handle
   --  a GET request.
   overriding
   procedure Do_Get (Server   : in Excel_Servlet;
                     Request  : in out ASF.Requests.Request'Class;
                     Response : in out ASF.Responses.Response'Class);

private

   type Excel_Servlet is new Servlet.Core.Servlet with null record;

end Adafr.Members.Servlets;
