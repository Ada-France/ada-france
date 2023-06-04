-----------------------------------------------------------------------
--  adafr-members-servlets -- Export the members in Excel sheet
--  Copyright (C) 2021, 2022 Ada France
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
with Ada.Calendar;

with Excel_Out;

with Unicode.Encodings;
with Util.Log.Loggers;
with Util.Dates.ISO8601;

with ADO.Sessions;
with ADO.Queries;

with ASF.Streams;

with Adafr.Members.Modules;
with Adafr.Members.Models;
package body Adafr.Members.Servlets is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Adafr.Members.Servlets");

   function To_Latin1 (Content : in Ada.Strings.Unbounded.Unbounded_String) return String is
     (Unicode.Encodings.Convert (Ada.Strings.Unbounded.To_String (Content),
                                 Unicode.Encodings.Get_By_Name ("utf-8"),
                                 Unicode.Encodings.Get_By_Name ("iso-8859-15")));

   function To_Latin1 (Content : in String) return String is
     (Unicode.Encodings.Convert (Content,
                                 Unicode.Encodings.Get_By_Name ("utf-8"),
                                 Unicode.Encodings.Get_By_Name ("iso-8859-15")));

   overriding
   procedure Initialize (Server  : in out Excel_Servlet;
                         Context : in Servlet.Core.Servlet_Registry'Class) is
   begin
      null;
   end Initialize;

   overriding
   procedure Do_Get (Server   : in Excel_Servlet;
                     Request  : in out ASF.Requests.Request'Class;
                     Response : in out ASF.Responses.Response'Class) is
      pragma Unreferenced (Server);
      use Adafr.Members.Models;
      use type Excel_Out.Cell_Border;
      use type Ada.Calendar.Time;

      procedure Report (Label          : in String;
                        Status         : in Models.Status_Type;
                        Report_Expired : in Boolean);

      URI         : constant String := Request.Get_Request_URI;
      Content     : Excel_Out.Excel_Out_String;
      Module      : constant Modules.Member_Module_Access := Modules.Get_Member_Module;
      Now         : constant Ada.Calendar.Time := Ada.Calendar.Clock;
      Date        : constant String := Util.Dates.ISO8601.Image (Now);
      Session     : ADO.Sessions.Session := Module.Get_Session;
      Query       : ADO.Queries.Context;
      List        : Adafr.Members.Models.Export_Member_Info_Vector;
      Row         : Positive := 1;
      Font_Title  : Excel_Out.Font_Type;
      Font_Normal : Excel_Out.Font_Type;
      Fmt_Title   : Excel_Out.Format_Type;
      Fmt_Header  : Excel_Out.Format_Type;
      Fmt_Default : Excel_Out.Format_Type;
      Fmt_Expired : Excel_Out.Format_Type;

      procedure Report (Label          : in String;
                        Status         : in Models.Status_Type;
                        Report_Expired : in Boolean) is
      begin
         --  Setup headers.
         Content.Use_Format (Fmt_Title);
         Content.Write_Row_Height (Row, 30);
         Content.Write (Row, 1, Label);
         Content.Merge (11);
         Row := Row + 1;

         Content.Use_Format (Fmt_Header);
         Content.Write (Row, 1, "Id");
         Content.Write (Row, 2, "Nom");
         Content.Write (Row, 3, To_Latin1 ("Prénom"));
         Content.Write (Row, 4, "Couriel");
         Content.Write (Row, 5, "Cotisation");
         Content.Write (Row, 6, To_Latin1 ("Société"));
         Content.Write (Row, 7, "Adresse");
         Content.Write (Row, 8, " ");
         Content.Write (Row, 9, " ");
         Content.Write (Row, 10, "Code postal");
         Content.Write (Row, 11, "Ville");
         Content.Write (Row, 12, "Pays");

         Content.Use_Format (Fmt_Default);
         Row := Row + 1;
         for Member of List loop
            declare
               Is_Expired : constant Boolean := not Member.Subscription_Deadline.Is_Null
                 and then Member.Subscription_Deadline.Value < Now
                 and then Member.Status /= Models.WAITING_PAYMENT;
            begin
               if (Status = Member.Status and not Report_Expired and not Is_Expired)
                 or else (Report_Expired and Is_Expired)
               then
                  if Is_Expired then
                     Content.Use_Format (Fmt_Expired);
                  else
                     Content.Use_Format (Fmt_Default);
                  end if;
                  Content.Write (Row, 1, Ado.Identifier'Image (Member.Id));
                  Content.Write (Row, 2, To_Latin1 (Member.First_Name));
                  Content.Write (Row, 3, To_Latin1 (Member.Last_Name));
                  Content.Write (Row, 4, To_Latin1 (Member.Email));
                  Content.Write (Row, 5, Member.Amount);
                  Content.Write (Row, 6, To_Latin1 (Member.Company));
                  Content.Write (Row, 7, To_Latin1 (Member.Address1));
                  Content.Write (Row, 8, To_Latin1 (Member.Address2));
                  Content.Write (Row, 9, To_Latin1 (Member.Address3));
                  Content.Write (Row, 10, Member.Postal_Code);
                  Content.Write (Row, 11, To_Latin1 (Member.City));
                  Content.Write (Row, 12, To_Latin1 (Member.Country));
                  Row := Row + 1;
               end if;
            end;
         end loop;
      end Report;

   begin
      Log.Info ("GET: {0}", URI);

      --  Send the file.
      Response.Set_Content_Type ("application/vnd.ms-excel");
      Response.Add_Header ("Content-Disposition",
                           "attachment; filename=adafr-members-" & Date & ".xls");

      Query.Set_Query (Adafr.Members.Models.Query_Adafr_Export_Member_List);
      Adafr.Members.Models.List (List, Session, Query);

      Content.Create;
      Content.Zoom_Level (85, 100);
      Content.Header ("Ada France members");
      Content.Footer ("Ada France");
      Content.Margins (0.5, 0.5, 0.5, 0.5);
      Content.Page_Setup (Orientation => Excel_Out.Landscape,
                          Scale_Or_Fit => Excel_Out.Fit);

      Content.Define_font ("Arial Narrow", 16, font_title, Excel_Out.bold);
      Content.Define_font ("Calibri", 12, font_normal);

      Content.Define_format (font_title, Excel_Out.general,
                             Fmt_Title,
                             border => Excel_Out.top & Excel_Out.bottom,
                             vertical_align => Excel_Out.centred);
      Content.Define_format (font_normal, Excel_Out.general,
                             Fmt_Header,
                             Background_Color => Excel_Out.Dark_Blue,
                             border => Excel_Out.Bottom,
                             Vertical_Align => Excel_Out.Centred);
      Content.Define_format (font_normal, Excel_Out.general,
                             Fmt_Default);
      Content.Define_format (font_normal, Excel_Out.general,
                             Fmt_Expired,
                             Background_Color => Excel_Out.Silver);

      --  Setup sizes of various columns.
      Content.Write_Column_Width (1, 5);
      Content.Write_Column_Width (2, 20);
      Content.Write_Column_Width (3, 20);
      Content.Write_Column_Width (4, 40);
      Content.Write_Column_Width (5, 10);
      Content.Write_Column_Width (6, 30);
      Content.Write_Column_Width (7, 20);
      Content.Write_Column_Width (8, 20);
      Content.Write_Column_Width (9, 10);
      Content.Write_Column_Width (10, 15);
      Content.Write_Column_Width (11, 10);
      Content.Write_Column_Width (12, 20);

      Row := 5;
      Report ("Membres Ada France et Ada Europe", Models.Member_Ada_Europe, False);

      Row := Row + 3;
      Report ("Membres Ada France", Models.Member_Ada_France, False);

      Row := Row + 3;
      Report ("En attente de la cotisation", Models.Waiting_Payment, False);

      Row := Row + 3;
      Report (To_Latin1 ("Expirés"), Models.Waiting_Payment, True);

      Content.Close;
      declare
         Output : ASF.Streams.Print_Stream := Response.Get_Output_Stream;
         Result : constant String := Content.Contents;
      begin
         Output.Write (Result);
      end;

   exception
      when E : others =>
         Log.Error ("Internal error", E);
         Response.Send_Error (ASF.Responses.SC_INTERNAL_SERVER_ERROR);
         return;

   end Do_Get;

end Adafr.Members.Servlets;
