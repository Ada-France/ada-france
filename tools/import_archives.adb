-----------------------------------------------------------------------
--  import_archives -- Import old Ada France Archives
--  Copyright (C) 2017 Ada France
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
with Ada.Command_Line;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Strings.Maps;
with Ada.Calendar.Formatting;

with ADO;
with ADO.SQL;
with ADO.Drivers;
with ADO.Queries;
with ADO.Statements;
with ADO.Sessions;
with ADO.Sessions.Factory;
with ADO.Sessions.Entities;

with Util.Strings;
with Util.Log.Loggers;
with Util.Files;
with Util.Strings.Sets;
with Util.Strings.Maps;
with Util.Strings.Transforms;

with AWA.Tags.Models;
with AWA.Users.Models;
with AWA.Blogs.Beans;
with AWA.Blogs.Models;
procedure Import_Archives is

   use Ada.Strings.Unbounded;
   use Ada.Text_IO;

   type State_Type is (PARSE_TITLE, PARSE_DATE, PARSE_POST);

   function Get_Month (Name : in String) return Ada.Calendar.Month_Number;
   procedure Set_Date (Line : in String);
   procedure Set_Title (Line : in String);
   function Find_Tag (Title : in String) return String;
   procedure Add_Tags;
   procedure Save;
   procedure Import_Line (Line : in String);
   procedure Import_Tag (Line : in String);

   State   : State_Type := PARSE_TITLE;
   Author  : AWA.Users.Models.User_Ref;
   Blog    : AWA.Blogs.Models.Blog_Ref;
   Post    : AWA.Blogs.Models.Post_Ref;
   Text    : Ada.Strings.Unbounded.Unbounded_String;
   DB      : ADO.Sessions.Master_Session;
   Kind    : ADO.Entity_Type;
   Titles  : Util.Strings.Sets.Set;
   Tag_Map : Util.Strings.Maps.Map;

   --  A list of character that we want to drop.
   Trim_Set : constant Ada.Strings.Maps.Character_Set
     := Ada.Strings.Maps.To_Set (Span => (Low  => Character'Val (0),
                                          High => ' '));

   function Get_Month (Name : in String) return Ada.Calendar.Month_Number is
   begin
      if Name = "jan" then
         return 1;
      elsif Name = "fév" then
         return 2;
      elsif Name = "mar" then
         return 3;
      elsif Name = "avr" then
         return 4;
      elsif Name = "mai" then
         return 5;
      elsif Name = "juin" then
         return 6;
      elsif Name = "juil" then
         return 7;
      elsif Name = "août" then
         return 8;
      elsif Name = "sept" then
         return 9;
      elsif Name = "oct" then
         return 10;
      elsif Name = "nov" then
         return 11;
      elsif Name = "déc" then
         return 12;
      else
         Ada.Text_IO.Put_Line ("Invalid month: " & Name);
         return 12;
      end if;
   end Get_Month;

   --  ------------------------------
   --  Set the post creation and publication date.
   --  ------------------------------
   procedure Set_Date (Line : in String) is
      use Ada.Calendar;

      Year  : Year_Number;
      Month : Month_Number;
      Day   : Day_Number;
      Pos1  : constant Natural := Util.Strings.Index (Line, ' ', Line'First + 1);
      Pos2  : constant Natural := Util.Strings.Index (Line, ' ', Pos1 + 1);
      Pos3  : constant Natural := Util.Strings.Index (Line, ' ', Pos2 + 1);
      Date  : Ada.Calendar.Time;
   begin
      Day := Day_Number'Value (Line (Line'First + 1 .. Pos1 - 1));
      Year := Year_Number'Value (Line (Pos2 + 1 .. Pos3 - 1));
      Month := Get_Month (Line (Pos1 + 1 .. Pos2 - 1));
      Date := Ada.Calendar.Formatting.Time_Of (Year        => Year,
                                               Month       => Month,
                                               Day         => Day,
                                               Hour        => 12,
                                               Minute      => 0,
                                               Second      => 0,
                                               Sub_Second  => 0.0);
      Post.Set_Create_Date (Date);
      Post.Set_Publish_Date ((Is_Null => False, Value => Date));
   end Set_Date;

   --  ------------------------------
   --  Set the post title.
   --  ------------------------------
   procedure Set_Title (Line : in String) is
      Title : constant String := Ada.Strings.Fixed.Trim (Line, Trim_Set, Trim_Set);
   begin
      Post.Set_Title (Title);
      Post.Set_Status (AWA.Blogs.Models.POST_PUBLISHED);
   end Set_Title;

   --  ------------------------------
   --  Find a possible tag by looking at the title and the tag word map table.
   --  ------------------------------
   function Find_Tag (Title : in String) return String is
      Lower : constant String := Util.Strings.Transforms.To_Lower_Case (Title);
      Iter : Util.Strings.Maps.Cursor := Tag_Map.First;
   begin
      --  Pass 1: check on title with case.
      while Util.Strings.Maps.Has_Element (Iter) loop
         if Ada.Strings.Fixed.Index (Title, Util.Strings.Maps.Key (Iter)) > 0 then
            return Util.Strings.Maps.Element (Iter);
         end if;
         Util.Strings.Maps.Next (Iter);
      end loop;

      --  Pass 2: check on lower case title.
      Iter := Tag_Map.First;
      while Util.Strings.Maps.Has_Element (Iter) loop
         if Ada.Strings.Fixed.Index (Lower, Util.Strings.Maps.Key (Iter)) > 0 then
            return Util.Strings.Maps.Element (Iter);
         end if;
         Util.Strings.Maps.Next (Iter);
      end loop;

      --  Print title without tags and use the default group for them.
      Ada.Text_IO.Put_Line ("Title: " & Title);
      return "Divers";
   end Find_Tag;

   --  ------------------------------
   --  Identify the tag to be set for the post and create it in the database.
   --  ------------------------------
   procedure Add_Tags is
      Tag      : constant String := Find_Tag (Post.Get_Title);
      Query    : ADO.Queries.Context;
      Tag_Info : AWA.Tags.Models.Tag_Ref;
      Tag_Link : AWA.Tags.Models.Tagged_Entity_Ref;
   begin
      if Tag'Length = 0 then
         return;
      end if;
      Query.Set_SQL ("SELECT"
                     & " t.id, e.id"
                     & " FROM awa_tag AS t"
                     & " LEFT JOIN awa_tagged_entity AS e ON t.id = e.tag_id"
                     & " AND e.entity_type = :entity_type AND e.for_entity_id = :entity_id"
                     & " WHERE t.name = :tag");
      declare
         Stmt : ADO.Statements.Query_Statement := DB.Create_Statement (Query);
      begin
         --  Build the query.
         Stmt.Bind_Param (Name => "entity_type", Value => Kind);
         Stmt.Bind_Param (Name => "entity_id", Value => Post.Get_Id);
         Stmt.Bind_Param (Name => "tag", Value => Tag);

         --  Run the query.
         Stmt.Execute;

         if not Stmt.Has_Elements then
            --  The tag is not defined in the database.
            --  Create it and link it to the entity.
            Tag_Info.Set_Name (Tag);
            Tag_Link.Set_Tag (Tag_Info);
            Tag_Link.Set_For_Entity_Id (Post.Get_Id);
            Tag_Link.Set_Entity_Type (Kind);

            Tag_Info.Save (DB);
            Tag_Link.Save (DB);

         elsif Stmt.Is_Null (1) then
            --  The tag is defined but the entity is not linked with it.
            Tag_Info.Set_Id (Stmt.Get_Identifier (0));
            Tag_Link.Set_Tag (Tag_Info);
            Tag_Link.Set_For_Entity_Id (Post.Get_Id);
            Tag_Link.Set_Entity_Type (Kind);
            Tag_Link.Save (DB);
         end if;
      end;
   end Add_Tags;

   --  ------------------------------
   --  Save the post and prepare to read the next post.
   --  ------------------------------
   procedure Save is
   begin
      if State = PARSE_POST then
         --  Avoid duplicate posts.
         if not Titles.Contains (Post.Get_Title) then
            Post.Set_Text (Text);
            Post.Set_Blog (Blog);
            Post.Set_Author (Author);
            Post.Set_Uri (AWA.Blogs.Beans.Get_Predefined_Uri (Post.Get_Title,
                          Post.Get_Publish_Date.Value));
            Post.Save (DB);
            Titles.Include (Post.Get_Title);
            Add_Tags;
         end if;
      end if;

      --  Reset for next posts.
      Text  := To_Unbounded_String ("");
      Post  := AWA.Blogs.Models.Null_Post;
      State := PARSE_TITLE;
   end Save;

   procedure Import_Line (Line : in String) is
   begin
      case State is
         when PARSE_TITLE =>
            if Line'Length > 3 and then Line (Line'First) = '*' then
               Set_Title (Line (Line'First + 2 .. Line'Last));
               State := PARSE_DATE;
            end if;

         when PARSE_DATE =>
            if Line'Length > 3 and then Line (Line'First) = '[' then
               Set_Date (Line);
               State := PARSE_POST;
            end if;

         when PARSE_POST =>
            if Line'Length > 3 and then Line (Line'First) = '*' then
               Save;
               Set_Title (Line (Line'First + 2 .. Line'Last));
               Post.Set_Author (Author);
               Post.Set_Allow_Comments (False);
               State := PARSE_DATE;
            elsif Line'Length > 3 and then Line (Line'First .. Line'First + 1) = "- " then
               Append (Text, "* ");
               Append (Text, Ada.Strings.Fixed.Trim (Line (Line'First + 2 .. Line'Last),
                       Trim_Set, Trim_Set));
               Append (Text, ASCII.LF);
            else
               Append (Text, Ada.Strings.Fixed.Trim (Line, Trim_Set, Trim_Set));
               if Length (Text) > 0 then
                  Append (Text, ASCII.LF);
               end if;
            end if;

      end case;
   end Import_Line;

   --  ------------------------------
   --  Import a tag line of the form:
   --    <word> <tag>
   --  ------------------------------
   procedure Import_Tag (Line : in String) is
      Pos : constant Natural := Util.Strings.Index (Line, ' ');
   begin
      if Pos > 0 then
         Tag_Map.Include (Ada.Strings.Fixed.Trim (Line (Line'First .. Pos - 1), Ada.Strings.Both),
                          Ada.Strings.Fixed.Trim (Line (Pos + 1 .. Line'Last), Ada.Strings.Both));
      end if;
   end Import_Tag;

   Count   : constant Natural := Ada.Command_Line.Argument_Count;
   Query   : ADO.SQL.Query;
   Found   : Boolean;
   Factory : ADO.Sessions.Factory.Session_Factory;
begin
   if Count /= 4 then
      Ada.Text_IO.Put_Line ("Usage: import_archives db-config author tag-file file");
      Ada.Text_IO.Put_Line ("Example: import_archives adafr.properties "
                            & "jean-pierre.rosen tags.txt file.txt");
      return;
   end if;

   declare
      Config   : constant String := Ada.Command_Line.Argument (1);
      User     : constant String := Ada.Command_Line.Argument (2);
      Tag_File : constant String := Ada.Command_Line.Argument (3);
      File     : constant String := Ada.Command_Line.Argument (4);
   begin
      Util.Log.Loggers.Initialize (Config);

      --  Initialize the database drivers.
      ADO.Drivers.Initialize (Config);

      --  Initialize the session factory to connect to the
      --  database defined by 'database' property.
      Factory.Create (ADO.Drivers.Get_Config ("database"));

      --  Find the author in the database.
      DB := Factory.Get_Master_Session;
      DB.Begin_Transaction;
      Query.Set_Filter ("name = ?");
      Query.Bind_Param (1, User);
      Author.Find (DB, Query, Found);
      if not Found then
         Ada.Text_IO.Put_Line ("User '" & User & "' was not found in the database");
         return;
      end if;

      --  Find the blog instance.
      Query.Bind_Param (1, "Ada France");
      Blog.Find (DB, Query, Found);
      if not Found then
         Ada.Text_IO.Put_Line ("Blog 'Ada France' was not found in the database");
         return;
      end if;

      --  Get the awa_post entity type for tags.
      Kind := ADO.Sessions.Entities.Find_Entity_Type (DB, "awa_post");

      --  Load the tag matching file.
      Util.Files.Read_File (Path    => Tag_File,
                            Process => Import_Tag'Access);

      --  Load the archive file.
      Util.Files.Read_File (Path    => File,
                            Process => Import_Line'Access);
      Save;
   end;
   DB.Commit;
end Import_Archives;
