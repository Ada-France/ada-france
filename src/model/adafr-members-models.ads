-----------------------------------------------------------------------
--  Adafr.Members.Models -- Adafr.Members.Models
-----------------------------------------------------------------------
--  File generated by Dynamo DO NOT MODIFY
--  Template used: templates/model/package-spec.xhtml
--  Ada Generator: https://github.com/stcarrez/dynamo Version 1.3.0
-----------------------------------------------------------------------
--  Copyright (C) 2023 Stephane Carrez
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
pragma Warnings (Off);
with ADO.Sessions;
with ADO.Objects;
with ADO.Statements;
with ADO.SQL;
with ADO.Schemas;
with ADO.Queries;
with ADO.Queries.Loaders;
with Ada.Calendar;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with Util.Beans.Objects;
with Util.Beans.Objects.Enums;
with Util.Beans.Basic.Lists;
with ADO.Audits;
with AWA.Users.Models;
with Util.Beans.Methods;
pragma Warnings (On);
package Adafr.Members.Models is

   pragma Style_Checks ("-mrIu");

   type Filter_Type is (LIST_ALL, LIST_MEMBERS, LIST_AE_MEMBERS, LIST_PENDING, LIST_AF_MEMBERS, LIST_EXPIRED, LIST_INACTIVE);
   for Filter_Type use (LIST_ALL => 0, LIST_MEMBERS => 1, LIST_AE_MEMBERS => 2, LIST_PENDING => 3, LIST_AF_MEMBERS => 4, LIST_EXPIRED => 5, LIST_INACTIVE => 6);
   package Filter_Type_Objects is
      new Util.Beans.Objects.Enums (Filter_Type);

   type Nullable_Filter_Type is record
      Is_Null : Boolean := True;
      Value   : Filter_Type;
   end record;

   type Status_Type is (PENDING, WAITING_PAYMENT, MEMBER_ADA_FRANCE, MEMBER_ADA_EUROPE, INACTIVE);
   for Status_Type use (PENDING => 0, WAITING_PAYMENT => 1, MEMBER_ADA_FRANCE => 2, MEMBER_ADA_EUROPE => 3, INACTIVE => 4);
   package Status_Type_Objects is
      new Util.Beans.Objects.Enums (Status_Type);

   type Nullable_Status_Type is record
      Is_Null : Boolean := True;
      Value   : Status_Type;
   end record;

   type Receipt_Ref is new ADO.Objects.Object_Ref with null record;

   type Member_Ref is new ADO.Objects.Object_Ref with null record;

   --  Create an object key for Receipt.
   function Receipt_Key (Id : in ADO.Identifier) return ADO.Objects.Object_Key;
   --  Create an object key for Receipt from a string.
   --  Raises Constraint_Error if the string cannot be converted into the object key.
   function Receipt_Key (Id : in String) return ADO.Objects.Object_Key;

   Null_Receipt : constant Receipt_Ref;
   function "=" (Left, Right : Receipt_Ref'Class) return Boolean;

   --  Set the receipt id
   procedure Set_Id (Object : in out Receipt_Ref;
                     Value  : in ADO.Identifier);

   --  Get the receipt id
   function Get_Id (Object : in Receipt_Ref)
                 return ADO.Identifier;

   --  Set the receipt creation date
   procedure Set_Create_Date (Object : in out Receipt_Ref;
                              Value  : in Ada.Calendar.Time);

   --  Get the receipt creation date
   function Get_Create_Date (Object : in Receipt_Ref)
                 return Ada.Calendar.Time;

   --  Set the amount in euros
   procedure Set_Amount (Object : in out Receipt_Ref;
                         Value  : in Integer);

   --  Get the amount in euros
   function Get_Amount (Object : in Receipt_Ref)
                 return Integer;

   --
   procedure Set_Member (Object : in out Receipt_Ref;
                         Value  : in ADO.Identifier);

   --
   function Get_Member (Object : in Receipt_Ref)
                 return ADO.Identifier;

   --  Load the entity identified by 'Id'.
   --  Raises the NOT_FOUND exception if it does not exist.
   procedure Load (Object  : in out Receipt_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Id      : in ADO.Identifier);

   --  Load the entity identified by 'Id'.
   --  Returns True in <b>Found</b> if the object was found and False if it does not exist.
   procedure Load (Object  : in out Receipt_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Id      : in ADO.Identifier;
                   Found   : out Boolean);

   --  Find and load the entity.
   overriding
   procedure Find (Object  : in out Receipt_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Query   : in ADO.SQL.Query'Class;
                   Found   : out Boolean);

   --  Save the entity.  If the entity does not have an identifier, an identifier is allocated
   --  and it is inserted in the table.  Otherwise, only data fields which have been changed
   --  are updated.
   overriding
   procedure Save (Object  : in out Receipt_Ref;
                   Session : in out ADO.Sessions.Master_Session'Class);

   --  Delete the entity.
   overriding
   procedure Delete (Object  : in out Receipt_Ref;
                     Session : in out ADO.Sessions.Master_Session'Class);

   overriding
   function Get_Value (From : in Receipt_Ref;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Table definition
   RECEIPT_TABLE : constant ADO.Schemas.Class_Mapping_Access;

   --  Internal method to allocate the Object_Record instance
   overriding
   procedure Allocate (Object : in out Receipt_Ref);

   --  Copy of the object.
   procedure Copy (Object : in Receipt_Ref;
                   Into   : in out Receipt_Ref);

   --  --------------------
   --  The Member table holds the list of Ada France members with the necessary
   --  information so that we can send them the Ada User Journal if they are
   --  member of Ada Europe. The member is first in the PENDING state
   --  until we receive the validation of the email address. Then, it enters
   --  int the WAITNG_PAYMENT state until the payment is acknowledged.
   --  The payment process is manual (wire transfer or by check) and
   --  switch to MEMBER once it is received.the member identifier
   --  --------------------
   --  Create an object key for Member.
   function Member_Key (Id : in ADO.Identifier) return ADO.Objects.Object_Key;
   --  Create an object key for Member from a string.
   --  Raises Constraint_Error if the string cannot be converted into the object key.
   function Member_Key (Id : in String) return ADO.Objects.Object_Key;

   Null_Member : constant Member_Ref;
   function "=" (Left, Right : Member_Ref'Class) return Boolean;

   --
   procedure Set_Id (Object : in out Member_Ref;
                     Value  : in ADO.Identifier);

   --
   function Get_Id (Object : in Member_Ref)
                 return ADO.Identifier;
   --  Get optimistic locking version
   function Get_Version (Object : in Member_Ref)
                 return Integer;

   --  Set the member's first name.
   procedure Set_First_Name (Object : in out Member_Ref;
                             Value  : in Ada.Strings.Unbounded.Unbounded_String);
   procedure Set_First_Name (Object : in out Member_Ref;
                             Value : in String);

   --  Get the member's first name.
   function Get_First_Name (Object : in Member_Ref)
                 return Ada.Strings.Unbounded.Unbounded_String;
   function Get_First_Name (Object : in Member_Ref)
                 return String;

   --  Set the member's last name.
   procedure Set_Last_Name (Object : in out Member_Ref;
                            Value  : in Ada.Strings.Unbounded.Unbounded_String);
   procedure Set_Last_Name (Object : in out Member_Ref;
                            Value : in String);

   --  Get the member's last name.
   function Get_Last_Name (Object : in Member_Ref)
                 return Ada.Strings.Unbounded.Unbounded_String;
   function Get_Last_Name (Object : in Member_Ref)
                 return String;

   --  Set the optional member's company name.
   procedure Set_Company (Object : in out Member_Ref;
                          Value  : in Ada.Strings.Unbounded.Unbounded_String);
   procedure Set_Company (Object : in out Member_Ref;
                          Value : in String);

   --  Get the optional member's company name.
   function Get_Company (Object : in Member_Ref)
                 return Ada.Strings.Unbounded.Unbounded_String;
   function Get_Company (Object : in Member_Ref)
                 return String;

   --  Set first adress field.
   procedure Set_Address1 (Object : in out Member_Ref;
                           Value  : in Ada.Strings.Unbounded.Unbounded_String);
   procedure Set_Address1 (Object : in out Member_Ref;
                           Value : in String);

   --  Get first adress field.
   function Get_Address1 (Object : in Member_Ref)
                 return Ada.Strings.Unbounded.Unbounded_String;
   function Get_Address1 (Object : in Member_Ref)
                 return String;

   --  Set second address field.
   procedure Set_Address2 (Object : in out Member_Ref;
                           Value  : in Ada.Strings.Unbounded.Unbounded_String);
   procedure Set_Address2 (Object : in out Member_Ref;
                           Value : in String);

   --  Get second address field.
   function Get_Address2 (Object : in Member_Ref)
                 return Ada.Strings.Unbounded.Unbounded_String;
   function Get_Address2 (Object : in Member_Ref)
                 return String;

   --  Set third address field.
   procedure Set_Address3 (Object : in out Member_Ref;
                           Value  : in Ada.Strings.Unbounded.Unbounded_String);
   procedure Set_Address3 (Object : in out Member_Ref;
                           Value : in String);

   --  Get third address field.
   function Get_Address3 (Object : in Member_Ref)
                 return Ada.Strings.Unbounded.Unbounded_String;
   function Get_Address3 (Object : in Member_Ref)
                 return String;

   --  Set address postal code.
   procedure Set_Postal_Code (Object : in out Member_Ref;
                              Value  : in Ada.Strings.Unbounded.Unbounded_String);
   procedure Set_Postal_Code (Object : in out Member_Ref;
                              Value : in String);

   --  Get address postal code.
   function Get_Postal_Code (Object : in Member_Ref)
                 return Ada.Strings.Unbounded.Unbounded_String;
   function Get_Postal_Code (Object : in Member_Ref)
                 return String;

   --  Set address tiown.
   procedure Set_City (Object : in out Member_Ref;
                       Value  : in Ada.Strings.Unbounded.Unbounded_String);
   procedure Set_City (Object : in out Member_Ref;
                       Value : in String);

   --  Get address tiown.
   function Get_City (Object : in Member_Ref)
                 return Ada.Strings.Unbounded.Unbounded_String;
   function Get_City (Object : in Member_Ref)
                 return String;

   --  Set the country.
   procedure Set_Country (Object : in out Member_Ref;
                          Value  : in Ada.Strings.Unbounded.Unbounded_String);
   procedure Set_Country (Object : in out Member_Ref;
                          Value : in String);

   --  Get the country.
   function Get_Country (Object : in Member_Ref)
                 return Ada.Strings.Unbounded.Unbounded_String;
   function Get_Country (Object : in Member_Ref)
                 return String;

   --  Set the date when the member record was created.
   procedure Set_Create_Date (Object : in out Member_Ref;
                              Value  : in Ada.Calendar.Time);

   --  Get the date when the member record was created.
   function Get_Create_Date (Object : in Member_Ref)
                 return Ada.Calendar.Time;

   --  Set the date when the member's email was validated.
   procedure Set_Mail_Verify_Date (Object : in out Member_Ref;
                                   Value  : in ADO.Nullable_Time);

   --  Get the date when the member's email was validated.
   function Get_Mail_Verify_Date (Object : in Member_Ref)
                 return ADO.Nullable_Time;

   --  Set the date when the payment was received.
   procedure Set_Payment_Date (Object : in out Member_Ref;
                               Value  : in ADO.Nullable_Time);

   --  Get the date when the payment was received.
   function Get_Payment_Date (Object : in Member_Ref)
                 return ADO.Nullable_Time;

   --
   procedure Set_Status (Object : in out Member_Ref;
                         Value  : in Status_Type);

   --
   function Get_Status (Object : in Member_Ref)
                 return Status_Type;

   --  Set whether the member is also member of Ada Europe.
   procedure Set_Ada_Europe (Object : in out Member_Ref;
                             Value  : in Boolean);

   --  Get whether the member is also member of Ada Europe.
   function Get_Ada_Europe (Object : in Member_Ref)
                 return Boolean;

   --  Set secure key salt.
   procedure Set_Salt (Object : in out Member_Ref;
                       Value  : in Ada.Strings.Unbounded.Unbounded_String);
   procedure Set_Salt (Object : in out Member_Ref;
                       Value : in String);

   --  Get secure key salt.
   function Get_Salt (Object : in Member_Ref)
                 return Ada.Strings.Unbounded.Unbounded_String;
   function Get_Salt (Object : in Member_Ref)
                 return String;

   --  Set date when the information was updated.
   procedure Set_Update_Date (Object : in out Member_Ref;
                              Value  : in Ada.Calendar.Time);

   --  Get date when the information was updated.
   function Get_Update_Date (Object : in Member_Ref)
                 return Ada.Calendar.Time;

   --  Set the subscription deadline
   procedure Set_Subscription_Deadline (Object : in out Member_Ref;
                                        Value  : in ADO.Nullable_Time);

   --  Get the subscription deadline
   function Get_Subscription_Deadline (Object : in Member_Ref)
                 return ADO.Nullable_Time;

   --  Set amount in euros
   procedure Set_Amount (Object : in out Member_Ref;
                         Value  : in Integer);

   --  Get amount in euros
   function Get_Amount (Object : in Member_Ref)
                 return Integer;

   --
   procedure Set_Receipt (Object : in out Member_Ref;
                          Value  : in Receipt_Ref'Class);

   --
   function Get_Receipt (Object : in Member_Ref)
                 return Receipt_Ref'Class;

   --  Set the member's email address.
   procedure Set_Email (Object : in out Member_Ref;
                        Value  : in AWA.Users.Models.Email_Ref'Class);

   --  Get the member's email address.
   function Get_Email (Object : in Member_Ref)
                 return AWA.Users.Models.Email_Ref'Class;

   --  Load the entity identified by 'Id'.
   --  Raises the NOT_FOUND exception if it does not exist.
   procedure Load (Object  : in out Member_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Id      : in ADO.Identifier);

   --  Load the entity identified by 'Id'.
   --  Returns True in <b>Found</b> if the object was found and False if it does not exist.
   procedure Load (Object  : in out Member_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Id      : in ADO.Identifier;
                   Found   : out Boolean);

   --  Reload from the database the same object if it was modified.
   --  Returns True in `Updated` if the object was reloaded.
   --  Raises the NOT_FOUND exception if it does not exist.
   procedure Reload (Object  : in out Member_Ref;
                     Session : in out ADO.Sessions.Session'Class;
                     Updated : out Boolean);

   --  Find and load the entity.
   overriding
   procedure Find (Object  : in out Member_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Query   : in ADO.SQL.Query'Class;
                   Found   : out Boolean);

   --  Save the entity.  If the entity does not have an identifier, an identifier is allocated
   --  and it is inserted in the table.  Otherwise, only data fields which have been changed
   --  are updated.
   overriding
   procedure Save (Object  : in out Member_Ref;
                   Session : in out ADO.Sessions.Master_Session'Class);

   --  Delete the entity.
   overriding
   procedure Delete (Object  : in out Member_Ref;
                     Session : in out ADO.Sessions.Master_Session'Class);

   overriding
   function Get_Value (From : in Member_Ref;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Table definition
   MEMBER_TABLE : constant ADO.Schemas.Class_Mapping_Access;

   --  Internal method to allocate the Object_Record instance
   overriding
   procedure Allocate (Object : in out Member_Ref);

   --  Copy of the object.
   procedure Copy (Object : in Member_Ref;
                   Into   : in out Member_Ref);


   --  --------------------
   --    Describes the change made on the adafr_member database record.
   --  --------------------
   type Audit_Info is
     new Util.Beans.Basic.Bean with  record

      --  the date of change.
      Date : Ada.Calendar.Time;

      --  the audit field name.
      Name : Ada.Strings.Unbounded.Unbounded_String;

      --  the old field value.
      Old_Value : ADO.Nullable_String;

      --  the new field value.
      New_Value : ADO.Nullable_String;

      --  the user who made the change.
      Author : ADO.Nullable_String;
   end record;

   --  Get the bean attribute identified by the name.
   overriding
   function Get_Value (From : in Audit_Info;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Set the bean attribute identified by the name.
   overriding
   procedure Set_Value (Item  : in out Audit_Info;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object);


   package Audit_Info_Beans is
      new Util.Beans.Basic.Lists (Element_Type => Audit_Info);
   package Audit_Info_Vectors renames Audit_Info_Beans.Vectors;
   subtype Audit_Info_List_Bean is Audit_Info_Beans.List_Bean;

   type Audit_Info_List_Bean_Access is access all Audit_Info_List_Bean;

   --  Run the query controlled by <b>Context</b> and append the list in <b>Object</b>.
   procedure List (Object  : in out Audit_Info_List_Bean'Class;
                   Session : in out ADO.Sessions.Session'Class;
                   Context : in out ADO.Queries.Context'Class);

   subtype Audit_Info_Vector is Audit_Info_Vectors.Vector;

   --  Run the query controlled by <b>Context</b> and append the list in <b>Object</b>.
   procedure List (Object  : in out Audit_Info_Vector;
                   Session : in out ADO.Sessions.Session'Class;
                   Context : in out ADO.Queries.Context'Class);

   Query_Adafr_Member_History : constant ADO.Queries.Query_Definition_Access;

   --  --------------------
   --    Describes the information about an Ada-France member.
   --  --------------------
   type Export_Member_Info is
     new Util.Beans.Basic.Bean with  record

      --  the member identifier.
      Id : ADO.Identifier;

      --  the member first name.
      First_Name : Ada.Strings.Unbounded.Unbounded_String;

      --  the member last name.
      Last_Name : Ada.Strings.Unbounded.Unbounded_String;

      --  the member status.
      Status : Status_Type;

      --  whether the member is member of Ada-Europe.
      Ada_Europe : Boolean;

      --  the date when the member was created.
      Create_Date : Ada.Calendar.Time;

      --  the date when the subscription was paied.
      Payment_Date : ADO.Nullable_Time;

      --  the date when the subscription terminates.
      Subscription_Deadline : ADO.Nullable_Time;

      --  the subscription amount paid.
      Amount : Integer;

      --  the member email address.
      Email : Ada.Strings.Unbounded.Unbounded_String;

      --  the company.
      Company : Ada.Strings.Unbounded.Unbounded_String;

      --  the first address line.
      Address1 : Ada.Strings.Unbounded.Unbounded_String;

      --  the second address line.
      Address2 : Ada.Strings.Unbounded.Unbounded_String;

      --  the third address line.
      Address3 : Ada.Strings.Unbounded.Unbounded_String;

      --  the postal code.
      Postal_Code : Ada.Strings.Unbounded.Unbounded_String;

      --  the city.
      City : Ada.Strings.Unbounded.Unbounded_String;

      --  the country.
      Country : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   --  Get the bean attribute identified by the name.
   overriding
   function Get_Value (From : in Export_Member_Info;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Set the bean attribute identified by the name.
   overriding
   procedure Set_Value (Item  : in out Export_Member_Info;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object);


   package Export_Member_Info_Beans is
      new Util.Beans.Basic.Lists (Element_Type => Export_Member_Info);
   package Export_Member_Info_Vectors renames Export_Member_Info_Beans.Vectors;
   subtype Export_Member_Info_List_Bean is Export_Member_Info_Beans.List_Bean;

   type Export_Member_Info_List_Bean_Access is access all Export_Member_Info_List_Bean;

   --  Run the query controlled by <b>Context</b> and append the list in <b>Object</b>.
   procedure List (Object  : in out Export_Member_Info_List_Bean'Class;
                   Session : in out ADO.Sessions.Session'Class;
                   Context : in out ADO.Queries.Context'Class);

   subtype Export_Member_Info_Vector is Export_Member_Info_Vectors.Vector;

   --  Run the query controlled by <b>Context</b> and append the list in <b>Object</b>.
   procedure List (Object  : in out Export_Member_Info_Vector;
                   Session : in out ADO.Sessions.Session'Class;
                   Context : in out ADO.Queries.Context'Class);

   Query_Adafr_Export_Member_List : constant ADO.Queries.Query_Definition_Access;

   --  --------------------
   --    Describes the information about an Ada-France member.
   --  --------------------
   type Member_Info is
     new Util.Beans.Basic.Bean with  record

      --  the member identifier.
      Id : ADO.Identifier;

      --  the member first name.
      First_Name : ADO.Nullable_String;

      --  the member last name.
      Last_Name : ADO.Nullable_String;

      --  the member status.
      Status : Status_Type;

      --  whether the member is member of Ada-Europe.
      Ada_Europe : ADO.Nullable_Boolean;

      --  the date when the member was created.
      Create_Date : ADO.Nullable_Time;

      --  the date when the invitation was sent to the user.
      Payment_Date : ADO.Nullable_Time;

      --  the date when the subscription terminates.
      Subscription_Deadline : ADO.Nullable_Time;

      --  the member email address.
      Email : Ada.Strings.Unbounded.Unbounded_String;

      --  the user login name.
      Login_Name : ADO.Nullable_String;

      --  the user role.
      Role : ADO.Nullable_String;
   end record;

   --  Get the bean attribute identified by the name.
   overriding
   function Get_Value (From : in Member_Info;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Set the bean attribute identified by the name.
   overriding
   procedure Set_Value (Item  : in out Member_Info;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object);


   package Member_Info_Beans is
      new Util.Beans.Basic.Lists (Element_Type => Member_Info);
   package Member_Info_Vectors renames Member_Info_Beans.Vectors;
   subtype Member_Info_List_Bean is Member_Info_Beans.List_Bean;

   type Member_Info_List_Bean_Access is access all Member_Info_List_Bean;

   --  Run the query controlled by <b>Context</b> and append the list in <b>Object</b>.
   procedure List (Object  : in out Member_Info_List_Bean'Class;
                   Session : in out ADO.Sessions.Session'Class;
                   Context : in out ADO.Queries.Context'Class);

   subtype Member_Info_Vector is Member_Info_Vectors.Vector;

   --  Run the query controlled by <b>Context</b> and append the list in <b>Object</b>.
   procedure List (Object  : in out Member_Info_Vector;
                   Session : in out ADO.Sessions.Session'Class;
                   Context : in out ADO.Queries.Context'Class);

   Query_Adafr_User_List : constant ADO.Queries.Query_Definition_Access;

   Query_Adafr_Member_List : constant ADO.Queries.Query_Definition_Access;

   Query_Adafr_Member_List_By_Status : constant ADO.Queries.Query_Definition_Access;

   Query_Adafr_Member_List_Expired : constant ADO.Queries.Query_Definition_Access;


   --  --------------------
   --    save the member in the database.
   --  --------------------
   type Member_Bean is abstract new Adafr.Members.Models.Member_Ref
     and Util.Beans.Basic.Bean and Util.Beans.Methods.Method_Bean with  record
      Email : Ada.Strings.Unbounded.Unbounded_String;

      --  validation key used to grant the register operation.
      Key : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   --  This bean provides some methods that can be used in a Method_Expression.
   overriding
   function Get_Method_Bindings (From : in Member_Bean)
                                 return Util.Beans.Methods.Method_Binding_Array_Access;

   --  Get the bean attribute identified by the name.
   overriding
   function Get_Value (From : in Member_Bean;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Set the bean attribute identified by the name.
   overriding
   procedure Set_Value (Item  : in out Member_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object);

   procedure Save (Bean : in out Member_Bean;
                  Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is abstract;

   procedure Send (Bean : in out Member_Bean;
                  Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is abstract;

   procedure Register (Bean : in out Member_Bean;
                      Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is abstract;

   procedure Validate (Bean : in out Member_Bean;
                      Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is abstract;

   procedure Load (Bean : in out Member_Bean;
                  Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is abstract;

   procedure Save_Payment (Bean : in out Member_Bean;
                          Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is abstract;

   procedure Create (Bean : in out Member_Bean;
                    Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is abstract;

   type Member_List_Bean is abstract limited
     new Util.Beans.Basic.Bean and Util.Beans.Methods.Method_Bean with  record

      --  the number of pages.
      Page : Integer;

      --  the total number of members found.
      Count : Integer;

      --  the list filter mode.
      Filter : Filter_Type;

      --  the number of members per page.
      Page_Size : Integer;
   end record;

   --  This bean provides some methods that can be used in a Method_Expression.
   overriding
   function Get_Method_Bindings (From : in Member_List_Bean)
                                 return Util.Beans.Methods.Method_Binding_Array_Access;

   --  Get the bean attribute identified by the name.
   overriding
   function Get_Value (From : in Member_List_Bean;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Set the bean attribute identified by the name.
   overriding
   procedure Set_Value (Item  : in out Member_List_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object);

   procedure Load (Bean : in out Member_List_Bean;
                  Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is abstract;


private
   RECEIPT_NAME : aliased constant String := "adafr_receipt";
   COL_0_1_NAME : aliased constant String := "id";
   COL_1_1_NAME : aliased constant String := "create_date";
   COL_2_1_NAME : aliased constant String := "amount";
   COL_3_1_NAME : aliased constant String := "member";

   RECEIPT_DEF : aliased constant ADO.Schemas.Class_Mapping :=
     (Count   => 4,
      Table   => RECEIPT_NAME'Access,
      Members => (
         1 => COL_0_1_NAME'Access,
         2 => COL_1_1_NAME'Access,
         3 => COL_2_1_NAME'Access,
         4 => COL_3_1_NAME'Access)
     );
   RECEIPT_TABLE : constant ADO.Schemas.Class_Mapping_Access
      := RECEIPT_DEF'Access;


   Null_Receipt : constant Receipt_Ref
      := Receipt_Ref'(ADO.Objects.Object_Ref with null record);

   type Receipt_Impl is
      new ADO.Objects.Object_Record (Key_Type => ADO.Objects.KEY_INTEGER,
                                     Of_Class => RECEIPT_DEF'Access)
   with record
       Create_Date : Ada.Calendar.Time;
       Amount : Integer;
       Member : ADO.Identifier;
   end record;

   type Receipt_Access is access all Receipt_Impl;

   overriding
   procedure Destroy (Object : access Receipt_Impl);

   overriding
   procedure Find (Object  : in out Receipt_Impl;
                   Session : in out ADO.Sessions.Session'Class;
                   Query   : in ADO.SQL.Query'Class;
                   Found   : out Boolean);

   overriding
   procedure Load (Object  : in out Receipt_Impl;
                   Session : in out ADO.Sessions.Session'Class);
   procedure Load (Object  : in out Receipt_Impl;
                   Stmt    : in out ADO.Statements.Query_Statement'Class;
                   Session : in out ADO.Sessions.Session'Class);

   overriding
   procedure Save (Object  : in out Receipt_Impl;
                   Session : in out ADO.Sessions.Master_Session'Class);

   overriding
   procedure Create (Object  : in out Receipt_Impl;
                     Session : in out ADO.Sessions.Master_Session'Class);

   overriding
   procedure Delete (Object  : in out Receipt_Impl;
                     Session : in out ADO.Sessions.Master_Session'Class);

   procedure Set_Field (Object : in out Receipt_Ref'Class;
                        Impl   : out Receipt_Access);
   MEMBER_NAME : aliased constant String := "adafr_member";
   COL_0_2_NAME : aliased constant String := "id";
   COL_1_2_NAME : aliased constant String := "version";
   COL_2_2_NAME : aliased constant String := "first_name";
   COL_3_2_NAME : aliased constant String := "last_name";
   COL_4_2_NAME : aliased constant String := "company";
   COL_5_2_NAME : aliased constant String := "address1";
   COL_6_2_NAME : aliased constant String := "address2";
   COL_7_2_NAME : aliased constant String := "address3";
   COL_8_2_NAME : aliased constant String := "postal_code";
   COL_9_2_NAME : aliased constant String := "city";
   COL_10_2_NAME : aliased constant String := "country";
   COL_11_2_NAME : aliased constant String := "create_date";
   COL_12_2_NAME : aliased constant String := "mail_verify_date";
   COL_13_2_NAME : aliased constant String := "payment_date";
   COL_14_2_NAME : aliased constant String := "status";
   COL_15_2_NAME : aliased constant String := "ada_europe";
   COL_16_2_NAME : aliased constant String := "salt";
   COL_17_2_NAME : aliased constant String := "update_date";
   COL_18_2_NAME : aliased constant String := "subscription_deadline";
   COL_19_2_NAME : aliased constant String := "amount";
   COL_20_2_NAME : aliased constant String := "receipt_id";
   COL_21_2_NAME : aliased constant String := "email_id";

   MEMBER_DEF : aliased constant ADO.Schemas.Class_Mapping :=
     (Count   => 22,
      Table   => MEMBER_NAME'Access,
      Members => (
         1 => COL_0_2_NAME'Access,
         2 => COL_1_2_NAME'Access,
         3 => COL_2_2_NAME'Access,
         4 => COL_3_2_NAME'Access,
         5 => COL_4_2_NAME'Access,
         6 => COL_5_2_NAME'Access,
         7 => COL_6_2_NAME'Access,
         8 => COL_7_2_NAME'Access,
         9 => COL_8_2_NAME'Access,
         10 => COL_9_2_NAME'Access,
         11 => COL_10_2_NAME'Access,
         12 => COL_11_2_NAME'Access,
         13 => COL_12_2_NAME'Access,
         14 => COL_13_2_NAME'Access,
         15 => COL_14_2_NAME'Access,
         16 => COL_15_2_NAME'Access,
         17 => COL_16_2_NAME'Access,
         18 => COL_17_2_NAME'Access,
         19 => COL_18_2_NAME'Access,
         20 => COL_19_2_NAME'Access,
         21 => COL_20_2_NAME'Access,
         22 => COL_21_2_NAME'Access)
     );
   MEMBER_TABLE : constant ADO.Schemas.Class_Mapping_Access
      := MEMBER_DEF'Access;

   MEMBER_AUDIT_DEF : aliased constant ADO.Audits.Auditable_Mapping :=
     (Count    => 14,
      Of_Class => MEMBER_DEF'Access,
      Members  => (
         1 => 2,
         2 => 3,
         3 => 4,
         4 => 5,
         5 => 6,
         6 => 7,
         7 => 8,
         8 => 9,
         9 => 10,
         10 => 12,
         11 => 13,
         12 => 14,
         13 => 15,
         14 => 19)
     );
   MEMBER_AUDIT_TABLE : constant ADO.Audits.Auditable_Mapping_Access
      := MEMBER_AUDIT_DEF'Access;

   Null_Member : constant Member_Ref
      := Member_Ref'(ADO.Objects.Object_Ref with null record);

   type Member_Impl is
      new ADO.Audits.Auditable_Object_Record (Key_Type => ADO.Objects.KEY_INTEGER,
                                     Of_Class => MEMBER_DEF'Access,
                                     With_Audit => MEMBER_AUDIT_DEF'Access)
   with record
       Version : Integer;
       First_Name : Ada.Strings.Unbounded.Unbounded_String;
       Last_Name : Ada.Strings.Unbounded.Unbounded_String;
       Company : Ada.Strings.Unbounded.Unbounded_String;
       Address1 : Ada.Strings.Unbounded.Unbounded_String;
       Address2 : Ada.Strings.Unbounded.Unbounded_String;
       Address3 : Ada.Strings.Unbounded.Unbounded_String;
       Postal_Code : Ada.Strings.Unbounded.Unbounded_String;
       City : Ada.Strings.Unbounded.Unbounded_String;
       Country : Ada.Strings.Unbounded.Unbounded_String;
       Create_Date : Ada.Calendar.Time;
       Mail_Verify_Date : ADO.Nullable_Time;
       Payment_Date : ADO.Nullable_Time;
       Status : Status_Type;
       Ada_Europe : Boolean;
       Salt : Ada.Strings.Unbounded.Unbounded_String;
       Update_Date : Ada.Calendar.Time;
       Subscription_Deadline : ADO.Nullable_Time;
       Amount : Integer;
       Receipt : Receipt_Ref;
       Email : AWA.Users.Models.Email_Ref;
   end record;

   type Member_Access is access all Member_Impl;

   overriding
   procedure Destroy (Object : access Member_Impl);

   overriding
   procedure Find (Object  : in out Member_Impl;
                   Session : in out ADO.Sessions.Session'Class;
                   Query   : in ADO.SQL.Query'Class;
                   Found   : out Boolean);

   overriding
   procedure Load (Object  : in out Member_Impl;
                   Session : in out ADO.Sessions.Session'Class);
   procedure Load (Object  : in out Member_Impl;
                   Stmt    : in out ADO.Statements.Query_Statement'Class;
                   Session : in out ADO.Sessions.Session'Class);

   overriding
   procedure Save (Object  : in out Member_Impl;
                   Session : in out ADO.Sessions.Master_Session'Class);

   overriding
   procedure Create (Object  : in out Member_Impl;
                     Session : in out ADO.Sessions.Master_Session'Class);

   overriding
   procedure Delete (Object  : in out Member_Impl;
                     Session : in out ADO.Sessions.Master_Session'Class);

   procedure Set_Field (Object : in out Member_Ref'Class;
                        Impl   : out Member_Access);

   package File_1 is
      new ADO.Queries.Loaders.File (Path => "adafr-member-history.xml",
                                    Sha1 => "1D0B2243B921CD6BF0FBCD19ADA0E1FE0392D18F");

   package Def_Auditinfo_Adafr_Member_History is
      new ADO.Queries.Loaders.Query (Name => "adafr-member-history",
                                     File => File_1.File'Access);
   Query_Adafr_Member_History : constant ADO.Queries.Query_Definition_Access
   := Def_Auditinfo_Adafr_Member_History.Query'Access;

   package File_2 is
      new ADO.Queries.Loaders.File (Path => "adafr-members-export.xml",
                                    Sha1 => "50EA1E34DC5A163A59D89631B1F5BF45A16DFB8D");

   package Def_Exportmemberinfo_Adafr_Export_Member_List is
      new ADO.Queries.Loaders.Query (Name => "adafr-export-member-list",
                                     File => File_2.File'Access);
   Query_Adafr_Export_Member_List : constant ADO.Queries.Query_Definition_Access
   := Def_Exportmemberinfo_Adafr_Export_Member_List.Query'Access;

   package File_3 is
      new ADO.Queries.Loaders.File (Path => "adafr-members.xml",
                                    Sha1 => "4AC5A1DCF0193D455D6866C6E9450788C890CA04");

   package Def_Memberinfo_Adafr_User_List is
      new ADO.Queries.Loaders.Query (Name => "adafr-user-list",
                                     File => File_3.File'Access);
   Query_Adafr_User_List : constant ADO.Queries.Query_Definition_Access
   := Def_Memberinfo_Adafr_User_List.Query'Access;

   package Def_Memberinfo_Adafr_Member_List is
      new ADO.Queries.Loaders.Query (Name => "adafr-member-list",
                                     File => File_3.File'Access);
   Query_Adafr_Member_List : constant ADO.Queries.Query_Definition_Access
   := Def_Memberinfo_Adafr_Member_List.Query'Access;

   package Def_Memberinfo_Adafr_Member_List_By_Status is
      new ADO.Queries.Loaders.Query (Name => "adafr-member-list-by-status",
                                     File => File_3.File'Access);
   Query_Adafr_Member_List_By_Status : constant ADO.Queries.Query_Definition_Access
   := Def_Memberinfo_Adafr_Member_List_By_Status.Query'Access;

   package Def_Memberinfo_Adafr_Member_List_Expired is
      new ADO.Queries.Loaders.Query (Name => "adafr-member-list-expired",
                                     File => File_3.File'Access);
   Query_Adafr_Member_List_Expired : constant ADO.Queries.Query_Definition_Access
   := Def_Memberinfo_Adafr_Member_List_Expired.Query'Access;
end Adafr.Members.Models;
