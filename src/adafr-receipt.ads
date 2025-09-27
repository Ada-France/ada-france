-----------------------------------------------------------------------
--  adafr-receipt -- Receipt generation
--  Copyright (C) 2020, 2025 Ada France
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
package Adafr.Receipt is

   use Ada.Strings.Unbounded;

   --  Template configuration parameters.
   type Information is record
      Company     : Unbounded_String; --  @_COMPANY_@
      First_Name  : Unbounded_String; --  @_FIRST_NAME_@
      Last_Name   : Unbounded_String; --  @_LAST_NAME_@
      Address1    : Unbounded_String; --  @_ADDRESS1_@
      Address2    : Unbounded_String; --  @_ADDRESS2_@
      Address3    : Unbounded_String; --  @_ADDRESS3_@
      Postal_Code : Unbounded_String; --  @_POSTAL_CODE_@
      City        : Unbounded_String; --  @_CITY_@
      Country     : Unbounded_String; --  @_COUNTRY_@
      Amount      : Unbounded_String; --  @_AMOUNT_@
      Date        : Unbounded_String; --  @_DATE_@
      Ada_Europe  : Boolean;          --  @_ADA_EUROPE_@
      Ada_User_Society : Boolean;     --  @_ADA_USER_SOCIETY_@
      Signature   : Unbounded_String; --  @_SIGNATURE_@
   end record;

   --  Sign the information with the key.
   procedure Sign (Info : in out Information;
                   Key  : in String);

   --  Create the ConTeX receipt file by using the template and configuration.
   procedure Create (Path     : in String;
                     Template : in String;
                     Member   : in Information);

   --  Generate the receipt PDF file by running 'context --noconsole <path>'.
   procedure Generate (Path : in String);

end Adafr.Receipt;
