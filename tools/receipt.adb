-----------------------------------------------------------------------
--  receipt -- Import old Ada France Archives
--  Copyright (C) 2017, 2019 Ada France
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

with Adafr.Receipt;
procedure Receipt is

   use Ada.Strings.Unbounded;

   Member  : Adafr.Receipt.Information;
begin
   Member.Company := To_Unbounded_String ("Ada Conformity Assessment Authority");
   Member.First_Name := To_Unbounded_String ("Augusta Ada");
   Member.Last_Name := To_Unbounded_String ("King Byron");
   Member.Address1 := To_Unbounded_String ("Sainte Marie Magdalene");
   Member.Postal_Code := To_Unbounded_String ("NG15 8GD");
   Member.City := To_Unbounded_String ("Newstead Abbey");
   Member.Country := To_Unbounded_String ("England");
   Member.Amount := To_Unbounded_String ("65");
   Member.Date := To_Unbounded_String ("Jeudi 28 Mai 2020");
   Member.Ada_Europe := True;
   Adafr.Receipt.Create ("test-receipt.tex", "config/recu-cotisation.tmpl", Member);
   Adafr.Receipt.Generate ("test-receipt.tex");
end Receipt;
