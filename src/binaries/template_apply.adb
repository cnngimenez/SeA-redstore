--  template_apply.adb ---

--  Copyright 2020 cnngimenez
--
--  Author: cnngimenez

--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.

--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.

--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.

-------------------------------------------------------------------------

with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Command_Line;
use Ada.Command_Line;
with Ada.Strings.Fixed;
use Ada.Strings.Fixed;
with Ada.Strings;
use Ada.Strings;

with SeA.Redstore.Templates;
use SeA.Redstore.Templates;

procedure Template_Apply is

    function Field_Name (Line : String) return String;
    function Field_Value (Line : String) return String;
    procedure Gather_Fields;

    Malformed_Input : exception;
    Template : Template_Type;

    function Field_Name (Line : String) return String is
        Pos : Natural;
    begin
        Pos := Index (Line, ":=");
        if Pos = 0 then
            raise Malformed_Input with
              "Expecting: ""field_name := field_value"" input.";
        end if;

        return Trim (Line (Line'First .. Pos), Both);
    end Field_Name;

    function Field_Value (Line : String) return String is
        Pos : Natural;
    begin
        Pos := Index (Line, ":=");
        if Pos = 0 then
            raise Malformed_Input with
              "Expecting: ""field_name := field_value"" input.";
        end if;

        return Trim (Line (Pos + 2 .. Line'Last), Both);
    end Field_Value;

    procedure Gather_Fields is
    begin
        loop
            declare
                Line : constant String := Get_Line;
            begin
                exit when End_Of_File;

                Template.Add_Value (Field_Name (Line),
                                    Field_Value (Line));
            end;
        end loop;
    end Gather_Fields;

begin
    if Argument_Count < 1 then
        Put_Line ("Synopsis: template_apply TEMPLATE_PATH");
        return;
    end if;

    Template.Initialize_With_File (Argument (1));
    Gather_Fields;
    Put_Line (Template.Apply);
end Template_Apply;
