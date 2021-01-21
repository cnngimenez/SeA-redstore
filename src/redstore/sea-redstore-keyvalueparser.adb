--  sea-redstore-keyvalueparser.adb ---

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

with Ada.Strings;
use Ada.Strings;
with Ada.Strings.Fixed;
use Ada.Strings.Fixed;

package body SeA.Redstore.Keyvalueparser is
    function Field_Name (Line : String) return String is
        Pos : Natural;
    begin
        if Trim (Line, Both) = "" then
            return "";
        end if;

        Pos := Index (Line, ":=");
        if Pos = 0 then
            raise Malformed_Input with
              "Expecting: ""field_name := field_value"" input in line:" & Line;
        end if;

        return Trim (Line (Line'First .. Pos -  1), Both);
    end Field_Name;

    function Field_Value (Line : String) return String is
        Pos : Natural;
    begin
        if Trim (Line, Both) = "" then
            return "";
        end if;

        Pos := Index (Line, ":=");
        if Pos = 0 then
            raise Malformed_Input with
              "Expecting: ""field_name := field_value"" input in line:" & Line;
        end if;

        return Trim (Line (Pos + 2 .. Line'Last), Both);
    end Field_Value;
end SeA.Redstore.Keyvalueparser;
