--  template_filler.adb ---

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

with SeA.Redstore.Templates;
use SeA.Redstore.Templates;

procedure Template_Filler is
    procedure Put_Field (Field_Name : String; Field_Value : String);
    procedure Show_Help;

    Template : Template_Type;

    procedure Put_Field (Field_Name : String; Field_Value : String) is
        pragma Unreferenced (Field_Value);
    begin
        Put (Field_Name);
        Put (" := ");

        declare
            Value : constant String := Get_Line;
        begin
            Put_Line (Value);
        end;
    end Put_Field;

    procedure Show_Help is
    begin
        Put_Line ("Display the fields founded in the template file and ask for"
                    & " their value");
        New_Line;
        Put_Line ("Synopsis:");
        Put_Line ("    template_apply TEMPLATE_FILE_PATH");
        New_Line;
        Put_Line ("TEMPLATE_FILE_PATH is the template with ""|FIELDNAME|"" "
                    & "strings inside the file.");
    end Show_Help;

begin
    if Argument_Count < 1 then
        Show_Help;
        return;
    end if;

    Template.Initialize_With_File (Argument (1));

    Template.Iterate_Fields (Put_Field'Access);
end Template_Filler;
