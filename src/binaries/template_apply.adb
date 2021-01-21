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

with SeA.Redstore.Templates;
use SeA.Redstore.Templates;

procedure Template_Apply is
    Template : Template_Type;
begin
    if Argument_Count < 2 then
        Put_Line ("Synopsis: template_apply TEMPLATE_PATH NAMEVALUE_PATH");
        return;
    end if;

    Template.Initialize_With_File (Argument (1));
    Template.Read_Value_File (Argument (2));

    Put_Line (Template.Apply);
end Template_Apply;
