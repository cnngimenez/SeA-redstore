--  query.adb ---

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
with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

with SeA.Redstore.Servers;
with SeA.Redstore.Servers.Configs;
use SeA;

procedure Query is

    function Read_Query return String;
    procedure Send_Query (Query_Str : String);

    function Read_Query return String is
        Ch : Character;
        Input : Unbounded_String;
    begin
        while not End_Of_File loop
            Get_Immediate (Ch);
            Append (Input, Ch);
        end loop;

        return To_String (Input);
    end Read_Query;

    procedure Send_Query (Query_Str : String) is
        Server : Redstore.Servers.Server_Type;
        Status_Code : Natural;

        Str_Body : Unbounded_String;
    begin
        Redstore.Servers.Configs.Load ("./run/redstore.conf", Server);

        Server.Query (Query_Str);

        Server.Get_Last_Response (Status_Code, Str_Body);
        Put_Line ("Server response:" & Status_Code'Image);
        Put_Line (To_String (Str_Body));
    end Send_Query;

begin
    declare
        Query_Str : constant String := Read_Query;
    begin
        Send_Query (Query_Str);
    end;
end Query;
