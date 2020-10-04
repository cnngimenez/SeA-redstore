--  send_ttl.adb ---

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
with Ada.Command_Line;
use Ada.Command_Line;
with Ada.Directories;

with SeA.Redstore.Servers;
with SeA.Redstore.Servers.Configs;
use SeA;

procedure Send_Ttl is

    procedure Show_Help;
    procedure Send_Ttl_File (Ttl_File : String);
    function Read_File (Ttl_File : String) return String;

    function Read_File (Ttl_File : String) return String is
        File : File_Type;
        Lf_Char : constant Character := Character'Val (10);
        Text : Unbounded_String;
    begin
        Open (File, In_File, Ttl_File);

        while not End_Of_File (File) loop
            declare
                Line : constant String := Get_Line (File);
            begin
                Append (Text, Line);
                Append (Text, Lf_Char);
            end;
        end loop;

        return To_String (Text);
    end Read_File;

    procedure Send_Ttl_File (Ttl_File : String) is
        Server : Redstore.Servers.Server_Type;
        Status_Code : Natural;
        Str_Body : Unbounded_String;

        Turtle_Data : constant String := Read_File (Ttl_File);
    begin
        Redstore.Servers.Configs.Load ("./run/redstore.conf", Server);

        Server.Insert_TTL (Turtle_Data);

        Server.Get_Last_Response (Status_Code, Str_Body);
        Put_Line ("Server response:" & Status_Code'Image);
        Put_Line (To_String (Str_Body));
    end Send_Ttl_File;

    procedure Show_Help is
    begin
        Put_Line ("Synopsis:");
        Put_Line ("    bin/send_ttl TURTLE_FILE");
        Put_Line ("");
        Put_Line ("The file run/redstore.conf must be edited with the "
                    & "connection data.");
        Put_Line ("");
    end Show_Help;

begin
    if Argument_Count /= 1 then
        Show_Help;
        return;
    end if;

    declare
        Ttl_File : constant String := Argument (1);
    begin
        if not Ada.Directories.Exists (Ttl_File) then
            Put_Line ("The Turtle file does not exists!");
            return;
        end if;

        Send_Ttl_File (Ttl_File);
    end;

end Send_Ttl;
