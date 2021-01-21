--  redstore_command.adb ---

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
with Ada.Strings;
use Ada.Strings;
with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

with SeA.Redstore.Templates;
use SeA.Redstore.Templates;
with SeA.Redstore.Servers;
with SeA.Redstore.Servers.Configs;
use SeA;

procedure Redstore_Command is

    function Valid_Format (Format : String) return String;
    function Valid_Lang (Lang : String) return String;
    procedure Send_Command (Command : String;
    Query_Format : String := "";
                            Query_Lang : String := "");
    procedure Show_Help;

    Template : Template_Type;

    procedure Send_Command (Command : String;
                            Query_Format : String := "";
                            Query_Lang : String := "") is
        Template_Data : constant String := Template.Apply;

        Server : Redstore.Servers.Server_Type;
        Status_Code : Natural;
        Str_Body : Unbounded_String;
    begin
        Redstore.Servers.Configs.Load ("./run/redstore.conf", Server);

        Put_Line ("Server configuration:");
        Put_Line ("- Host: " & Server.Get_Host_URI);
        Put_Line ("- Graph: " & Server.Get_Graph);
        Put_Line ("- Base URI: " & Server.Get_Base_URI);
        New_Line;

        Put_Line ("Sending data:");
        Put_Line ("```");
        Put_Line (Template_Data);
        Put_Line ("```");

        if Command = "query" then
            Put_Line ("Querying...");
            Put_Line ("Using 【" & Valid_Format (Query_Format)
                        & "】 query format and 【"
                        & Valid_Lang (Query_Lang)
                        & "】 query language.");
            Server.Query (Template_Data, Valid_Format (Query_Format),
            Valid_Lang (Query_Lang));
        elsif Command = "insert" then
            Put_Line ("Inserting tuples...");
            Server.Insert_TTL (Template_Data);
        elsif Command = "delete" then
            Put_Line  ("Deleting tuples...");
            Server.Delete_TTL (Template_Data);
        end if;

        Server.Get_Last_Response (Status_Code, Str_Body);
        Put_Line ("Server response: " & Status_Code'Image);
        New_Line;
        Put_Line (To_String (Str_Body));
    end Send_Command;

    procedure Show_Help is
    begin
        Put_Line ("Send a command to the restore server.");
        New_Line;
        Put_Line ("Synopsis:");
        Put_Line ("    redstore_command COMMAND TEMPLATE_PATH NAMEVALUE_PATH "
            & "[QUERY_FORMAT] [QUERY_LANG]");
        New_Line;
        Put_Line ("COMMAND := (query | insert | delete)");
        Put_Line ("    The command to send.");
        Put_Line ("TEMPLATE_PATH : The file path.");
        Put_Line ("NAMEVALUE_PATH : The file with ""FIELD_NAME := VALUE"" "
                    & "strings. If it is ""-"", then query each field and "
                    & "read their values from standard in.");
    end Show_Help;

    function Valid_Format (Format : String) return String is
    begin
        if Format = "" then
            return "xml";
        else
            return Format;
        end if;
    end Valid_Format;

    function Valid_Lang (Lang : String) return String is
    begin
        if Lang = "" then
            return "laqrs";
        else
            return Lang;
        end if;
    end Valid_Lang;

begin
    if Argument_Count < 3 then
        Show_Help;
        return;
    end if;

    Template.Initialize_With_File (Argument (2));
    if Argument (3) = "-" then
        Template.Read_Value_Stdin;
    else
        Template.Read_Value_File (Argument (3));
    end if;

    if Argument_Count >= 5 then
        Send_Command (Argument (1), Argument (4), Argument (5));
    elsif Argument_Count >= 4 then
        Send_Command (Argument (1), Argument (4));
    else
        Send_Command (Argument (1));
    end if;

end Redstore_Command;
