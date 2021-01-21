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
with Ada.IO_Exceptions;
with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;
with Ada.Command_Line;
use Ada.Command_Line;

with SeA.Redstore.Servers;
with SeA.Redstore.Servers.Configs;
use SeA;

procedure Query is

    function Read_Query return String;
    procedure Send_Query (Query_Str : String;
                          Format : String;
                          Lang : String);
    procedure Show_Help;

    function Read_Query return String is
        Ch : Character;
        Input : Unbounded_String;
    begin
        while not End_Of_File loop
            Get_Immediate (Ch);
            Append (Input, Ch);
        end loop;

        return To_String (Input);

    exception
    when Ada.IO_Exceptions.End_Error =>
        return To_String (Input);
    end Read_Query;

    procedure Send_Query (Query_Str : String;
                          Format : String;
                          Lang : String) is
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

        Server.Query (Query_Str, Format, Lang);

        Server.Get_Last_Response (Status_Code, Str_Body);
        Put_Line ("Server response:" & Status_Code'Image);
        Put_Line (To_String (Str_Body));
    end Send_Query;

    procedure Show_Help is
    begin
        Put_Line ("Make a query to the redstore server.");
        New_Line;
        Put_Line ("Synopsis:");
        Put_Line ("    query [FORMAT] [LANGUAGE]");
        New_Line;
        Put_Line ("The query text is entered as standard input.");
        Put_Line ("- FORMAT :: The results format. table by default. "
                    & "Redstore usually accepts: xml, json, table, csv, mkr, "
                    & "tsv, htlm, turtle, rdfxml, ntriples, rdfxml-xmp, "
                    & "rdfxml-abbrev, rss-1.0, atom, dot, json-triples, "
                    & "nquads.");
        Put_Line ("- LANGUAGE :: The query language used. laqrs by default. "
                    & "Redstore usually accepts: sparql10, sparql, "
                    & "sparql11-query, sparql11-update, laqrs.");
    end Show_Help;

    Format, Lang : Unbounded_String;

begin
    if Argument_Count >= 1 then
        Format := To_Unbounded_String (Argument (1));
    else
        Format := To_Unbounded_String ("table");
    end if;
    if Argument_Count >= 2 then
        Lang := To_Unbounded_String (Argument (2));
    else
        Lang := To_Unbounded_String ("laqrs");
    end if;

    if Format = "help" or else Format = "--help" then
        Show_Help;
        return;
    end if;

    Put_Line ("Querying using the 【"
                & To_String (Lang) & "】language and requesting the 【"
                & To_String (Format) & "】format for answer");
    New_Line;

    declare
        Query_Str : constant String := Read_Query;
    begin
        Send_Query (Query_Str,
                    To_String (Format),
                    To_String (Lang));
    end;
end Query;
