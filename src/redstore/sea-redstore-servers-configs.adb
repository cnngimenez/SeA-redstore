--  sea-redstore-servers-configs.adb ---

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
with Ada.Directories;

with SeA.Redstore.Keyvalueparser;

package body SeA.Redstore.Servers.Configs is

    procedure Load (Filepath : String; Server : out Server_Type) is
        use SeA.Redstore.Keyvalueparser;
        File : File_Type;
    begin
        if not Ada.Directories.Exists (Filepath) then
            raise Config_File_Not_Founded with
              "The configuration file """
                & Filepath & """ has not been founded";
        end if;

        Open (File, In_File, Filepath);

        while not End_Of_File (File) loop
            declare
                Line : constant String := Get_Line (File);
                Field : constant String := Field_Name (Line);
                Value : constant String := Field_Value (Line);
            begin
                if Field = "Host" then
                    Server.Host_URI := To_Unbounded_String (Value);
                elsif Field = "Graph" then
                    Server.Graph := To_Unbounded_String (Value);
                elsif Field = "Base_URI" then
                    Server.Base_URI := To_Unbounded_String (Value);
                end if;
            end;
        end loop;

        Close (File);
    end Load;

    procedure Save (Server : Server_Type; Filepath : String) is
        File : File_Type;
    begin
        Create (File, Out_File, Filepath);
        Put_Line (File, To_String (Server.Host_URI));
        Put_Line (File, To_String (Server.Graph));
        Put_Line (File, To_String (Server.Base_URI));
        Close (File);
    end Save;

end SeA.Redstore.Servers.Configs;
