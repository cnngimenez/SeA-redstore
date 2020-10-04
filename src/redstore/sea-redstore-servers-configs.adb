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

package body SeA.Redstore.Servers.Configs is

    procedure Load (Filepath : String; Server : out Server_Type) is
        File : File_Type;
    begin
        if not Ada.Directories.Exists (Filepath) then
            return;
        end if;

        Open (File, In_File, Filepath);

        declare
            Host : constant String := Get_Line (File);
            Graph : constant String := Get_Line (File);
            Base_URI : constant String := Get_Line (File);
        begin
            --  Put_Line (Host);
            --  Put_Line (Graph);
            --  Put_Line (Base_URI);
            Server.Host_URI := To_Unbounded_String (Host);
            Server.Graph := To_Unbounded_String (Graph);
            Server.Base_URI := To_Unbounded_String (Base_URI);
        end;

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
