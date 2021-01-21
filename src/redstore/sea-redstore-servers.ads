--  sea-redstore-servers.ads ---

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

with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

--
--  Provide Server_Type types to connect to a Redstore server.
--
package SeA.Redstore.Servers is

    type Server_Type is tagged private;

    --  URI: The protocol, host y port. Example: http://example.com:8080
    procedure Initialize (Server : in out Server_Type;
                          Host_URI : String);

    procedure Query (Server : in out Server_Type; Query_Str : String;
                     Format : String := "xml";
                     Lang : String := "laqrs");

    --  Raises Redstore_Answer_Error when the GET status received is not a
    --  between 200 and 299.
    procedure Insert_TTL (Server : in out Server_Type; TTL_Str : String);
    procedure Delete_TTL (Server : in out Server_Type; TTL_Str : String);
    --  procedure Query_Sparql (Server : in out Server_Type;
    --                          Sparql : String;
    --                          Results : out String);

    --  ----------
    --  Getters and setters
    --  ----------

    --  Get the last response information.
    procedure Get_Last_Response (Server : Server_Type;
                                 Status_Code : out Natural;
                                 Str_Body : out Unbounded_String);

    function Get_Graph (Server : Server_Type) return String;
    procedure Set_Graph (Server : in out Server_Type;
                         Graph : String);

    function Get_Base_URI (Server : Server_Type) return String;
    function Get_Host_URI (Server : Server_Type) return String;

    --  ----------
    --  Exceptions
    --  ----------

    Redstore_Answer_Error : exception;

private

    type Server_Type is tagged record
        Host_URI : Unbounded_String;
        Last_Response_Status : Natural;
        Last_Response_Body : Unbounded_String;
        Graph : Unbounded_String;
        Base_URI : Unbounded_String;
    end record;

end SeA.Redstore.Servers;
