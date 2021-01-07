--  sea-redstore-servers.adb ---

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

with Util.Serialize.IO.Form;
with Util.Http.Clients;
with Util.Http.Clients.Curl;

package body SeA.Redstore.Servers is

    procedure Delete_TTL (Server : in out Server_Type;
                          TTL_Str : String) is
        Redstore_Client : Util.Http.Clients.Client;
        Response : Util.Http.Clients.Response;

        Uri : constant String := To_String (Server.Host_URI) & "/delete";
        Form : Util.Http.Clients.Form_Data;

        use Util.Serialize.IO.Form;
    begin
        Util.Http.Clients.Initialize (Form, TTL_Str'Length + 500);
        Write_Attribute (Output_Stream (Form),
                         "content", TTL_Str);
        Write_Attribute (Output_Stream (Form),
                         "content-type", "turtle");
        Write_Attribute (Output_Stream (Form),
                         "base-uri", To_String (Server.Base_URI));
        Write_Attribute (Output_Stream (Form),
                         "graph", To_String (Server.Graph));

        Redstore_Client.Post (Uri, Form, Response);
        Server.Last_Response_Status := Response.Get_Status;
        Server.Last_Response_Body := To_Unbounded_String (Response.Get_Body);

        if Response.Get_Status < 200 or else Response.Get_Status >= 300 then
            raise Redstore_Answer_Error with
              "The redstore server answered with error status: "
                & Response.Get_Status'Image & " "
                & Response.Get_Body;
        end if;

    end Delete_TTL;

    procedure Get_Last_Response (Server : Server_Type;
                                 Status_Code : out Natural;
                                 Str_Body : out Unbounded_String) is
    begin
        Status_Code := Server.Last_Response_Status;
        Str_Body := Server.Last_Response_Body;
    end Get_Last_Response;

    procedure Initialize (Server : in out Server_Type;
                          Host_URI : String) is
    begin
        Server.Host_URI := To_Unbounded_String (Host_URI);
        Server.Last_Response_Status := 0;
        Server.Last_Response_Body := Null_Unbounded_String;
        Server.Base_URI := Null_Unbounded_String;
        Server.Graph := Null_Unbounded_String;
    end Initialize;

    procedure Insert_TTL (Server : in out Server_Type; TTL_Str : String) is
        Redstore_Client : Util.Http.Clients.Client;
        Response : Util.Http.Clients.Response;

        URI : constant String := To_String (Server.Host_URI) &
          "/insert";
        Form : Util.Http.Clients.Form_Data;

        use Util.Serialize.IO.Form;
    begin
        Util.Http.Clients.Initialize (Form, TTL_Str'Length + 500);
        Write_Attribute (Output_Stream (Form),
                         "content", TTL_Str);
        Write_Attribute (Output_Stream (Form),
                         "content-type", "turtle");
        Write_Attribute (Output_Stream (Form),
                         "base-uri", To_String (Server.Base_URI));
        Write_Attribute (Output_Stream (Form),
                         "graph", To_String (Server.Graph));

        Redstore_Client.Post (URI, Form, Response);
        Server.Last_Response_Status := Response.Get_Status;
        Server.Last_Response_Body := To_Unbounded_String (Response.Get_Body);

        if Response.Get_Status < 200 or else Response.Get_Status  >= 300 then
            raise Redstore_Answer_Error with
              "The redstore server answered with error status: "
                & Response.Get_Status'Image & " "
                & Response.Get_Body;
        end if;
    end Insert_TTL;

    procedure Query (Server : in out Server_Type; Query_Str : String;
                     Format : String := "xml";
                     Lang : String := "laqrs") is
        Redstore_Client : Util.Http.Clients.Client;
        Response : Util.Http.Clients.Response;

        Uri : constant String := To_String (Server.Host_URI) & "/sparql";
        Form : Util.Http.Clients.Form_Data;

        use Util.Serialize.IO.Form;
    begin
        Util.Http.Clients.Initialize (Form, Query_Str'Length + 500);
        Write_Attribute (Output_Stream (Form),
                         "query", Query_Str);
        Write_Attribute (Output_Stream (Form),
                         "format", Format);
        Write_Attribute (Output_Stream (Form),
                         "lang", Lang);
        Redstore_Client.Post (Uri, Form, Response);

        Server.Last_Response_Status := Response.Get_Status;
        Server.Last_Response_Body := To_Unbounded_String (Response.Get_Body);

        if Response.Get_Status < 200 or else Response.Get_Status >= 300 then
            raise Redstore_Answer_Error with
              "The redstore server answered with error status: "
                & Response.Get_Status'Image & " "
                & Response.Get_Body;
        end if;
    end Query;

    procedure Set_Graph (Server : in out Server_Type;
                         Graph : String) is
    begin
        Server.Graph := To_Unbounded_String (Graph);
    end Set_Graph;

begin
    Util.Http.Clients.Curl.Register;
end SeA.Redstore.Servers;
