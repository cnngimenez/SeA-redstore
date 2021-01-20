--  sea-redstore-templates.adb ---

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
with GNAT.Regpat;

with SeA.Redstore.Keyvalueparser;

package body SeA.Redstore.Templates is

    procedure Add_Value (Template : in out Template_Type;
                         Key : Unbounded_String;
                         Value : Unbounded_String) is
    begin
        Template.Fields.Include (Key, Value);
    end Add_Value;

    procedure Add_Value (Template : in out Template_Type;
                         Key : String;
                         Value : String) is
    begin
        Template.Add_Value (To_Unbounded_String (Key),
                            To_Unbounded_String (Value));
    end Add_Value;

    function Apply (Template : Template_Type) return String is
    begin
        return To_String (Template.Apply);
    end Apply;

    function Apply (Template : Template_Type) return Unbounded_String is
        Applied : Unbounded_String := Template.Text;
        use String_Hash_Pack;

        procedure Replace_String (Position : Cursor);

        procedure Replace_String (Position : Cursor) is
            Field_Name : constant String := To_String (Key (Position));
            Value : constant String := To_String (Element (Position));
            Field : constant String := "|" & Field_Name & "|";
            Field_Position : Natural := 0;
        begin
            Field_Position := Index (Applied, Field, 1);
            while Field_Position > 0 loop
                Replace_Slice (Applied,
                               Field_Position,
                               Field_Position + Field'Length - 1,
                               Value);
                Field_Position := Index (Applied, Field,
                                         Field_Position + Field'Length);
            end loop;
        end Replace_String;

    begin
        Template.Fields.Iterate (Replace_String'Access);

        return Applied;
    end Apply;

    procedure Fill_Fields (Template : in out Template_Type) is
        use GNAT.Regpat;
        Str : constant String := To_String (Template.Text);
        Matches : Match_Array (0 .. 1);
        Regexp : constant Pattern_Matcher :=
          Compile (Field_Pattern);
        Current : Natural := Str'First;
        Empty_Str : constant Unbounded_String := To_Unbounded_String ("");
        Field_Name : Unbounded_String;
    begin
        Template.Fields.Clear;
        loop
            Match (Regexp, Str, Matches, Current);
            exit when Matches (0) = No_Match;

            Field_Name := To_Unbounded_String
              (Str (Matches (1).First .. Matches (1).Last));
            Template.Fields.Include (Field_Name, Empty_Str);

            Current := Matches (0).Last + 1;
        end loop;
    end Fill_Fields;

    function Get_Value (Template : Template_Type;
                        Key : String) return String is

    begin
        return To_String (Template.Get_Value (To_Unbounded_String (Key)));
    end Get_Value;

    function Get_Value (Template : Template_Type;
                        Key : Unbounded_String) return Unbounded_String is
    begin
        return Template.Fields.Element (Key);
    end Get_Value;

    procedure Initialize_With_File (Template : in out Template_Type;
                                    Filepath : String) is

        function Read_File (Filepath : String) return Unbounded_String;

        function Read_File (Filepath : String) return Unbounded_String is
            use Ada.Text_IO;
            Temp : Unbounded_String;
            File : File_Type;
            Ch : Character;
        begin
            Open (File, In_File, Filepath);
            loop
                Get_Immediate (File, Ch);
                exit when End_Of_File (File);
                Append (Temp, Ch);
            end loop;
            Close (File);

            return Temp;
        end Read_File;

    begin
        Template.Text := Read_File (Filepath);
        Fill_Fields (Template);
    end Initialize_With_File;

    procedure Iterate_Fields (Template : Template_Type;
                              Proc : not null access
                                procedure (Field_Name : String;
                                           Field_Value : String)) is
        use String_Hash_Pack;

        procedure Call_Process (Position : Cursor);

        procedure Call_Process (Position : Cursor) is
        begin
            Proc (To_String (Key (Position)),
                  To_String (Element (Position)));
        end Call_Process;

    begin
        Template.Fields.Iterate (Call_Process'Access);
    end Iterate_Fields;

    procedure Read_Value_File (Template : in out Template_Type;
                               Filepath : String) is
        use Ada.Text_IO;
        use SeA.Redstore.Keyvalueparser;

        File : File_Type;
    begin
        Open (File, In_File, Filepath);
        loop
            declare
                Line : constant String := Get_Line (File);
            begin
                Template.Add_Value (Field_Name (Line), Field_Value (Line));
                exit when End_Of_File (File);
            end;
        end loop;
        Close (File);
    end Read_Value_File;

end SeA.Redstore.Templates;
