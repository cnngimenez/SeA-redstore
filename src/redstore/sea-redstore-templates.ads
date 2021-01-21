--  sea-redstore-templates.ads ---

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
with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded.Hash;

package SeA.Redstore.Templates is

    type Template_Type is tagged private;

    procedure Initialize_With_File (Template : in out Template_Type;
                                    Filepath : String);
    procedure Add_Value (Template : in out Template_Type;
                         Key : Unbounded_String;
                         Value : Unbounded_String);
    procedure Add_Value (Template : in out Template_Type;
                         Key : String;
                         Value : String);
    function Get_Value (Template : Template_Type;
                        Key : String) return String;
    function Get_Value (Template : Template_Type;
                        Key : Unbounded_String) return Unbounded_String;
    function Apply (Template : Template_Type) return String;
    function Apply (Template : Template_Type) return Unbounded_String;

    procedure Iterate_Fields (Template : Template_Type;
                              Proc : not null access
                                procedure (Field_Name : String;
                                           Field_Value : String));
    procedure Read_Value_File (Template : in out Template_Type;
                               Filepath : String);
    procedure Read_Value_Stdin (Template : in out Template_Type);

private

    package String_Hash_Pack is new Ada.Containers.Hashed_Maps
      (
       Key_Type => Unbounded_String,
       Element_Type => Unbounded_String,
       Hash => Ada.Strings.Unbounded.Hash,
       Equivalent_Keys => "="
      );

    type Template_Type is tagged record
        Text : Unbounded_String;
        Fields : String_Hash_Pack.Map;
    end record;

    Field_Pattern : constant String := "\|([[:alnum:]]+)\|";

    --  Reset Fields and add all "|fieldname|" strings founded to it.
    procedure Fill_Fields (Template : in out Template_Type);

end SeA.Redstore.Templates;
