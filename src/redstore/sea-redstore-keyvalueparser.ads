--  sea-redstore-keyvalueparser.ads ---

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

--
--  A simple parser for configuration files.
--
package SeA.Redstore.Keyvalueparser is

    --  Extract the field name from a "NAME := VALUE" line.
    --
    --  Raises Malformed_Input exception if ":=" is not founded.
    function Field_Name (Line : String) return String;

    --  Extract the field value from a "NAME := VALUE" line.
    --
    --  Raises Malformed_Input exception if ":=" is not founded.
    function Field_Value (Line : String) return String;

    Malformed_Input : exception;

end SeA.Redstore.Keyvalueparser;
