(**
 **************************************************************************
 *                                                                        *
 *  This file is part of the uktat Developer's Component Library.        *
 *                                                                        *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution, *
 *  for details about the copyright.                                      *
 *                                                                        *
 *  This program is distributed in the hope that it will be useful,       *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *
 *                                                                        *
 **************************************************************************
**)

unit uktlibc;

interface
uses
  SysUtils,
  dummy;

  function memcmp(const A, B: pointer; S: integer): integer;

implementation

function memcmp(const A, B: pointer; S: integer): integer;
var I: Integer; Match: Boolean;
begin
  Result := -1;

  Match := true;
  I := 0;
  while (Match and (I < S)) do
  begin
    Match := (pbyte(A) = pbyte(b));
    Inc(I);
  end;

  if (not Match) then
  begin
    if (pbyte(A) < pbyte(b))
      then Result := -1
      else Result := +1;
  end;
end;

end.
