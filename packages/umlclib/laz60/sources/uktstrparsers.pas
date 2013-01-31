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

unit uktstrparsers;

interface
uses
  uktAnsicharsets, uktStrings, uktTextConsts,
  uktAnsicharsetconsts,
  dummy;

  function ParseCharSingle
    (const Value: string; var Index: Integer; ValidChar: ansichar): Boolean;
  function ParseSingle
    (const Value: string; var Index: Integer; ValidChars: ansicharset): Boolean;

  function ParseCharWhile
    (const Value: string; var Index: Integer; ValidChar: ansichar): Boolean;
  function ParseCharUntil
    (const Value: string; var Index: Integer; BreakChar: ansichar): Boolean;

  function ParseWhile
   (const Value: string; var Index: Integer; ValidChars: ansicharset): Boolean;
  function ParseUntil
   (const Value: string; var Index: Integer; BreakChars: ansicharset): Boolean;

  function ParseQuotedStr(const Value: string; var Index: Integer): Boolean;
  function ParseUnsigned(const Value: string; var Index: Integer): Boolean;
  function ParseIdentifier(const Value: string; var Index: Integer): Boolean;
  function ParseQualified(const Value: string; var Index: Integer): Boolean;
  function ParseCount(const Value: string; var Index: Integer; Count: Integer): Boolean;

  function IsQuotedStr(const Value: string): Boolean;
  function IsUnsigned(const Value: string): Boolean;
  function IsIdentifier(const Value: string): Boolean;

  function ExtractQuotedStr
    (const Source: string; var Dest: string; var Index: Integer): Boolean;
  function ExtractUnsigned
    (const Source: string; var Dest: string; var Index: Integer): Boolean;
  function ExtractIdentifier
    (const Source: string; var Dest: string; var Index: Integer): Boolean;
  function ExtractQualified
    (const Source: string; var Dest: string; var Index: Integer): Boolean;
  function ExtractCount
    (const Source: string; var Dest: string; var Index: Integer; Count: Integer): Boolean;

implementation

function ParseCharSingle
  (const Value: string; var Index: Integer; ValidChar: ansichar): Boolean;
var Backup: Integer; L: Integer;
begin
  Backup := Index;
  // backup pointer
  // respaldar apuntador

  L := Length(Value);
  if ((Index <= L) and (Value[Index] = ValidChar))
    then Inc(Index);
  // try parsing
  // intenta parsear

  Result := (Index > Backup);
  // verify any changes
  // revisa si hubo cambios

  if (not Result)
    then Index := Backup;
  // restore pointer
  // restaurar apuntador

  // Goal: Returns a single ansicharacter.
  // Objetivo: Regresa un solo caracter.
end;

function ParseSingle
  (const Value: string; var Index: Integer; ValidChars: ansicharset): Boolean;
var Backup: Integer; L: Integer;
begin
  Backup := Index;
  // backup pointer
  // respaldar apuntador

  L := Length(Value);
  if ((Index <= L) and IsMember(Value[Index], ValidChars))
    then Inc(Index);
  // try parsing
  // intenta parsear

  Result := (Index > Backup);
  // verify any changes
  // revisa si hubo cambios

  if (not Result)
    then Index := Backup;
  // restore pointer
  // restaurar apuntador

  // Goal: Returns a single ansicharacter.
  // Objetivo: Regresa un solo caracter.
end;

function ParseCharWhile
  (const Value: string; var Index: Integer; ValidChar: ansichar): Boolean;
var Backup: Integer; L: Integer;
begin
  Backup := Index;
  // backup pointer
  // respaldar apuntador

  L := Length(Value);
  while ((Index <= L) and (Value[Index] = ValidChar)) do
   Inc(Index);
  // try parsing
  // intenta parsear

  Result := (Index > Backup);
  // verify any changes
  // revisa si hubo cambios

  if (not Result)
    then Index := Backup;
  // restore pointer
  // restaurar apuntador

  // Goal: Returns a group of ansicharacters.
  // Objetivo: Regresa un grupo de caracteres.
end;

function ParseCharUntil
  (const Value: string; var Index: Integer; BreakChar: ansichar): Boolean;
var Backup: Integer; L: Integer;
begin
  Backup := Index;
  // backup pointer
  // respaldar apuntador

  L := Length(Value);
  while (Index <= L) and (Value[Index] <> BreakChar) do
   Inc(Index);
  // try parsing
  // intenta parsear

  Result := (Index > Backup);
  // verify any changes
  // revisa si hubo cambios

  if (not Result)
    then Index := Backup;
  // restore pointer
  // restaurar apuntador

  // Goal: Returns a group of ansicharacters.
  // Objetivo: Regresa un grupo de caracteres.
end;

function ParseWhile
 (const Value: string; var Index: Integer; ValidChars: ansicharset): Boolean;
var Backup: Integer; L: Integer;
begin
  Backup := Index;
  // backup pointer
  // respaldar apuntador

  L := Length(Value);
  while (Index <= L) and IsMember(Value[Index], ValidChars) do
   Inc(Index);
  // try parsing
  // intenta parsear

  Result := (Index > Backup);
  // verify any changes
  // revisa si hubo cambios

  if (not Result)
    then Index := Backup;
  // restore pointer
  // restaurar apuntador

  // Goal: Returns a group of non-space ansicharacters.
  // Objetivo: Regresa un grupo de caracteres que no son espacios.
end;

function ParseUntil
 (const Value: string; var Index: Integer; BreakChars: ansicharset): Boolean;
var Backup: Integer; L: Integer;
begin
  Backup := Index;
  // backup pointer
  // respaldar apuntador

  L := Length(Value);
  while (Index <= L) and not IsMember(Value[Index], BreakChars) do
   Inc(Index);
  // try parsing
  // intenta parsear

  Result := (Index > Backup);
  // verify any changes
  // revisa si hubo cambios

  if (not Result)
    then Index := Backup;
  // restore pointer
  // restaurar apuntador

  // Goal: Returns a group of non-space ansicharacters.
  // Objetivo: Regresa un grupo de caracteres que no son espacios.
end;

function ParseQuotedStr(const Value: string; var Index: Integer): Boolean;
var Backup: Integer;
begin
  Backup := Index;
  // backup pointer
  // respaldar apuntador

  Result := ParseCharSingle(Value, Index, ansiSingleQuote);
  if (Result) then
  begin
    Result := ParseCharUntil(Value, Index, ansiSingleQuote);
    if (Result) then
    begin
      Result := ParseCharSingle(Value, Index, ansiSingleQuote);
    end;
  end;
  // try parsing
  // intenta parsear

  Result := Result and (Index > Backup);
  // verify any changes
  // revisa si hubo cambios

  if (not Result)
    then Index := Backup;
  // restore pointer
  // restaurar apuntador

  // Goal: Parse a quoted string.
  // Objetivo: Parsear una cadena entre comillas.
end;

function ParseUnsigned(const Value: string; var Index: Integer): Boolean;
var Backup: Integer;
begin
  Backup := Index;
  // backup pointer
  // respaldar apuntador

  Result := ParseWhile(Value, Index, DigitSet);
  // try parsing
  // intenta parsear

  Result := Result and (Index > Backup);
  // verify any changes
  // revisa si hubo cambios

  if (not Result)
    then Index := Backup;
  // restore pointer
  // restaurar apuntador

  // Goal: Parse a unsigned integer.
  // Objetivo: Parsear un entero sin signo.
end;

function ParseIdentifier(const Value: string; var Index: Integer): Boolean;
var Backup: Integer;
begin
  Backup := Index;
  // backup pointer
  // respaldar apuntador

  Result := ParseSingle(Value, Index, AlphaSet);
  if (Result) then
  begin
    Result := ParseWhile(Value, Index, IDSet);
  end;
  // try parsing
  // intenta parsear

  Result := Result and (Index > Backup);
  // verify any changes
  // revisa si hubo cambios

  if (not Result)
    then Index := Backup;
  // restore pointer
  // restaurar apuntador

  // Goal: Parse an identifier.
  // Objetivo: Parsear un identificador.
end;

function ParseQualified(const Value: string; var Index: Integer): Boolean;
var Backup: Integer;
begin
  Backup := Index;
  // backup pointer
  // respaldar apuntador

  Result := ParseSingle(Value, Index, AlphaSet);
  if (Result) then
  begin
    Result := ParseWhile(Value, Index, QIDSet);
  end;
  // try parsing
  // intenta parsear

  Result := Result and (Index > Backup);
  // verify any changes
  // revisa si hubo cambios

  if (not Result)
    then Index := Backup;
  // restore pointer
  // restaurar apuntador

  // Goal: Parse an identifier.
  // Objetivo: Parsear un identificador.
end;

function ParseCount
  (const Value: string; var Index: Integer; Count: Integer): Boolean;
var Backup: Integer;
begin
  Backup := Index;
  // backup pointer

  Result := (Pred(Index + Count) <= System.Length(Value));
  if (Result)
    then Inc(Index, Count);

  if (not Result)
    then Index := Backup;
  // restore pointer

  // Goal: Parse "Count" ansicharacters.
end;

function IsQuotedStr(const Value: string): Boolean;
var Index: Integer;
begin
  Index := 1;
  Result := ParseQuotedStr(Value, Index);
end;

function IsUnsigned(const Value: string): Boolean;
var Index: Integer;
begin
  Index := 1;
  Result := ParseUnsigned(Value, Index);
end;

function IsIdentifier(const Value: string): Boolean;
var Index: Integer;
begin
  Index := 1;
  Result := ParseIdentifier(Value, Index);
end;

function ExtractQuotedStr
  (const Source: string; var Dest: string; var Index: Integer): Boolean;
var Start: Integer;
begin
  Start := Index;
  Result := ParseQuotedStr(Source, Index);
  if (Result)
    then Dest := ParseFrom(Source, Start, Index);
  // Goal: Extract a quoted string.
  // Objetivo: Extractar una cadena entre comillas.
end;

function ExtractUnsigned
  (const Source: string; var Dest: string; var Index: Integer): Boolean;
var Start: Integer;
begin
  Start := Index;
  Result := ParseUnsigned(Source, Index);
  if (Result)
    then Dest := ParseFrom(Source, Start, Index);
  // Goal: Extract a unsigned integer.
  // Objetivo: Extractar un entero sin signo.
end;

function ExtractIdentifier
  (const Source: string; var Dest: string; var Index: Integer): Boolean;
var Start: Integer;
begin
  Start := Index;
  Result := ParseIdentifier(Source, Index);
  if (Result)
    then Dest := ParseFrom(Source, Start, Index);
  // Goal: Extract an identifier.
  // Objetivo: Extractar un identificador.
end;

function ExtractQualified
  (const Source: string; var Dest: string; var Index: Integer): Boolean;
var Start: Integer;
begin
  Start := Index;
  Result := ParseQualified(Source, Index);
  if (Result)
    then Dest := ParseFrom(Source, Start, Index);
  // Goal: Extract an identifier.
  // Objetivo: Extractar un identificador.
end;

function ExtractCount
  (const Source: string; var Dest: string; var Index: Integer; Count: Integer): Boolean;
var Start: Integer;
begin
  Start := Index;
  Result := ParseCount(Source, Index, Count);
  if (Result)
    then Dest := ParseFrom(Source, Start, Index);
  // Goal: Extract "Count" ansicharacters.
  // Objetivo: Extraer "Count" caracteres.
end;

end.
