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

unit uktshortansistrs;

{.LONGSTRINGS OFF}
{.VARSTRINGCHECKS OFF}
{$OPENSTRINGS OFF}

interface
uses
{$IFDEF MSWINDOWS}
  Windows, Messages,
{$ENDIF}
{$IFDEF LINUX}
  Types, Libc,
{$ENDIF}
  SysUtils, Math,
  uktComparisons,
  //uktTextmarkers,
  uktAnsiChars, uktTextConsts,
  uktAnsiCharSets,
  uktAnsiCharSetConsts,
  dummy;

type
  //shortansistring  = type shortstring;

  shortansistring  = System.shortstring;
  Tshortansistring = shortansistring;
  Pshortansistring = ^Tshortansistring;  

const
  sdvShortANSIStringMaxSize = sizeof(shortansistring);

{ global properties }

  function getLength
    (const AValue: shortansistring): Word; overload;

  function getUppercase
    (const AValue: shortansistring): shortansistring; overload;
  function getLowercase
    (const AValue: shortansistring): shortansistring; overload;

  function getQuotedUppercase
    (const AValue: shortansistring; const A, B: ansichar): shortansistring; overload;
  function getQuotedLowercase
    (const AValue: shortansistring; const A, B: ansichar): shortansistring; overload;

  function getTrimLeft
    (const AValue: shortansistring): shortansistring; overload;
  function getTrimRight
    (const AValue: shortansistring): shortansistring; overload;
  function getTrim
    (const AValue: shortansistring): shortansistring; overload;

  function getUnTrimLeft
    (const AValue: shortansistring; ACount: Integer): shortansistring; overload;
  function getUnTrimRight
    (const AValue: shortansistring; ACount: Integer): shortansistring; overload;

  procedure setLength
    (var AValue: shortansistring; ACount: Byte); overload;

  procedure setUppercase
    (var AValue: shortansistring); overload;
  procedure setLowercase
    (var AValue: shortansistring); overload;

  procedure setQuotedUppercase
    (var AValue: shortansistring; const A, B: ansichar); overload;
  procedure setQuotedLowercase
    (var AValue: shortansistring; const A, B: ansichar); overload;

  procedure setTrimLeft
    (var AValue: shortansistring); overload;
  procedure setTrimRight
    (var AValue: shortansistring); overload;
  procedure setTrim
    (var AValue: shortansistring); overload;

  procedure setUnTrimLeft
    (var AValue: shortansistring; ACount: Integer); overload;
  procedure setUnTrimRight
    (var AValue: shortansistring; ACount: Integer); overload;

{ global functions }

  function IsEmpty
    (const AValue: shortansistring): Boolean;
  function IsStringOfChar
    (const S: shortansistring; C: Char): Boolean;

  function StringOfChar
    (const AValue: ansichar; const ACount: Byte): shortansistring;

  function Pos
    (const SubStr: shortansistring; const S: shortansistring): Byte; overload;

  function StartsWith
    (const SubStr: shortansistring; const S: string): Boolean;
  function FinishesWith
    (const SubStr: shortansistring; const S: string): Boolean;

  function Left
    (const AValue: shortansistring; const ACount: Byte): shortansistring;
  function Right
    (const AValue: shortansistring; const ACount: Byte): shortansistring;

  function UnLeft
    (const AValue: shortansistring; const ACount: Byte): shortansistring;
  function UnRightCopy
    (const AValue: shortansistring; const ACount: Byte): shortansistring;

  function TryStrToChar
    (const AValue: shortansistring; var Dest: ansichar): Boolean;
  function StrToCharDef
    (const AValue: shortansistring; const DefAValue: ansichar): ansichar;
  function StrToChar
    (const AValue: shortansistring): ansichar;

  function PtrToStr(const AValue: pointer): shortansistring;
  function StrToPtr(const AValue: shortansistring): pointer;

  function DelphiToPascal
    (const AValue: string): shortansistring;
  function PascalToDelphi
    (const AValue: shortansistring): string;

  function DuplicateStr(const AValue: shortansistring): pointer;

  procedure GetMemStr(var AValue: pointer);
  procedure FreeMemStr(var AValue: pointer);

  procedure Empty(var AValue: shortansistring);

  procedure MoveLeft
    (var Dest: shortansistring; const Source: shortansistring; const ACount: Byte);
  procedure MoveRight
    (var Dest: shortansistring; const Source: shortansistring; const ACount: Byte);
  procedure Move
    (var Dest: shortansistring; const Source: shortansistring; AIndex, ACount: Byte);

  procedure Concat
    (var Dest: shortansistring; const Source: ansichar); overload;
  procedure Concat
    (var Dest: shortansistring; const Source: shortansistring); overload;

  function ConcatCopy
    (const A: shortansistring; B: ansichar): shortansistring; overload;
  function ConcatCopy
    (const A, B: shortansistring): shortansistring; overload;

  function MoveCopy
    (const AValue: shortansistring; AIndex, ACount: Byte): shortansistring; overload;
  function ReverseCopy
    (const AValue: shortansistring): shortansistring;

  function SubStr
    (const PartialStr, FullStr: shortansistring; var AIndex: Integer): Boolean;
  function IsIdentifier(const AValue: shortansistring): Boolean;
  function RangeToStr(const Min, Max: ansichar): shortansistring;

  function ReplaceChar
    (const AValue: shortansistring; A, B: ansichar): shortansistring; overload;
  function ReplaceChars
    (const AValue: shortansistring; Source: ansicharset; Dest: ansichar): shortansistring; overload;

  function ReplaceStrALL
    (var FullStr: shortansistring; const SubStr, AValue: shortansistring): Integer;

  function ReplaceCopy
    (const FullStr, Source, Dest: shortansistring): shortansistring;
  function DeleteCopy
    (const FullStr, Source: shortansistring): shortansistring;

  procedure ReplacePos
    (var FullStr: shortansistring;
     var AIndex: Integer; const Source, Dest: shortansistring);
  procedure Replace
    (var FullStr: shortansistring; const Source, Dest: shortansistring);

  function RemoveCharCopy
   (const AValue: shortansistring; IgnoreChar: ansichar): shortansistring;
  function RemoveCharsCopy
   (const AValue: shortansistring; IgnoreChars: ansicharset): shortansistring;

  procedure RemoveChar(var AValue: shortansistring; IgnoreChar: ansichar);
  procedure RemoveChars(var AValue: shortansistring; IgnoreChars: ansicharset);

  function EqualAt
    (const Str, SubStr: shortansistring; const AIndex: Integer): Boolean;
  function SameAt
    (const Str, SubStr: shortansistring; const AIndex: Integer): Boolean;

  function Scan
    (const Str: shortansistring; const SubStr: shortansistring; {copy} AIndex: Word): Word; overload;

  function SkipChar
   (const AValue: shortansistring; AIndex: Integer; ValidChar: ansichar): Integer;
  function SkipChars
   (const AValue: shortansistring; AIndex: Integer; ValidChars: ansicharset): Integer;
  function SkipCharWhile
   (const AValue: shortansistring; AIndex: Integer; ValidChar: ansichar): Integer;
  function SkipCharUntil
   (const AValue: shortansistring; AIndex: Integer; BreakChar: ansichar): Integer;
  function SkipWhile
   (const AValue: shortansistring; AIndex: Integer; ValidChars: ansicharset): Integer;
  function SkipUntil
   (const AValue: shortansistring; AIndex: Integer; BreakChars: ansicharset): Integer;
  function SkipToken
    (const AValue: shortansistring; var S, F: Integer): shortansistring;
  function SkipDigit
    (const AValue: shortansistring; var I, F: Integer): shortansistring;
  function SkipLetter
    (const AValue: shortansistring; var I, F: Integer): shortansistring;

  function CopyFrom(const Source: shortansistring; Start, Finish: Integer): shortansistring;
  function ParseFrom(const Source: shortansistring; var Start, Finish: Integer): shortansistring;

  function SkipEmpty(const Start, Finish: Integer): Boolean;

  function ExtractCharUntil
   (const AValue: shortansistring; AIndex: Integer; BreakChar: ansichar): shortansistring;
  function ExtractWhile
   (const AValue: shortansistring; AIndex: Integer; SkipChars: ansicharset): shortansistring;
  function ExtractUntil
   (const AValue: shortansistring; AIndex: Integer; BreakChars: ansicharset): shortansistring;
  function ExtractString(const AValue: shortansistring; AIndex: Integer): shortansistring;

  function LastChar(const AString: shortansistring): ansichar;

  function AlignLeft
   (const Source: shortansistring; ACount: Integer; AValue: ansichar): shortansistring;
  function AlignRight
   (const Source: shortansistring; ACount: Integer; AValue: ansichar): shortansistring;

  function SameText(const A, B: shortansistring): Boolean;
  function Compare(const A, B: shortansistring): TComparison;

  function EqualOptions
    (const A, B: shortansistring; Options: TStringOptions): Boolean;
  function CompareOptions
    (const A, B: shortansistring; Options: TStringOptions): TComparison;

{ global operators }

  { = } function Equal(const A, B: shortansistring): Boolean;

implementation

{ global properties }

function getLength(const AValue: shortansistring): Word;
begin
  Result := ord(AValue[0]);
  // Goal: Returns the length of the given string.
  // Objetivo: Regresa la longuitud de la cadena dada.
end;

function getUppercase(const AValue: shortansistring): shortansistring;
begin
  Result := SysUtils.ANSIUpperCase(AValue);
  // Goal: Returns a uppercase copy of the given string.
  // Objetivo: Regresa una copia en mayusculas de la cadena dada.
end;

function getLowercase(const AValue: shortansistring): shortansistring;
var S: string;
begin
  S := PascalToDelphi(AValue);
  S := SysUtils.ANSILowerCase(S);
  Result := DelphiToPascal(S);
  // Goal: Returns a lowercase copy of the given string.
  // Objetivo: Regresa una copia en minusculas de la cadena dado.
end;

function getQuotedUppercase
  (const AValue: shortansistring; const A, B: ansichar): shortansistring;
begin
  Result := AValue;
  setQuotedUppercase(Result, A, B);
  // Goal: Returns a uppercase copy of the given string,
  // without modifying delimited substrings.

  // Objetivo: Regresa una copia en mayusculas de la cadena dada,
  // sin modificar a las subcadenas delimitadas.
end;

function getQuotedLowercase
  (const AValue: shortansistring; const A, B: ansichar): shortansistring;
begin
  Result := AValue;
  setQuotedLowercase(Result, A, B);
  // Goal: Returns a lowercase copy of the given nullstring,
  // without modifying delimited substrings.

  // Objetivo: Regresa una copia en minusculas de la cadena terminada en nulo,
  // dada sin modificar a las subcadenas delimitadas.
end;

function getTrimLeft(const AValue: shortansistring): shortansistring;
var S: string;
begin
  S := PascalToDelphi(AValue);
  S := SysUtils.TrimLeft(S);
  Result := DelphiToPascal(S);
  // Goal: Returns a copy of the given string without leading spaces.
  // Objetivo: Regresa una copia de la cadena dada sin espacios iniciales.
end;

function getTrimRight(const AValue: shortansistring): shortansistring;
var S: string;
begin
  S := PascalToDelphi(AValue);
  S := SysUtils.TrimRight(S);
  Result := DelphiToPascal(S);
  // Goal: Returns a copy of the given string without trailing spaces.
  // Objetivo: Regresa una copia de la cadena dada sin espacios finales.
end;

function getTrim(const AValue: shortansistring): shortansistring;
var S: string;
begin
  S := PascalToDelphi(AValue);
  S := SysUtils.Trim(S);
  Result := DelphiToPascal(S);
  // Goal: Returns a copy of the given string
  // without leading & trailing spaces.

  // Objetivo: Regresa una copia de la cadena dada
  // sin espacios iniciales y finales.
end;

function getUnTrimLeft(const AValue: shortansistring; ACount: Integer): shortansistring;
var L: Integer;
begin
  L := getLength(AValue);
  Concat(Result, StringOfChar(#32, ACount-L));
  Concat(Result, AValue);
  // Goal: Returns a copy of the given string plus leading spaces.
  // Objetivo: Regresa una copia de la cadena dada mas espacios iniciales.
end;

function getUnTrimRight(const AValue: shortansistring; ACount: Integer): shortansistring;
var L : Integer;
begin
  L := getLength(AValue);
  Concat(Result, AValue);
  Concat(Result, StringOfChar(#32, ACount-L));
  // Goal: Returns a copy of the given string plus trailing spaces.
  // Objetivo: Regresa una copia de la cadena dada mas espacios finales.
end;

procedure setLength(var AValue: shortansistring; ACount: Byte);
begin
  AValue[0] := ansichar(ACount);
end;

procedure setUppercase(var AValue: shortansistring);
var S: string;
begin
  S := PascalToDelphi(AValue);
  S := SysUtils.ANSIUpperCase(S);
  AValue := DelphiToPascal(S);
  // Goal: Changes the given string into uppercase.
  // Objetivo: Cambia la cadena dada a mayusculas.
end;

procedure setLowercase(var AValue: shortansistring);
var S: string;
begin
  S := PascalToDelphi(AValue);
  S := SysUtils.ANSILowerCase(S);
  AValue := DelphiToPascal(S);
  // Goal: Changes the given string into lowercase.
  // Objetivo: Cambia la cadena dada a minusculas.
end;

procedure setQuotedUppercase(var AValue: shortansistring; const A, B: ansichar);
var InsideString: Boolean; AIndex: Integer; C: ansichar;
begin
  InsideString := FALSE;
  for AIndex := 1 to getLength(AValue) do
  begin
    C := AValue[AIndex];
    if (InsideString)
      then InsideString := not (C = B)
      else InsideString := (C = A);
    // initial or final delimiter found ?
    // delimitador inicial o final encontrado ?

    if (not InsideString and IsMember(C, LowerSet))
      then AValue[AIndex] := System.UpCase(C);
    // replace characters
    // reemplazar caracteres
  end;
  // Goal: Changes the given string into uppercase,
  // without modifying delimited substrings.

  // Objetivo: Cambia la cadena dada a mayusculas.
  // sin modificar a las subcadenas delimitadas.
end;

procedure setQuotedLowercase(var AValue: shortansistring; const A, B: ansichar);
var InsideString: Boolean; AIndex: Integer; C: ansichar;
begin
  InsideString := FALSE;
  for AIndex := 1 to getLength(AValue) do
  begin
    C := AValue[AIndex];
    if (InsideString)
      then InsideString := not (C = B)
      else InsideString := (C = A);
    // initial or final delimiter found ?
    // delimitador inicial o final encontrado ?

    if (not InsideString and IsMember(C, UpperSet))
      then AValue[AIndex] := Chr(Ord(C) + 32)
    // replace characters
    // reemplazar caracteres
  end;
  // Goal: Changes the given string into lowercase,
  // without modifying delimited substrings.

  // Objetivo: Cambia la cadena dada a minusculas.
  // sin modificar a las subcadenas delimitadas.
end;

procedure setTrimLeft(var AValue: shortansistring);
var S: string;
begin
  S := PascalToDelphi(AValue);
  S := SysUtils.TrimLeft(S);
  AValue := DelphiToPascal(S);
  // Goal: Returns the given string without leading spaces.
  // Objetivo: Regresa la cadena dada sin espacios iniciales.
end;

procedure setTrimRight(var AValue: shortansistring);
var S: string;
begin
  S := PascalToDelphi(AValue);
  S := SysUtils.TrimRight(S);
  AValue := DelphiToPascal(S);
  // Goal: Returns the given string without trailing spaces.
  // Objetivo: Regresa la cadena dada sin espacios finales.
end;

procedure setTrim(var AValue: shortansistring);
var S: string;
begin
  S := PascalToDelphi(AValue);
  S := SysUtils.Trim(S);
  AValue := DelphiToPascal(S);
  // Goal: Returns the given string without leading & trailing spaces.
  // Objetivo: Regresa la cadena dada sin espacios iniciales y finales.
end;

procedure setUnTrimLeft(var AValue: shortansistring; ACount: Integer);
var L: Integer;
begin
  L := Length(AValue);
  Concat(AValue, StringOfChar(#32, ACount-L));
  Concat(AValue, AValue);
  // Goal: Returns the given string plus leading spaces.
  // Objetivo: Regresa la cadena dada mas espacios iniciales.
end;

procedure setUnTrimRight(var AValue: shortansistring; ACount: Integer);
var L: Integer;
begin
  L := Length(AValue);
  Concat(AValue, AValue);
  Concat(AValue, StringOfChar(#32, ACount-L));
  // Goal: Returns the given string plus trailing spaces.
  // Objetivo: Regresa la cadena dada mas espacios finales.
end;

{ global functions }

function IsEmpty(const AValue: shortansistring): Boolean;
begin
  Result := getLength(AValue) = 0;
  // Goal: Returns if a shortansistring is empty.
  // Objetivo: Regresa si una cadena esta vacia.
end;

function IsStringOfChar(const S: shortansistring; C: Char): Boolean;
var I, L: Integer; Match: Boolean;
begin
  L := System.Length(S);

  Result := (L > 0);
  if (Result) then
  begin
    I := 1; Match := TRUE;
    while ((I <= L) and (Match)) do
    begin
      Match := (S[i] = C);
      Inc(I);
    end;

    Result := Match;
  end;
  // Objetivo: Regresa si una cadena esta compuesta solo del mismo caracter.
  // Goal: Returns if a string is composed with the same character.
end;

function StringOfChar(const AValue: ansichar; const ACount: Byte): shortansistring;
begin
  Empty(Result);
  System.FillChar(Result, ACount, AValue);
  setLength(Result, ACount);
  // Goal: Returns a shortansistring of the same ansichar.
  // Objetivo: Regresa una cadena del mismo caractaer.
end;

function Pos(const SubStr: shortansistring; const S: shortansistring): Byte;
begin
  Result := System.Pos(SubStr, S);
  // Objetivo: Regresa el indice de la primer ocurrencia de "Substr".
  // Goal: Returns the AIndex of the first ocurrence of "Substr".
end;

function StartsWith(const SubStr: shortansistring; const S: string): Boolean;
var Shorter, Len: Integer; LeadStr: string;
begin
  Shorter := System.Length(SubStr);
  // obtain length of substring
  // obtener longuitud de subcadena

  Len := System.Length(S);
  // obtain length of full string
  // obtener longuitud de cadena completa

  Result := not (Shorter > Len);
  // substring must be shorter or equal size than full string
  // la subcadena debe ser mas corta o de igual tamaño que la cadena completa

  if (Result) then
  begin
    LeadStr := System.Copy(S, 1, Shorter);
    Result  := (SubStr = LeadStr);
  end;
  // Objetivo: Regresa si una subcadena es igual o esta al inicio de
  // otra cadena.
  // Goal: Returns if a substring is equal o is the start of another string.
end;

function FinishesWith(const SubStr: shortansistring; const S: string): Boolean;
begin
  // @to-do:
  Result := false;
end;

function Left(const AValue: shortansistring; const ACount: Byte): shortansistring;
begin
 uktshortansistrs.MoveLeft(Result, AValue, ACount);
  // Goal: Returns the leftmost characters of "AValue".
  // Objetivo: Regresa los caracteres mas a la izquierda de "AValue".
end;

function Right(const AValue: shortansistring; const ACount: Byte): shortansistring;
begin
 uktshortansistrs.MoveRight(Result, AValue, ACount);
  // Goal: Returns the rightmost characters of "AValue".
  // Objetivo: Regresa los caracteres mas a la derecha de "AValue".
end;

function UnLeft(const AValue: shortansistring; const ACount: Byte): shortansistring;
begin
 uktshortansistrs.Move(Result, AValue, ACount, getLength(AValue));
  // Goal: Removes the leftmost characters of "AValue".
  // Objetivo: Remueve los caracteres mas a la izquierda de "AValue".
end;

function UnRightCopy(const AValue: shortansistring; const ACount: Byte): shortansistring;
var AIndex: Integer;
begin
  AIndex := Succ(getLength(AValue)-ACount);
 uktshortansistrs.Move(Result, AValue, 1, AIndex);
  // Goal: Removes the RightCopymost characters of "AValue".
  // Objetivo: Remueve los caracteres mas a la derecha de "AValue".
end;

function TryStrToChar(const AValue: shortansistring; var Dest: ansichar): Boolean;
begin
  Result := (getLength(AValue) > 0);
  if (Result)
    then Dest := AValue[1];
  // Goal: To cast a "shortansistring", to a "ansichar".
  // Objetivo: Convertir un "shortansistring" en un "ansichar".
end;

function StrToCharDef(const AValue: shortansistring; const DefAValue: ansichar): ansichar;
begin
  if (not TryStrToChar(AValue, Result))
    then Result := DefAValue;
  // Goal: To cast a "shortansistring", to a "ansichar".
  // Objetivo: Convertir un "shortansistring" en un "ansichar".
end;

function StrToChar(const AValue: shortansistring): ansichar;
begin
  Result := #0;
  TryStrToChar(AValue, Result)
  // Goal: To cast a "shortansistring", to a "ansichar".
  // Objetivo: Convertir un "shortansistring" en un "ansichar".
end;

function PtrToStr(const AValue: pointer): shortansistring;
begin
  Result := Pshortansistring(AValue)^;
  // Goal: Copy an string into a dynamic string.
  // Objetivo: Copiar una cadena a una cadena dinamica.
end;

function StrToPtr(const AValue: shortansistring): pointer;
begin
  Result := @AValue;
  // Goal: Copy an string into a dynamic string.
  // Objetivo: Copiar una cadena a una cadena dinamica.
end;

function DelphiToPascal(const AValue: string): shortansistring;
var ACount: Integer;
begin
  ACount := Math.Min(System.Length(AValue), sdvShortANSIStringMaxSize);
 uktshortansistrs.Empty(Result);
  System.Move(AValue[1], Result[1], ACount);
 uktshortansistrs.setLength(Result, ACount);
end;

function PascalToDelphi(const AValue: shortansistring): string;
begin
  System.SetLength(Result, sdvShortANSIStringMaxSize);
  System.Move(AValue[1], Result[1], sdvShortANSIStringMaxSize);
  System.SetLength(Result, getLength(AValue));
end;

function DuplicateStr(const AValue: shortansistring): pointer;
begin
  Result := nil;
  GetMemStr(Result);
  System.Move(AValue, Result^, sdvShortANSIStringMaxSize);
  // Objetivo: Regresa en memoria dinamica una copia de la cadena dada.
  // Goal: Returns a, dynamic memory of the given string.
end;

procedure GetMemStr(var AValue: pointer);
var
  V: Pshortansistring absolute AValue;
  L: Integer;
begin
  AValue := nil; L := sdvShortANSIStringMaxSize;
  GetMem(AValue, L);
  Empty(V^);
end;

procedure FreeMemStr(var AValue: pointer);
begin
  FreeMem(AValue, sdvShortANSIStringMaxSize);
  AValue := nil;
end;

procedure Empty(var AValue: shortansistring);
begin
  System.FillChar(AValue, sdvShortANSIStringMaxSize, #0);
  // Goal: Clear a ansi string.
  // Objetivo: Limpia una cadena ansi.
end;

procedure MoveLeft
  (var Dest: shortansistring; const Source: shortansistring; const ACount: Byte);
begin
 uktshortansistrs.Move(Dest, Source, 1, ACount);
  // Goal: Returns the leftmost characters of "AValue".
  // Objetivo: Regresa los caracteres mas a la izquierda de "AValue".
end;

procedure MoveRight
  (var Dest: shortansistring; const Source: shortansistring; const ACount: Byte);
var AIndex: Integer;
begin
  AIndex := Succ(getLength(Source)-ACount);
 uktshortansistrs.Move(Dest, Source, AIndex, ACount);
  // Goal: Returns the rightmost characters of "AValue".
  // Objetivo: Regresa los caracteres mas a la derecha de "AValue".
end;

procedure Move
  (var Dest: shortansistring; const Source: shortansistring; AIndex, ACount: Byte);
begin
 uktshortansistrs.Empty(Dest);
  System.Move(Source[AIndex], Dest, ACount);
 uktshortansistrs.setLength(Dest, ACount);
  // Objetivo: Copiar el contenido de una cadena a un nueva cadena.
  // Goal: Copy the contents of a string into a new string.
end;

procedure Concat(var Dest: shortansistring; const Source: ansichar);
var Len, AIndex: Word;
begin
  Len := getLength(Dest);
  if (Len < sdvShortANSIStringMaxSize) then
  begin
    AIndex := Succ(Len);
    Dest[AIndex] := Source;
    setLength(Dest, AIndex);
  end;
  // Objetivo: Agregar  un caracter al final de la cadena dada.
  // Goal: Add a character at the end of the given string.
end;

procedure Concat(var Dest: shortansistring; const Source: shortansistring);
var SourceLen, DestLen, AIndex, ACount, MaxLen: Word;
begin
  SourceLen := getLength(Source);
  DestLen   := getLength(Dest);
  AIndex     := Succ(DestLen);

  MaxLen   := SourceLen + DestLen;
  ACount    := Math.Min(SourceLen + DestLen, sdvShortANSIStringMaxSize - SourceLen);

  System.Move(Source[1], Dest[AIndex], ACount);
 uktshortansistrs.setLength(Dest, MaxLen);
end;

function ConcatCopy
  (const A: shortansistring; B: ansichar): shortansistring;
begin
 uktshortansistrs.Empty(Result);
 uktshortansistrs.Concat(Result, A);
 uktshortansistrs.Concat(Result, B);
  // Objetivo: Agregar  un caracter al final de la cadena dada.
  // Goal: Add a character at the end of the given string.
end;

function ConcatCopy
  (const A, B: shortansistring): shortansistring;
begin
 uktshortansistrs.Empty(Result);
 uktshortansistrs.Concat(Result, A);
 uktshortansistrs.Concat(Result, B);
end;

function MoveCopy
  (const AValue: shortansistring; AIndex, ACount: Byte): shortansistring;
begin
 uktshortansistrs.Empty(Result);
 uktshortansistrs.Move(Result, AValue, AIndex, ACount);
  // Objetivo: Copiar el contenido de una cadena a un nueva cadena.
  // Goal: Copy the contents of a string into a new string.
end;

function ReverseCopy(const AValue: shortansistring): shortansistring;
begin
  Result := '';
end;

function SubStr(const PartialStr, FullStr: shortansistring; var AIndex: Integer): Boolean;
begin
  AIndex := Pos(PartialStr, FullStr);
  Result := (AIndex > 0);
  // Goal: Returns if a shortansistring is contained by other shortansistring.
  // Objetivo: Regresa si una cadena esta contenida en otra.
end;

function IsIdentifier(const AValue: shortansistring): Boolean;
var I: Integer;
begin
  Result := False;
  if ((getLength(AValue) = 0) or not IsMember(AValue[1], AlphaSet))
    then Exit;

  for I := 2 to getLength(AValue) do
    if (not IsMember(AValue[I], IDSet))
      then Exit;
      
  Result := TRUE;
  // Goal: To return if a shortansistring is a valid identifier.
  // Objetivo: Regresar si una cadena es un identificador valido.
end;

function RangeToStr(const Min, Max: ansichar): shortansistring;
var I: ansichar; { s := 'a' .. 'z'; }
begin
 uktshortansistrs.Empty(Result);
  for i := Min to Max do
    Concat(Result, i);
  // Goal: Transform a range of characters into a "shortansistring".
  // Objetivo: Transformar un rango de caracteres en un "shortansistring".

  // Warning: "Min" must be lesser than "Max".
  // Advertencia: "Min" debera ser menor que "Max".
end;

function ReplaceChar(const AValue: shortansistring; A, B: ansichar): shortansistring;
var I, L: Integer;
begin
  Result := AValue; L := getLength(AValue);
  for i := 1 to L do
    if (Result[i] = A)
      then Result[i] := B;
  // Goal: Replace a specific character from a shortansistring.
  // Objetivo: Reemplazar un caracter en especifico de una cadena.
end;

function ReplaceChars
 (const AValue: shortansistring; Source: ansicharset; Dest: ansichar): shortansistring;
var I: Integer;
begin
 uktshortansistrs.Empty(Result);
  for i := 1 to getLength(AValue) do
    if (IsMember(AValue[i], Source))
      then Concat(Result, Dest)
      else Concat(Result, AValue[i]);
  // Goal: Replace a specific character set from a shortansistring.
  // Objetivo: Reemplazar un conjunto caracter en especifico de una cadena.
end;

function ReplaceStrALL
  (var FullStr: shortansistring; const SubStr, AValue: shortansistring): Integer;
var Finished: Boolean; AIndex: Integer;
begin
  Result := 0;
  repeat
    ReplacePos(FullStr, AIndex, SubStr, AValue);
    Finished := (AIndex = 0);
    if (not Finished)
      then Inc(Result);
  until Finished;
  // Goal: Replaces all the "SubStr" found, "FullStr" with "AValue".
  // Objetivo: Reemplaza todas las "SubStr" encontradas en "FullStr" por "AValue".}
end;

function ReplaceCopy
  (const FullStr, Source, Dest: shortansistring): shortansistring;
var AIndex: Integer;
begin
  Result := FullStr;
  ReplacePos(Result, AIndex, Source, Dest);
  // Goal: Returns a copy of the string with the given substring replaced.
  // Objetivo: Regresa una copia de la cadena con la subcadena reemplazada.
end;

function DeleteCopy(const FullStr, Source: shortansistring): shortansistring;
begin
  Result := ReplaceCopy(FullStr, Source, '');
  // Goal: Returns a copy of the shortansistring with the given substring deleted.
  // Objetivo: Regresa una copia de la cadena con la subcadena eliminada.
end;

procedure ReplacePos
  (var FullStr: shortansistring;
   var AIndex: Integer; const Source, Dest: shortansistring);
var BeforeACount, AfterACount: Integer; BeforeString, AfterString: shortansistring;
begin
  AIndex := Pos(Source, FullStr);
  // localizar en que posicion inicia la cadena que se va BeforeString reemplazar
  // locate, which location starts the string to be replaced

  if (AIndex > 0) then
  begin
    AfterACount := AIndex + getLength(Source);
    // obtener la longuitud de la cadena despues de la parte antes a reemplazar
    // obtain the length of the string after the section to be replaced

    BeforeACount := getLength(FullStr) - getLength(Source);
    // obtener la longuitud de la cadena antes de la parte antes a reemplazar
    // obtain the length of the string before the section to be replaced

    if (AIndex <> 0) then
    begin
     uktshortansistrs.MoveLeft(BeforeString, FullStr, BeforeACount);
     uktshortansistrs.MoveRight(AfterString, FullStr, AfterACount);
      // extraer secciones antes y despues de cadena nodeseada
      // extract before & after section of unwanted string

     uktshortansistrs.Empty(FullStr);
     uktshortansistrs.Concat(FullStr, BeforeString);
     uktshortansistrs.Concat(FullStr, Dest);
     uktshortansistrs.Concat(FullStr, AfterString);
    end;
  end;
  // Goal: Replaces the first "Source" found, "FullStr" with "Dest".
  // Objetivo: Reemplaza la primera "Source" encontrada en "FullStr" por "Dest".}
end;

procedure Replace
  (var FullStr: shortansistring; const Source, Dest: shortansistring);
var AIndex: Integer;
begin
  ReplacePos(FullStr, AIndex, Source, Dest);
  // Goal: Replaces the first "Source" found, "FullStr" with "Dest".
  // Objetivo: Reemplaza la primera "Source" encontrada en "FullStr" por "Dest".}
end;

function RemoveCharCopy
  (const AValue: shortansistring; IgnoreChar: ansichar): shortansistring;
var I: Integer;
begin
 uktshortansistrs.Empty(Result);
  for i := 1 to Length(AValue) do
    if (AValue[i] <> IgnoreChar)
      then uktshortansistrs.Concat(Result, AValue[i]);
  // Goal: Delete a specific character from a string.
  // Objetivo: Eliminar un caracter en especifico de una cadena.
end;

function RemoveCharsCopy
  (const AValue: shortansistring; IgnoreChars: ansicharset): shortansistring;
var I: Integer;
begin
 uktshortansistrs.Empty(Result);
  for i := 1 to Length(AValue) do
    if (not IsMember(AValue[I], IgnoreChars))
      then uktshortansistrs.Concat(Result, AValue[i]);
  // Goal: Delete a specific character set from a string.
  // Objetivo: Eliminar un conjunto caracter en especifico de una cadena.
end;

procedure RemoveChar(var AValue: shortansistring; IgnoreChar: ansichar);
begin
  AValue := RemoveCharCopy(AValue, IgnoreChar);
  // Goal: Delete a specific character from a string.
  // Objetivo: Eliminar un caracter en especifico de una cadena.
end;

procedure RemoveChars(var AValue: shortansistring; IgnoreChars: ansicharset);
begin
  AValue := RemoveCharsCopy(AValue, IgnoreChars);
  // Goal: Delete a specific character set from a string.
  // Objetivo: Eliminar un conjunto caracter en especifico de una cadena.
end;

function EqualAt
  (const Str, SubStr: shortansistring; const AIndex: Integer): Boolean;
//var CanContinue: Boolean; Last1, Last2, I, J: Integer;
begin
  Result := FALSE;
//  Result := (System.Copy(Str, AIndex, getLength(SubStr)) = SubStr);

  (*
  Last1 := getLength(Str);
  Last2 := getLength(SubStr);
  I := AIndex;
  J := 1;

  repeat
    CanContinue := (I <

    Inc(I);
    Inc(J);
  until CanContinue;
  *)
  // Goal: Compares the "Str" shortansistring beggining at "AIndex" character
  // with the shortansistring "SubStr".

  // Objetivo: Compara la cadena "Str" comenzando con el caracter "AIndex"
  // con la cadena "SubStr".
end;

function SameAt
  (const Str, SubStr: shortansistring; const AIndex: Integer): Boolean;
begin
  Result := FALSE;
end;

function Scan
 (const Str: shortansistring; const SubStr: shortansistring; {copy} AIndex: Word): Word;
var Len: Word; Found: Boolean;
begin
  Len := getLength(Str);
  Found  := FALSE;

  while (not Found and (AIndex <= Len)) do
  begin
    if (EqualAt(Str, SubStr, AIndex))
      then Found := TRUE
      else Inc(AIndex);
  end;

  if (Found)
    then Result := AIndex
    else Result := 0;
  // Goal: Searches for a substring inside other shortansistring
  // beginning at the "AIndex" ansichar and returns the next character.

  // Objetivo: Busca una subcadena adentro de otra cadena
  // comenzando en el caracter numero "indice" y regresa el siguiente caracter.
end;

function SkipChars
  (const AValue: shortansistring; AIndex: Integer; ValidChars: ansicharset): Integer;
var L: Integer;
begin
  Result := AIndex; L := Length(AValue);
  if ((Result <= L) and IsMember(AValue[Result], ValidChars))
    then Inc(Result);
  // Goal: Returns a single character.
  // Objetivo: Regresa un solo caracter.
end;

function SkipChar
  (const AValue: shortansistring; AIndex: Integer; ValidChar: ansichar): Integer;
var L: Integer;
begin
  Result := AIndex; L := Length(AValue);
  if ((Result <= L) and (AValue[Result] = ValidChar))
    then Inc(Result);
  // Goal: Returns a single character.
  // Objetivo: Regresa un solo caracter.
end;

function SkipCharWhile
  (const AValue: shortansistring; AIndex: Integer; ValidChar: ansichar): Integer;
var L: Integer;
begin
  Result := AIndex; L := Length(AValue);
  while ((Result <= L) and (AValue[Result] = ValidChar)) do
   Inc(Result);
  // Goal: Returns a group of characters. }
  // Objetivo: Regresa un grupo de caracteres.
end;

function SkipCharUntil
  (const AValue: shortansistring; AIndex: Integer; BreakChar: ansichar): Integer;
var L: Integer;
begin
  Result := AIndex; L := Length(AValue);
  while ((Result <= L) and (AValue[Result] <> BreakChar)) do
   Inc(Result);
  // Goal: Returns a group of characters. }
  // Objetivo: Regresa un grupo de caracteres.
end;

function SkipWhile
  (const AValue: shortansistring; AIndex: Integer; ValidChars: ansicharset): Integer;
var L: Integer;
begin
  Result := AIndex; L := Length(AValue);
  while ((Result <= L) and IsMember(AValue[Result], ValidChars)) do
   Inc(Result);
  // Goal: Returns a group of non-space characters. }
  // Objetivo: Regresa un grupo de caracteres que no son espacios.
end;

function SkipUntil
  (const AValue: shortansistring; AIndex: Integer; BreakChars: ansicharset): Integer;
var L: Integer;
begin
  Result := AIndex; L := Length(AValue);
  while ((Result <= L) and not IsMember(AValue[Result], BreakChars)) do
   Inc(Result);
  // Goal: Returns a group of non-space characters. }
  // Objetivo: Regresa un grupo de caracteres que no son espacios.
end;

function SkipToken
  (const AValue: shortansistring; var S, F: Integer): shortansistring;
var StartAIndex, FinishAIndex: Integer;
begin
  StartAIndex  := S;

  StartAIndex  := SkipCharWhile(AValue, StartAIndex, #32);
  FinishAIndex := SkipCharUntil(AValue, StartAIndex, #32);
  Result      := CopyFrom(AValue, StartAIndex, FinishAIndex);

  S := StartAIndex;
  F := FinishAIndex;
end;

function SkipDigit
  (const AValue: shortansistring; var I, F: Integer): shortansistring;
begin
  I := SkipCharWhile(AValue, I, #32);
  F := SkipWhile(AValue, I, DigitSet);
  Result := CopyFrom(AValue, I, F);
  I := F;
end;

function SkipLetter
  (const AValue: shortansistring; var I, F: Integer): shortansistring;
begin
  I := SkipCharWhile(AValue, I, #32);
  F := SkipWhile(AValue, I, AlphaSet);
  Result := CopyFrom(AValue, I, F);
  I := F;
end;

function CopyFrom
  (const Source: shortansistring; Start, Finish: Integer): shortansistring;
var ACount: Integer;
begin
  ACount := Succ(Finish - Start);
 uktshortansistrs.Move(Result, Source, Start, ACount);
  // Goal: Returns a substring from "start" to "finish".
  // Objetivo: Regresa una subcadena desde "start" hasta "finish". }
end;

function ParseFrom
  (const Source: shortansistring; var Start, Finish: Integer): shortansistring;
var ACount: Integer;
begin
  ACount := Succ(Finish - Start);
 uktshortansistrs.Move(Result, Source, Start, ACount);
  Start  := Finish;
  // Goal: Returns a substring from "start" to "finish" & update "start".
  // Objetivo: Regresa una subcadena desde "start" hasta "finish" }
  { y actualiza "start. }
end;

function SkipEmpty(const Start, Finish: Integer): Boolean;
begin
  Result := ((Finish - Start) = 0);
  // Goal: Returns if there is a SubStr between the given indices is empty.
  // Objetivo: Regresa si hay una subcadena entre los indices dados esta vacia.
end;

function ExtractCharUntil
 (const AValue: shortansistring; AIndex: Integer; BreakChar: ansichar): shortansistring;
var I, L: Integer;
begin
 uktshortansistrs.Empty(Result);

  I := AIndex; L := Length(AValue);
  while ((I <= L) and (AValue[I] = #32)) do
    Inc(I);
  // Jump leading spaces
  // Brincar espacios al inicio

  while ((I <= L) and (AValue[I] <> BreakChar)) do
  begin
   uktshortansistrs.Concat(Result, AValue[I]);
    Inc(I);
  end;
  // Goal: Returns a group of non-space characters. }
  // Objetivo: Regresa un grupo de caracteres que no son espacios.
end;

function ExtractWhile
  (const AValue: shortansistring; AIndex: Integer; SkipChars: ansicharset): shortansistring;
var I, L: Integer;
begin
 uktshortansistrs.Empty(Result);

  I := AIndex; L := Length(AValue);
  while ((I <= L) and (AValue[I] = #32)) do
    Inc(I);
  // Jump leading spaces
  // Brincar espacios al inicio

  while ((I <= L) and IsMember(AValue[I], SkipChars)) do
  begin
   uktshortansistrs.Concat(Result, AValue[I]);
    Inc(I);
  end;
  // Goal: Returns a group of non-space selected characters. }
  // Objetivo: Regresa un grupo de caracteres seleccionados que no son espacios.
end;

function ExtractUntil
  (const AValue: shortansistring; AIndex: Integer; BreakChars: ansicharset): shortansistring;
var I, L: Integer;
begin
 uktshortansistrs.Empty(Result);

  I := AIndex; L := Length(AValue);
  while ((I <= L) and (AValue[I] = #32)) do
    Inc(I);
  // Jump leading spaces
  // Brincar espacios al inicio

  while ((I <= L) and not IsMember(AValue[I], BreakChars)) do
  begin
   uktshortansistrs.Concat(Result, AValue[I]);
    Inc(I);
  end;
  // Goal: Returns a group of non-space characters.
  // Objetivo: Regresa un grupo de caracteres que no son espacios.
end;

function ExtractString(const AValue: shortansistring; AIndex: Integer): shortansistring;
var I, L: Integer;
begin
 uktshortansistrs.Empty(Result);

  I := AIndex; L := Length(AValue);
  while ((I <= L) and (AValue[I] = #32)) do
    Inc(I);
  // Jump leading spaces
  // Brincar espacios al inicio

  while ((I <= L) and (AValue[I] <> #32)) do
  begin
   uktshortansistrs.Concat(Result, AValue[I]);
    Inc(I);
  end;
  // Goal: Returns a group of non-space characters.
  // Objetivo: Regresa un grupo de caracteres que no son espacios.
end;

function LastChar(const AString: shortansistring): ansichar;
var L: Integer;
begin
  L := Length(AString);
  if (L < 1)
    then Result := #0
    else Result := AString[ L ];
  // Goal: To return the last ansichar of a shortansistring.
  // Objetivo: Regresar el ultimo caracter de una cadena.
end;

function AlignLeft
 (const Source: shortansistring; ACount: Integer; AValue: ansichar): shortansistring;
var Difference: Integer;
begin
  Difference := ACount - getLength(Source);
 uktshortansistrs.Empty(Result);
 uktshortansistrs.Concat(Result, Source);
 uktshortansistrs.Concat(Result, StringOfChar(AValue, Difference));
  // Goal: Add characters to the right of a string.
  // Objetivo: Agregar caracteres a la derecha o final de una cadena.
end;

function AlignRight
 (const Source: shortansistring; ACount: Integer; AValue: ansichar): shortansistring;
var Difference: Integer;
begin
  Difference := ACount - getLength(Source);
 uktshortansistrs.Empty(Result);
 uktshortansistrs.Concat(Result, StringOfChar(AValue, Difference));
 uktshortansistrs.Concat(Result, Source);
  // Goal: Add characters to the left of a string.
  // Objetivo: Agregar caracteres a la izquierda o inicio de una cadena.
end;

function SameText(const A, B: shortansistring): Boolean;
begin
  Result := SysUtils.SameText(A, B);
  // Goal: Returns if 2 strings are equal, ignora case.
  // Objetivo: Regresa si 2 cadenas son iguales, ignorar caso.
end;

function Compare(const A, B: shortansistring): TComparison;
begin
  if (A = B)
    then Result := cmpEqual
  else if (A < B)
    then Result := cmpLower
  else Result := cmpHigher
  // Goal: Returns the comparison between 2 strings.
  // Objetivo: Regresa la comparacion de 2 cadenas.
end;

function EqualOptions
  (const A, B: shortansistring; Options: TStringOptions): Boolean;
begin
  case (Options) of
    soExactMatch: Result := (A = B);
    soForceMatch: Result := (getUppercase(A) = B);
    soForceFirst: Result := (getUppercase(A) = getUppercase(B));
    soForceLast:  Result := (A = getUppercase(B));
    else Result := FALSE;
  end;
  // Goal: Returns if 2 strings are equals uppon the given options.
  // Objetivo: Regresa si 2 cadenas son iguales basado en las opciones dadas.
end;

function CompareOptions
  (const A, B: shortansistring; Options: TStringOptions): TComparison;
begin
  case Options of
    soExactMatch: Result := Compare(A, B);
    soForceMatch: Result := Compare(getUppercase(A), getUppercase(B));
    soForceFirst: Result := Compare(getUppercase(A), B);
    soForceLast:  Result := Compare(A, getUppercase(B));
    else          Result := cmpHigher;
  end;
  // Goal: Returns if 2 strings are equals upon the given options.
  // Objetivo: Regresa si 2 cadenas son iguales basado en las opciones dadas.
end;

{ global operators }

function Equal(const A, B: shortansistring): Boolean;
begin
  Result := (A = B);
  // Goal: Returns if 2 strings are equal.
  // Objetivo: Regresa si 2 cadenas son iguales.
end;

end.
