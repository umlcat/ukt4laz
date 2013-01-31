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

unit uktansiarrays;

interface
uses
{$IFDEF MSWINDOWS}
  Windows, Messages,
{$ENDIF}
{$IFDEF LINUX}
  Types, Libc,
{$ENDIF}
{$IFDEF FPC}
  uktlibc,
{$ENDIF}
  SysUtils, Math,
  uktComparisons,
  uktANSIChars, uktANSICharSets,
  uktTextconsts,
  uktANSICharSetConsts,
  dummy;

type

// delphi*s "string" is a pointer similar to java*s references
// el "string" de delphi es un apuntador similar a las referencias de java

  ansiarray  = array[0..255] of ansichar;
  Tansiarray = ansiarray;
  Pansiarray = ^Tansiarray;
  // we'll use a pascal string (an array of char) instead
  // en vez de eso, usaremos una cadena pascal (arreglo de caracteres)

const
  sdvAnsiarraysize = System.SizeOf(ansiarray);

{ global properties }

  function getQuotedUppercase
    (const Value: ansiarray; const A, B: ansichar): ansiarray; overload;
  function getQuotedLowercase
    (const Value: ansiarray; const A, B: ansichar): ansiarray; overload;

  function getTrimLeft(const Value: ansiarray): ansiarray; overload;
  function getTrimRightCopy(const Value: ansiarray): ansiarray; overload;
  function getTrim(const Value: ansiarray): ansiarray; overload;

  function getUnTrimLeft(const Value: ansiarray; Count: Integer): ansiarray; overload;
  function getUnTrimRightCopy(const Value: ansiarray; Count: Integer): ansiarray; overload;

  procedure setQuotedUppercase
    (var Value: ansiarray; const A, B: ansichar); overload;
  procedure setQuotedLowercase
    (var Value: ansiarray; const A, B: ansichar); overload;

  procedure setTrimLeft(var Value: ansiarray); overload;
  procedure setTrimRight(var Value: ansiarray); overload;
  procedure setTrim(var Value: ansiarray); overload;

  procedure setUnTrimLeft(var Value: ansiarray; Count: Integer); overload;
  procedure setUnTrimRightCopy(var Value: ansiarray; Count: Integer); overload;

{ global functions }

  function Length(const Value: ansiarray): Word; overload;

  function SameText(const A, B: ansiarray): Boolean;

  function UppercaseCopy(const Value: ansiarray): ansiarray; overload;
  function LowercaseCopy(const Value: ansiarray): ansiarray; overload;
  function TogglecaseCopy(const Value: ansiarray): ansiarray; overload;
  function CapitalizeCopy(const Value: ansiarray): ansiarray; overload;

  function IsEmpty(const Value: ansiarray): Boolean;
  function StringOfChar(const Value: ansichar; const Count: Byte): ansiarray;

  function Pos(const SubStr: ansiarray; const S: ansiarray): Byte; overload;

  function Left(const Value: ansiarray; const Count: Byte): ansiarray;
  function Right(const Value: ansiarray; const Count: Byte): ansiarray;

  function UnLeft(const Value: ansiarray; const Count: Byte): ansiarray;
  function UnRightCopy(const Value: ansiarray; const Count: Byte): ansiarray;

  function TryStrToChar(const Value: ansiarray; var Dest: ansichar): Boolean;
  function StrToCharDef(const Value: ansiarray; const DefValue: ansichar): ansichar;
  function StrToChar(const Value: ansiarray): ansichar;

  function PtrToStr(const Value: pointer): ansiarray;
  function StrToPtr(const Value: ansiarray): pointer;

  function DelphiToPascal(const Value: string): ansiarray;
  function PascalToDelphi(const Value: ansiarray): string;

  function DuplicateStr(const Value: ansiarray): pointer;

  procedure UppercaseReplace(var Value: ansiarray);
  procedure LowercaseReplace(var Value: ansiarray);
  procedure TogglecaseReplace(var Value: ansiarray);
  procedure CapitalizeReplace(var Value: ansiarray);

  procedure GetMemStr(var Value: pointer);
  procedure FreeMemStr(var Value: pointer);

  procedure Empty(var Value: ansiarray);

  procedure MoveLeft
    (var Dest: ansiarray; const Source: ansiarray; const Count: Byte);
  procedure MoveRight
    (var Dest: ansiarray; const Source: ansiarray; const Count: Byte);
  procedure Move
    (var Dest: ansiarray; const Source: ansiarray; Index, Count: Byte);

  procedure Concat
    (var Dest: ansiarray; const Source: ansichar); overload;
  procedure Concat
    (var Dest: ansiarray; const Source: ansiarray); overload;

  function ConcatCopy
    (const A: ansiarray; B: ansichar): ansiarray; overload;
  function ConcatCopy
    (const A, B: ansiarray): ansiarray; overload;

  function MoveCopy
    (const Value: ansiarray; Index, Count: Byte): ansiarray; overload;
  function ReverseCopy
    (const Value: ansiarray): ansiarray;

  function SubStr
    (const PartialStr, FullStr: ansiarray; var Index: Integer): Boolean;
  function IsIdentifier(const Value: ansiarray): Boolean;
  function RangeToStr(const Min, Max: ansichar): ansiarray;

  function ReplaceChar
    (const Value: ansiarray; A, B: ansichar): ansiarray; overload;
  function ReplaceChars
    (const Value: ansiarray; Source: ansicharset; Dest: ansichar): ansiarray; overload;

  function ReplaceStrALL
    (var FullStr: ansiarray; const SubStr, Value: ansiarray): Integer;

  function ReplaceCopy
    (const FullStr, Source, Dest: ansiarray): ansiarray;
  function DeleteCopy
    (const FullStr, Source: ansiarray): ansiarray;

  procedure ReplacePos
    (var FullStr: ansiarray;
     var Index: Integer; const Source, Dest: ansiarray);
  procedure Replace
    (var FullStr: ansiarray; const Source, Dest: ansiarray);

  function RemoveCharCopy
   (const Value: ansiarray; IgnoreChar: ansichar): ansiarray;
  function RemoveCharsCopy
   (const Value: ansiarray; IgnoreChars: ansicharset): ansiarray;

  procedure RemoveChar(var Value: ansiarray; IgnoreChar: ansichar);
  procedure RemoveChars(var Value: ansiarray; IgnoreChars: ansicharset);

  function EqualAt
    (const Str, SubStr: ansiarray; const Index: Integer): Boolean;
  function SameAt
    (const Str, SubStr: ansiarray; const Index: Integer): Boolean;

  function Scan
    (const Str: ansiarray; const SubStr: ansiarray; {copy} Index: Word): Word; overload;

  function SkipChar
   (const Value: ansiarray; Index: Integer; ValidChar: ansichar): Integer;
  function SkipChars
   (const Value: ansiarray; Index: Integer; ValidChars: ansicharset): Integer;
  function SkipCharWhile
   (const Value: ansiarray; Index: Integer; ValidChar: ansichar): Integer;
  function SkipCharUntil
   (const Value: ansiarray; Index: Integer; BreakChar: ansichar): Integer;
  function SkipWhile
   (const Value: ansiarray; Index: Integer; ValidChars: ansicharset): Integer;
  function SkipUntil
   (const Value: ansiarray; Index: Integer; BreakChars: ansicharset): Integer;
  function SkipToken
    (const Value: ansiarray; var S, F: Integer): ansiarray;
  function SkipDigit
    (const Value: ansiarray; var I, F: Integer): ansiarray;
  function SkipAlpha
    (const Value: ansiarray; var I, F: Integer): ansiarray;

  function CopyFrom(const Source: ansiarray; Start, Finish: Integer): ansiarray;
  function ParseFrom(const Source: ansiarray; var Start, Finish: Integer): ansiarray;

  function SkipEmpty(const Start, Finish: Integer): Boolean;

  function ExtractCharUntil
   (const Value: ansiarray; Index: Integer; BreakChar: ansichar): ansiarray;
  function ExtractWhile
   (const Value: ansiarray; Index: Integer; SkipChars: ansicharset): ansiarray;
  function ExtractUntil
   (const Value: ansiarray; Index: Integer; BreakChars: ansicharset): ansiarray;
  function ExtractString(const Value: ansiarray; Index: Integer): ansiarray;

  function LastChar(const AString: ansiarray): ansichar;

  function AlignLeft
   (const Source: ansiarray; Count: Integer; Value: ansichar): ansiarray;
  function AlignRight
   (const Source: ansiarray; Count: Integer; Value: ansichar): ansiarray;

  { = } function Equal(const A, B: ansiarray): Boolean;
  function Compare(const A, B: ansiarray): TComparison;

  function EqualOptions
    (const A, B: ansiarray; Options: TStringOptions): Boolean;
  function CompareOptions
    (const A, B: ansiarray; Options: TStringOptions): TComparison;

implementation

{ global properties }

function getQuotedUppercase
  (const Value: ansiarray; const A, B: ansichar): ansiarray;
begin
  Result := Value;
  uktansiarrays.setQuotedUppercase(Result, A, B);
  // Goal: Returns a uppercase copy of the given string,
  // without modifying delimited substrings.

  // Objetivo: Regresa una copia en mayusculas de la cadena dada,
  // sin modificar a las subcadenas delimitadas.
end;

function getQuotedLowercase
  (const Value: ansiarray; const A, B: ansichar): ansiarray;
begin
  Result := Value;
  uktansiarrays.setQuotedLowercase(Result, A, B);
  // Goal: Returns a lowercase copy of the given nullstring,
  // without modifying delimited substrings.
  // Objetivo: Regresa una copia en minusculas de la cadena terminada en nulo,
  // dada sin modificar a las subcadenas delimitadas.
end;

function getTrimLeft(const Value: ansiarray): ansiarray;
var S: string;
begin
  S := PascalToDelphi(Value);
  S := SysUtils.TrimLeft(S);
  Result := DelphiToPascal(S);
  // Goal: Returns a copy of the given string without leading spaces.
  // Objetivo: Regresa una copia de la cadena dada sin espacios iniciales.
end;

function getTrimRightCopy(const Value: ansiarray): ansiarray;
var S: string;
begin
  S := PascalToDelphi(Value);
  S := SysUtils.TrimRight(S);
  Result := DelphiToPascal(S);
  // Goal: Returns a copy of the given string without trailing spaces.
  // Objetivo: Regresa una copia de la cadena dada sin espacios finales.
end;

function getTrim(const Value: ansiarray): ansiarray;
var S: string;
begin
  S := PascalToDelphi(Value);
  S := SysUtils.Trim(S);
  Result := DelphiToPascal(S);
  // Goal: Returns a copy of the given string
  // without leading & trailing spaces.

  // Objetivo: Regresa una copia de la cadena dada
  // sin espacios iniciales y finales.
end;

function getUnTrimLeft(const Value: ansiarray; Count: Integer): ansiarray;
var L: Integer;
begin
  L := Length(Value);
  Concat(Result, StringOfChar(#32, Count-L));
  Concat(Result, Value);
  // Goal: Returns a copy of the given string plus leading spaces.
  // Objetivo: Regresa una copia de la cadena dada mas espacios iniciales.
end;

function getUnTrimRightCopy(const Value: ansiarray; Count: Integer): ansiarray;
var L : Integer;
begin
  L := Length(Value);
  Concat(Result, Value);
  Concat(Result, StringOfChar(#32, Count-L));
  // Goal: Returns a copy of the given string plus trailing spaces.
  // Objetivo: Regresa una copia de la cadena dada mas espacios finales.
end;

procedure setLength(var Value: ansiarray; Count: Byte);
begin
  Value[0] := ansichar(Count);
end;

procedure setQuotedUppercase(var Value: ansiarray; const A, B: ansichar);
var InsideString: Boolean; Index: Integer; C: ansichar;
begin
  InsideString := FALSE;
  for Index := 1 to Length(Value) do
  begin
    C := Value[Index];
    if (InsideString)
      then InsideString := not (C = B)
      else InsideString := (C = A);
    // initial or final delimiter found ?
    // delimitador inicial o final encontrado ?

    if (not InsideString and IsMember(C, LowerSet))
      then Value[Index] := System.UpCase(C);
    // replace characters
    // reemplazar caracteres
  end;
  // Goal: Changes the given string into uppercase,
  // without modifying delimited substrings.

  // Objetivo: Cambia la cadena dada a mayusculas.
  // sin modificar a las subcadenas delimitadas.
end;

procedure setQuotedLowercase(var Value: ansiarray; const A, B: ansichar);
var InsideString: Boolean; Index: Integer; C: ansichar;
begin
  InsideString := FALSE;
  for Index := 1 to Length(Value) do
  begin
    C := Value[Index];
    if (InsideString)
      then InsideString := not (C = B)
      else InsideString := (C = A);
    // initial or final delimiter found ?
    // delimitador inicial o final encontrado ?

    if (not InsideString and IsMember(C, UpperSet))
      then Value[Index] := Chr(Ord(C) + 32)
    // replace characters
    // reemplazar caracteres
  end;
  // Goal: Changes the given string into lowercase,
  // without modifying delimited substrings.

  // Objetivo: Cambia la cadena dada a minusculas.
  // sin modificar a las subcadenas delimitadas.
end;

procedure setTrimLeft(var Value: ansiarray);
var S: string;
begin
  S := PascalToDelphi(Value);
  S := SysUtils.TrimLeft(S);
  Value := DelphiToPascal(S);
  // Goal: Returns the given string without leading spaces.
  // Objetivo: Regresa la cadena dada sin espacios iniciales.
end;

procedure setTrimRight(var Value: ansiarray);
var S: string;
begin
  S := PascalToDelphi(Value);
  S := SysUtils.TrimRight(S);
  Value := DelphiToPascal(S);
  // Goal: Returns the given string without trailing spaces.
  // Objetivo: Regresa la cadena dada sin espacios finales.
end;

procedure setTrim(var Value: ansiarray);
var S: string;
begin
  S := PascalToDelphi(Value);
  S := SysUtils.Trim(S);
  Value := DelphiToPascal(S);
  // Goal: Returns the given string without leading & trailing spaces.
  // Objetivo: Regresa la cadena dada sin espacios iniciales y finales.
end;

procedure setUnTrimLeft(var Value: ansiarray; Count: Integer);
var L: Integer;
begin
  L := Length(Value);
  Concat(Value, StringOfChar(#32, Count-L));
  Concat(Value, Value);
  // Goal: Returns the given string plus leading spaces.
  // Objetivo: Regresa la cadena dada mas espacios iniciales.
end;

procedure setUnTrimRightCopy(var Value: ansiarray; Count: Integer);
var L: Integer;
begin
  L := Length(Value);
  Concat(Value, Value);
  Concat(Value, StringOfChar(#32, Count-L));
  // Goal: Returns the given string plus trailing spaces.
  // Objetivo: Regresa la cadena dada mas espacios finales.
end;

{ global functions }

function Length(const Value: ansiarray): Word;
begin
  Result := ord(Value[0]);
  // Goal: Returns the length of the given string.
  // Objetivo: Regresa la longuitud de la cadena dada.
end;

function SameText(const A, B: ansiarray): Boolean;
var Index, Len: Integer;
begin
  Result := TRUE;
  Index := 0; Len := uktansiarrays.Length(A);
  while ((Index < Len) and Result) do
  begin
    Result := uktansichars.SameText(A[Index], B[Index]);
    Inc(Index);
  end;
//  Result := SysUtils.SameText(A, B);
  // Goal: Returns if 2 strings are equal, ignores sensitive case.
  // Objetivo: Regresa si 2 cadenas son iguales, ignorar caso sensitivo.
end;

function UppercaseCopy(const Value: ansiarray): ansiarray;
var I, L: Integer;
begin
  L := Length(Value);
  Result := Value;
  for I := 1 to L do
  begin
    Result[i] := uktansichars.UppercaseCopy(Value[i]);
  end;
  // Goal: Returns a uppercase copy of the given ansiarray.
  // Objetivo: Regresa una copia en mayusculas de la cadena dada.
end;

function LowercaseCopy(const Value: ansiarray): ansiarray;
var I, L: Integer;
begin
  L := Length(Value);
  Result := Value;
  for I := 1 to L do
  begin
    Result[i] := uktansichars.LowercaseCopy(Value[i]);
  end;
  // Goal: Returns a lowercase copy of the given ansiarray.
  // Objetivo: Regresa una copia en minusculas de la cadena dada.
end;

function TogglecaseCopy(const Value: ansiarray): ansiarray;
var I, L: Integer; C: char;
begin
  L := Length(Value);
  Result := Value;
  for I := 1 to L do
  begin
    C := Value[i];
    if (IsCharLower(C))
      then CharUpperBuff(@C, 1)
      else CharLowerBuff(@C, 1);
    Result[i] := C;
  end;
  // Goal: Swaps the sensitive case of each character in the given ansiarray.
  // Objetivo: Cambia el caso sensitivo de cada caracter en la cadena.
end;

function CapitalizeCopy(const Value: ansiarray): ansiarray;
var I, Last: Integer; C: char; MakeUppercase: Boolean;
begin
  Result := '';
  MakeUppercase := TRUE;
  Last := Length(Value);
  for I := 1 to Last do
  begin
    C := Value[i];
    if (C <> #32) then
    begin
      if (MakeUppercase) then
      begin
        CharUpperBuff(@C, 1);
        MakeUppercase := FALSE;
      end else CharLowerBuff(@C, 1);
    end else MakeUppercase := TRUE;
    Concat(Result, C);
  end;
  // Goal: Returns a copy with uppercase initials of the given ansiarray.
  // Objetivo: Regresa una copia con iniciales en mayusculas de la cadena dada.
end;

function getUppercase(const Value: ansiarray): ansiarray;
var S: string;
begin
  S := PascalToDelphi(Value);
  S := SysUtils.ANSIUpperCase(S);
  Result := DelphiToPascal(S);
  // Goal: Returns a uppercase copy of the given string.
  // Objetivo: Regresa una copia en mayusculas de la cadena dada.
end;

function getLowercase(const Value: ansiarray): ansiarray;
var S: string;
begin
  S := PascalToDelphi(Value);
  S := SysUtils.ANSILowerCase(S);
  Result := DelphiToPascal(S);
  // Goal: Returns a lowercase copy of the given string.
  // Objetivo: Regresa una copia en minusculas de la cadena dado.
end;

function IsEmpty(const Value: ansiarray): Boolean;
begin
  Result := Length(Value) = 0;
  // Goal: Returns if a ansiarray is empty.
  // Objetivo: Regresa si una cadena esta vacia.
end;

function StringOfChar(const Value: ansichar; const Count: Byte): ansiarray;
begin
  Empty(Result);
  System.FillChar(Result, Count, Value);
  // Goal: Returns a ansiarray of the same ansichar.
  // Objetivo: Regresa una cadena del mismo caractaer.
end;

function Pos(const SubStr: ansiarray; const S: ansiarray): Byte;
begin
  Result := System.Pos(SubStr, S);
  // Objetivo: Regresa el indice de la primer ocurrencia de "Substr".
  // Goal: Returns the index of the first ocurrence of "Substr".
end;

function Left(const Value: ansiarray; const Count: Byte): ansiarray;
begin
  uktansiarrays.MoveLeft(Result, Value, Count);
  // Goal: Returns the leftmost characters of "Value".
  // Objetivo: Regresa los caracteres mas a la izquierda de "Value".
end;

function Right(const Value: ansiarray; const Count: Byte): ansiarray;
begin
  uktansiarrays.MoveRight(Result, Value, Count);
  // Goal: Returns the rightmost characters of "Value".
  // Objetivo: Regresa los caracteres mas a la derecha de "Value".
end;

function UnLeft(const Value: ansiarray; const Count: Byte): ansiarray;
begin
  uktansiarrays.Move(Result, Value, Count, Length(Value));
  // Goal: Removes the leftmost characters of "Value".
  // Objetivo: Remueve los caracteres mas a la izquierda de "Value".
end;

function UnRightCopy(const Value: ansiarray; const Count: Byte): ansiarray;
var Index: Integer;
begin
  Index := Succ(Length(Value)-Count);
  uktansiarrays.Move(Result, Value, 1, Index);
  // Goal: Removes the RightCopymost characters of "Value".
  // Objetivo: Remueve los caracteres mas a la derecha de "Value".
end;

function TryStrToChar(const Value: ansiarray; var Dest: ansichar): Boolean;
begin
  Result := (Length(Value) < 1);
  if (Result)
    then Dest := Value[1];
  // Goal: To cast a "ansiarray" in to a "ansichar".
  // Objetivo: Convertir un "ansiarray" en un "ansichar".
end;

function StrToCharDef(const Value: ansiarray; const DefValue: ansichar): ansichar;
begin
  Result := DefValue;
  if (not TryStrToChar(Value, Result))
    then Result := DefValue;
  // Goal: To cast a "ansiarray" in to a "ansichar".
  // Objetivo: Convertir un "ansiarray" en un "ansichar".
end;

function StrToChar(const Value: ansiarray): ansichar;
begin
  Result := #0;
  TryStrToChar(Value, Result)
  // Goal: To cast a "ansiarray" in to a "ansichar".
  // Objetivo: Convertir un "ansiarray" en un "ansichar".
end;

function PtrToStr(const Value: pointer): ansiarray;
begin
  Result := Pansiarray(Value)^;
  // Goal: Copy an string into a dynamic string.
  // Objetivo: Copiar una cadena a una cadena dinamica.
end;

function StrToPtr(const Value: ansiarray): pointer;
begin
  Result := @Value;
  // Goal: Copy an string into a dynamic string.
  // Objetivo: Copiar una cadena a una cadena dinamica.
end;

function DelphiToPascal(const Value: string): ansiarray;
var Count: Integer;
begin
  Count := Math.Min(System.Length(Value), sdvAnsiarraysize);
  uktansiarrays.Empty(Result);
  System.Move(Value[1], Result[1], Count);
  uktansiarrays.setLength(Result, Count);
end;

function PascalToDelphi(const Value: ansiarray): string;
begin
  System.SetLength(Result, sdvAnsiarraysize);
  System.Move(Value[1], Result[1], sdvAnsiarraysize);
  System.SetLength(Result, Length(Value));
end;

function DuplicateStr(const Value: ansiarray): pointer;
begin
  Result := nil;
  GetMemStr(Result);
  System.Move(Value, Result^, sdvAnsiarraysize);
  // Objetivo: Regresa en memoria dinamica una copia de la cadena dada.
  // Goal: Returns a in dynamic memory of the given string.
end;

procedure UppercaseReplace(var Value: ansiarray);
begin
  Value := UpperCaseCopy(Value);
  // Goal: Changes the given ansiarray into uppercase.
  // Objetivo: Cambia la cadena dada a mayusculas.
end;

procedure LowercaseReplace(var Value: ansiarray);
begin
  Value := LowerCaseCopy(Value);
  // Goal: Changes the given ansiarray into lowercase.
  // Objetivo: Cambia la cadena dada a minusculas.
end;

procedure TogglecaseReplace(var Value: ansiarray);
begin
  Value := TogglecaseCopy(Value);
  // Goal: Swaps the sensitive case of each character in the given ansiarray.
  // Objetivo: Cambia el caso sensitivo de cada caracter en la cadena.
end;

procedure CapitalizeReplace(var Value: ansiarray);
begin
  Value := CapitalizeCopy(Value);
  // Goal: Changes the given string into capitalize.
  // Objetivo: Cambia la cadena dada a minusculas.
end;

procedure GetMemStr(var Value: pointer);
var
  V: Pansiarray absolute Value;
  L: Integer;
begin
  Value := nil; L := sdvAnsiarraysize;
  GetMem(Value, L);
  Empty(V^);
end;

procedure FreeMemStr(var Value: pointer);
begin
  FreeMem(Value, sdvAnsiarraysize);
  Value := nil;
end;

procedure Empty(var Value: ansiarray);
begin
  System.FillChar(Value, sdvAnsiarraysize, #0);
  // Goal: Clear a ansi string.
  // Objetivo: Limpia una cadena ansi.
end;

procedure MoveLeft
  (var Dest: ansiarray; const Source: ansiarray; const Count: Byte);
begin
  uktansiarrays.Move(Dest, Source, 1, Count);
  // Goal: Returns the leftmost characters of "Value".
  // Objetivo: Regresa los caracteres mas a la izquierda de "Value".
end;

procedure MoveRight
  (var Dest: ansiarray; const Source: ansiarray; const Count: Byte);
var Index: Integer;
begin
  Index := Succ(Length(Source)-Count);
  uktansiarrays.Move(Dest, Source, Index, Count);
  // Goal: Returns the rightmost characters of "Value".
  // Objetivo: Regresa los caracteres mas a la derecha de "Value".
end;

procedure Move
  (var Dest: ansiarray; const Source: ansiarray; Index, Count: Byte);
begin
  uktansiarrays.Empty(Dest);
  System.Move(Source[Index], Dest, Count);
  uktansiarrays.setLength(Dest, Count);
  // Objetivo: Copiar el contenido de una cadena a un nueva cadena.
  // Goal: Copy the contents of a string into a new string.
end;

procedure Concat(var Dest: ansiarray; const Source: ansichar);
var Len, Index: Word;
begin
  Len := Length(Dest);
  if (Len < sdvAnsiarraysize) then
  begin
    Index := Succ(Len);
    Dest[Index] := Source;
    setLength(Dest, Index);
  end;
  // Objetivo: Agregar  un caracter al final de la cadena dada.
  // Goal: Add a character at the end of the given string.
end;

procedure Concat(var Dest: ansiarray; const Source: ansiarray);
var SourceLen, DestLen, Index, Count, MaxLen: Word;
begin
  SourceLen := Length(Source);
  DestLen   := Length(Dest);
  Index     := Succ(DestLen);

  MaxLen   := SourceLen + DestLen;
  Count    := Math.Min(SourceLen + DestLen, sdvAnsiarraysize - SourceLen);

  System.Move(Source[1], Dest[Index], Count);
  uktansiarrays.setLength(Dest, MaxLen);
end;

function ConcatCopy
  (const A: ansiarray; B: ansichar): ansiarray;
begin
  uktansiarrays.Empty(Result);
  uktansiarrays.Concat(Result, A);
  uktansiarrays.Concat(Result, B);
  // Objetivo: Agregar  un caracter al final de la cadena dada.
  // Goal: Add a character at the end of the given string.
end;

function ConcatCopy
  (const A, B: ansiarray): ansiarray;
begin
  uktansiarrays.Empty(Result);
  uktansiarrays.Concat(Result, A);
  uktansiarrays.Concat(Result, B);
end;

function MoveCopy
  (const Value: ansiarray; Index, Count: Byte): ansiarray;
begin
  uktansiarrays.Empty(Result);
  uktansiarrays.Move(Result, Value, Index, Count);
  // Objetivo: Copiar el contenido de una cadena a un nueva cadena.
  // Goal: Copy the contents of a string into a new string.
end;

function ReverseCopy(const Value: ansiarray): ansiarray;
begin
  Result := '';
end;

function SubStr(const PartialStr, FullStr: ansiarray; var Index: Integer): Boolean;
begin
  Index := Pos(PartialStr, FullStr);
  Result := (Index > 0);
  // Goal: Returns if a ansiarray is contained by other ansiarray.
  // Objetivo: Regresa si una cadena esta contenida en otra.
end;

function IsIdentifier(const Value: ansiarray): Boolean;
var I: Integer;
begin
  Result := False;
  if (Length(Value) = 0) or not IsMember(Value[1], AlphaSet) then Exit;
  for I := 2 to Length(Value) do
    if (not IsMember(Value[I], IDSet))
      then Exit;
  Result := TRUE;
  // Goal: To return if a ansiarray is a valid identifier.
  // Objetivo: Regresar si una cadena es un identificador valido.
end;

function RangeToStr(const Min, Max: ansichar): ansiarray;
var I: ansichar; { s := 'a' .. 'z'; }
begin
  uktansiarrays.Empty(Result);
  for i := Min to Max do
    Concat(Result, i);
  // Goal: Transform a range of characters into a "ansiarray".
  // Objetivo: Transformar un rango de caracteres en un "ansiarray".

  // Warning: "Min" must be lesser than "Max".
  // Advertencia: "Min" debera ser menor que "Max".
end;

function ReplaceChar(const Value: ansiarray; A, B: ansichar): ansiarray;
var I, L: Integer;
begin
  Result := Value; L := Length(Value);
  for i := 1 to L do
    if (Result[i] = A)
      then Result[i] := B;
  // Goal: Replace a specific character from a ansiarray.
  // Objetivo: Reemplazar un caracter en especifico de una cadena.
end;

function ReplaceChars
 (const Value: ansiarray; Source: ansicharset; Dest: ansichar): ansiarray;
var I: Integer;
begin
  uktansiarrays.Empty(Result);
  for i := 1 to Length(Value) do
    if (IsMember(Value[i], Source))
      then Concat(Result, Dest)
      else Concat(Result, Value[i]);
  // Goal: Replace a specific character set from a ansiarray.
  // Objetivo: Reemplazar un conjunto caracter en especifico de una cadena.
end;

function ReplaceStrALL
  (var FullStr: ansiarray; const SubStr, Value: ansiarray): Integer;
var Finished: Boolean; Index: Integer;
begin
  Index := 0;
  Result := 0;
  repeat
    ReplacePos(FullStr, Index, SubStr, Value);
    Finished := (Index = 0);
    if (not Finished)
      then Inc(Result);
  until (Finished);
  // Goal: Replaces all the "SubStr" found in "FullStr" with "Value".
  // Objetivo: Reemplaza todas las "SubStr" encontradas en "FullStr" por "Value".}
end;

function ReplaceCopy
  (const FullStr, Source, Dest: ansiarray): ansiarray;
var Index: Integer;
begin
  Result := FullStr;
  Index  := 0;
  ReplacePos(Result, Index, Source, Dest);
  // Goal: Returns a copy of the string with the given substring replaced.
  // Objetivo: Regresa una copia de la cadena con la subcadena reemplazada.
end;

function DeleteCopy(const FullStr, Source: ansiarray): ansiarray;
begin
  Result := ReplaceCopy(FullStr, Source, '');
  // Goal: Returns a copy of the ansiarray with the given substring deleted.
  // Objetivo: Regresa una copia de la cadena con la subcadena eliminada.
end;

procedure ReplacePos
  (var FullStr: ansiarray;
   var Index: Integer; const Source, Dest: ansiarray);
var BeforeCount, AfterCount: Integer; BeforeString, AfterString: ansiarray;   
begin
  BeforeString := '';
  AfterString  := '';
  Index := Pos(Source, FullStr);
  // localizar en que posicion inicia la cadena que se va BeforeString reemplazar
  // locate in which location starts the string to be replaced

  if (Index > 0) then
  begin
    AfterCount := Index + Length(Source);
    // obtener la longuitud de la cadena despues de la parte antes a reemplazar
    // obtain the length of the string after the section to be replaced

    BeforeCount := Length(FullStr) - Length(Source);
    // obtener la longuitud de la cadena antes de la parte antes a reemplazar
    // obtain the length of the string before the section to be replaced

    if (Index <> 0) then
    begin
      uktansiarrays.MoveLeft(BeforeString, FullStr, BeforeCount);
      uktansiarrays.MoveRight(AfterString, FullStr, AfterCount);
      // extraer secciones antes y despues de cadena nodeseada
      // extract before & after section of unwanted string

      uktansiarrays.Empty(FullStr);
      uktansiarrays.Concat(FullStr, BeforeString);
      uktansiarrays.Concat(FullStr, Dest);
      uktansiarrays.Concat(FullStr, AfterString);
    end;
  end;
  // Goal: Replaces the first "Source" found in "FullStr" with "Dest".
  // Objetivo: Reemplaza la primera "Source" encontrada en "FullStr" por "Dest".}
end;

procedure Replace
  (var FullStr: ansiarray; const Source, Dest: ansiarray);
var Index: Integer;
begin
  Index := 0;
  ReplacePos(FullStr, Index, Source, Dest);
  // Goal: Replaces the first "Source" found in "FullStr" with "Dest".
  // Objetivo: Reemplaza la primera "Source" encontrada en "FullStr" por "Dest".}
end;

function RemoveCharCopy
  (const Value: ansiarray; IgnoreChar: ansichar): ansiarray;
var I: Integer;
begin
  uktansiarrays.Empty(Result);
  for i := 1 to Length(Value) do
    if (Value[i] <> IgnoreChar)
      then uktansiarrays.Concat(Result, Value[i]);
  // Goal: Delete a specific character from a string.
  // Objetivo: Eliminar un caracter en especifico de una cadena.
end;

function RemoveCharsCopy
  (const Value: ansiarray; IgnoreChars: ansicharset): ansiarray;
var I: Integer;
begin
  uktansiarrays.Empty(Result);
  for i := 1 to Length(Value) do
    if (not IsMember(Value[i], IgnoreChars))
      then uktansiarrays.Concat(Result, Value[i]);
  // Goal: Delete a specific character set from a string.
  // Objetivo: Eliminar un conjunto caracter en especifico de una cadena.
end;

procedure RemoveChar(var Value: ansiarray; IgnoreChar: ansichar);
begin
  Value := RemoveCharCopy(Value, IgnoreChar);
  // Goal: Delete a specific character from a string.
  // Objetivo: Eliminar un caracter en especifico de una cadena.
end;

procedure RemoveChars(var Value: ansiarray; IgnoreChars: ansicharset);
begin
  Value := RemoveCharsCopy(Value, IgnoreChars);
  // Goal: Delete a specific character set from a string.
  // Objetivo: Eliminar un conjunto caracter en especifico de una cadena.
end;

function EqualAt
  (const Str, SubStr: ansiarray; const Index: Integer): Boolean;
//var CanContinue: Boolean; Last1, Last2, I, J: Integer;
begin
  Result := FALSE;
//  Result := (System.Copy(Str, Index, Length(SubStr)) = SubStr);

  (*
  Last1 := Length(Str);
  Last2 := Length(SubStr);
  I := Index;
  J := 1;

  repeat
    CanContinue := (I <

    Inc(I);
    Inc(J);
  until (CanContinue);
  *)
  // Goal: Compares the "Str" ansiarray beggining at "Index" character
  // with the ansiarray "SubStr".

  // Objetivo: Compara la cadena "Str" comenzando con el caracter "Index"
  // con la cadena "SubStr".
end;

function SameAt
  (const Str, SubStr: ansiarray; const Index: Integer): Boolean;
begin
  Result := FALSE;
end;

function Scan
 (const Str: ansiarray; const SubStr: ansiarray; {copy} Index: Word): Word;
var Len: Word; Found: Boolean;
begin
  Len := Length(Str);
  Found  := FALSE;

  while (not Found and (Index <= Len)) do
  begin
    if (EqualAt(Str, SubStr, Index))
      then Found := TRUE
      else Inc(Index);
  end;

  if (Found)
    then Result := Index
    else Result := 0;
  // Goal: Searches for a substring inside other ansiarray
  // beginning at the "Index" ansichar and returns the next character.

  // Objetivo: Busca una subcadena adentro de otra cadena
  // comenzando en el caracter numero "indice" y regresa el siguiente caracter.
end;

function SkipChars
  (const Value: ansiarray; Index: Integer; ValidChars: ansicharset): Integer;
var L: Integer;
begin
  Result := Index; L := Length(Value);
  if ((Result <= L) and IsMember(Value[Result], ValidChars))
    then Inc(Result);
  // Goal: Returns a single character.
  // Objetivo: Regresa un solo caracter.
end;

function SkipChar
  (const Value: ansiarray; Index: Integer; ValidChar: ansichar): Integer;
var L: Integer;
begin
  Result := Index; L := Length(Value);
  if ((Result <= L) and (Value[Result] = ValidChar))
    then Inc(Result);
  // Goal: Returns a single character. }
  // Objetivo: Regresa un solo caracter.
end;

function SkipCharWhile
  (const Value: ansiarray; Index: Integer; ValidChar: ansichar): Integer;
var L: Integer;
begin
  Result := Index; L := Length(Value);
  while ((Result <= L) and (Value[Result] = ValidChar)) do
   Inc(Result);
  // Goal: Returns a group of characters. }
  // Objetivo: Regresa un grupo de caracteres.
end;

function SkipCharUntil
  (const Value: ansiarray; Index: Integer; BreakChar: ansichar): Integer;
var L: Integer;
begin
  Result := Index; L := Length(Value);
  while (Result <= L) and (Value[Result] <> BreakChar) do
   Inc(Result);
  // Goal: Returns a group of characters. }
  // Objetivo: Regresa un grupo de caracteres.
end;

function SkipWhile
  (const Value: ansiarray; Index: Integer; ValidChars: ansicharset): Integer;
var L: Integer;
begin
  Result := Index; L := Length(Value);
  while (Result <= L) and IsMember(Value[Result], ValidChars) do
   Inc(Result);
  // Goal: Returns a group of non-space characters. }
  // Objetivo: Regresa un grupo de caracteres que no son espacios.
end;

function SkipUntil
  (const Value: ansiarray; Index: Integer; BreakChars: ansicharset): Integer;
var L: Integer;
begin
  Result := Index; L := Length(Value);
  while (Result <= L) and not IsMember(Value[Result], BreakChars) do
   Inc(Result);
  // Goal: Returns a group of non-space characters. }
  // Objetivo: Regresa un grupo de caracteres que no son espacios.
end;

function SkipToken
  (const Value: ansiarray; var S, F: Integer): ansiarray;
var StartIndex, FinishIndex: Integer;
begin
  StartIndex  := S;

  StartIndex  := SkipCharWhile(Value, StartIndex, #32);
  FinishIndex := SkipCharUntil(Value, StartIndex, #32);
  Result      := CopyFrom(Value, StartIndex, FinishIndex);

  S := StartIndex;
  F := FinishIndex;
end;

function SkipDigit
  (const Value: ansiarray; var I, F: Integer): ansiarray;
begin
  I := SkipCharWhile(Value, I, #32);
  F := SkipWhile(Value, I, DigitSet);
  Result := CopyFrom(Value, I, F);
  I := F;
end;

function SkipAlpha
  (const Value: ansiarray; var I, F: Integer): ansiarray;
begin
  I := SkipCharWhile(Value, I, #32);
  F := SkipWhile(Value, I, AlphaSet);
  Result := CopyFrom(Value, I, F);
  I := F;
end;

function CopyFrom
  (const Source: ansiarray; Start, Finish: Integer): ansiarray;
var Count: Integer;
begin
  Count := Succ(Finish - Start);
  uktansiarrays.Move(Result, Source, Start, Count);
  // Goal: Returns a substring from "start" to "finish".
  // Objetivo: Regresa una subcadena desde "start" hasta "finish". }
end;

function ParseFrom
  (const Source: ansiarray; var Start, Finish: Integer): ansiarray;
var Count: Integer;
begin
  Count := Succ(Finish - Start);
  uktansiarrays.Move(Result, Source, Start, Count);
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
 (const Value: ansiarray; Index: Integer; BreakChar: ansichar): ansiarray;
var I, L: Integer;
begin
  uktansiarrays.Empty(Result);

  I := Index; L := Length(Value);
  while ((I <= L) and (Value[I] = #32)) do
    Inc(I);
  // Jump leading spaces
  // Brincar espacios al inicio

  while ((I <= L) and (Value[I] <> BreakChar)) do
  begin
    uktansiarrays.Concat(Result, Value[I]);
    Inc(I);
  end;
  // Goal: Returns a group of non-space characters. }
  // Objetivo: Regresa un grupo de caracteres que no son espacios.
end;

function ExtractWhile
  (const Value: ansiarray; Index: Integer; SkipChars: ansicharset): ansiarray;
var I, L: Integer;
begin
  uktansiarrays.Empty(Result);

  I := Index; L := Length(Value);
  while ((I <= L) and (Value[I] = #32)) do
    Inc(I);
  // Jump leading spaces
  // Brincar espacios al inicio

  while ((I <= L) and IsMember(Value[I], SkipChars)) do
  begin
    uktansiarrays.Concat(Result, Value[I]);
    Inc(I);
  end;
  // Goal: Returns a group of non-space selected characters. }
  // Objetivo: Regresa un grupo de caracteres seleccionados que no son espacios.
end;

function ExtractUntil
  (const Value: ansiarray; Index: Integer; BreakChars: ansicharset): ansiarray;
var I, L: Integer;
begin
  uktansiarrays.Empty(Result);

  I := Index; L := Length(Value);
  while ((I <= L) and (Value[I] = #32)) do
    Inc(I);
  // Jump leading spaces
  // Brincar espacios al inicio

  while ((I <= L) and not IsMember(Value[I], BreakChars)) do
  begin
    uktansiarrays.Concat(Result, Value[I]);
    Inc(I);
  end;
  // Goal: Returns a group of non-space characters. }
  // Objetivo: Regresa un grupo de caracteres que no son espacios.
end;

function ExtractString(const Value: ansiarray; Index: Integer): ansiarray;
var I, L: Integer;
begin
  uktansiarrays.Empty(Result);

  I := Index; L := Length(Value);
  while ((I <= L) and (Value[I] = #32)) do
    Inc(I);
  // Jump leading spaces
  // Brincar espacios al inicio

  while ((I <= L) and (Value[I] <> #32)) do
  begin
    uktansiarrays.Concat(Result, Value[I]);
    Inc(I);
  end;
  // Goal: Returns a group of non-space characters. }
  // Objetivo: Regresa un grupo de caracteres que no son espacios.
end;

function LastChar(const AString: ansiarray): ansichar;
var L: Integer;
begin
  L := Length(AString);
  if (L < 1)
    then Result := #0
    else Result := AString[ L ];
  // Goal: To return the last ansichar of a ansiarray.
  // Objetivo: Regresar el ultimo caracter de una cadena.
end;

function AlignLeft
 (const Source: ansiarray; Count: Integer; Value: ansichar): ansiarray;
var Difference: Integer;
begin
  Difference := Count - Length(Source);
  uktansiarrays.Empty(Result);
  uktansiarrays.Concat(Result, Source);
  uktansiarrays.Concat(Result, StringOfChar(Value, Difference));
  // Goal: Add characters to the right of a string.
  // Objetivo: Agregar caracteres a la derecha o final de una cadena.
end;

function AlignRight
 (const Source: ansiarray; Count: Integer; Value: ansichar): ansiarray;
var Difference: Integer;
begin
  Difference := Count - Length(Source);
  uktansiarrays.Empty(Result);
  uktansiarrays.Concat(Result, StringOfChar(Value, Difference));
  uktansiarrays.Concat(Result, Source);
  // Goal: Add characters to the left of a string.
  // Objetivo: Agregar caracteres a la izquierda o inicio de una cadena.
end;

function Equal(const A, B: ansiarray): Boolean;
begin
  {$ifdef Delphi}
  Result := (A = B);
  {$else}
  Result := (uktlibc.memcmp(@A, @B, sizeof(ansiarray)) = 0);
  {$endif}
  // Goal: Returns if 2 strings are equal.
  // Objetivo: Regresa si 2 cadenas son iguales.
end;

function Compare(const A, B: ansiarray): TComparison;
begin
  {$ifdef Delphi}
  if (A = B)
    then Result := cmpEqual
  else if (A < B)
    then Result := cmpLower
  else Result := cmpHigher
  {$else}
  Result := uktlibc.memcmp(@A, @B, sizeof(ansiarray));
  {$endif}
  // Goal: Returns the comparison between 2 strings.
  // Objetivo: Regresa la comparacion de 2 cadenas.
end;

function EqualOptions
  (const A, B: ansiarray; Options: TStringOptions): Boolean;
var C, D: ansiarray;
begin
  {$ifdef Delphi}
  case Options of
    soExactMatch: Result := (A = B);
    soForceMatch: Result := (getUppercase(A) = B);
    soForceFirst: Result := (getUppercase(A) = getUppercase(B));
    soForceLast:  Result := (A = getUppercase(B));
    else Result := FALSE;
  end;
  {$else}
  case Options of
    soExactMatch:
    begin
      C := A;
      D := B;
    end;
    soForceMatch:
    begin
      C := getUppercase(A);
      D := B;
    end;
    soForceFirst:
    begin
      C := getUppercase(A);
      D := getUppercase(B);
    end;
    soForceLast:
    begin
      C := A;
      D := getUppercase(B);
    end;
    else Result := FALSE;
  end;
  Result := (uktlibc.memcmp(@C, @D, sizeof(ansiarray)) = 0);
  {$endif}
  // Goal: Returns if 2 strings are equals uppon the given options.
  // Objetivo: Regresa si 2 cadenas son iguales basado en las opciones dadas.
end;

function CompareOptions
  (const A, B: ansiarray; Options: TStringOptions): TComparison;
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

end.
