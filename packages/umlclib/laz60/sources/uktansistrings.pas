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

unit uktansistrings;

interface
uses
{$IFDEF MSWINDOWS}
  Windows, 
  //Messages, 
  //Consts,
{$ENDIF}
{$IFDEF LINUX}
  Types, Libc,
{$ENDIF}
  SysConst,
  SysUtils,
  //uktcomparisons,
  uktansichars,
  uktansicharsets,
  uktansicharsetconsts,
  uktstrings,
  dummy;

(* global types *)

type
  Tansistring = System.ansistring;
  Pansistring = ^Tansistring;

(* global properties *)

  function getLength(const AValue: ansistring): Word; overload;

(* global functions *)

  function IsEmpty(const AValue: ansistring): Boolean;
  function SameText(const A, B: ansistring): Boolean;
  function IsStringOfChar(const S: ansistring; C: ansichar): Boolean;

  function Length(const AValue: ansistring): Word; overload;

  function UppercaseCopy(const AValue: ansistring): ansistring; overload;
  function LowercaseCopy(const AValue: ansistring): ansistring; overload;
  function TogglecaseCopy(const AValue: ansistring): ansistring; overload;
  function CapitalizeCopy(const AValue: ansistring): ansistring; overload;

  function ConcatCopy
    (const A: ansistring; const B: ansichar): ansistring; overload;
  function ConcatCopy
    (var A, B: ansistring): ansistring; overload;

  function StringOfChar(const AValue: ansichar; const ACount: Byte): ansistring;

  function CAR(const AValue: ansistring): ansistring; overload;
  function CDR(const AValue: ansistring): ansistring; overload;

  function CharPos
    (const SubStr: ansichar; const S: ansistring): Word;
  function CharPosReverse
    (const SubStr: ansichar; const S: ansistring): Word;

  function CharSetPos
    (const Items: ansicharset; const S: ansistring): Word;
  function CharSetPosReverse
    (const Items: ansicharset; const S: ansistring): Word;

  function LeadPos
    (const SubStr: ansistring; const S: ansistring): Boolean;
  function LeadPosSame
    (const SubStr: ansistring; const S: ansistring): Boolean;

  function StartsWith
    (const SubStr: string; const S: string): Boolean;
  function FinishesWith
    (const SubStr: string; const S: string): Boolean;

  function PosForward
    (const SubStr, FullStr: ansistring): Word; overload;
  function PosSameForward
    (const SubStr: ansistring; const S: ansistring): Integer;

  function PosBackward
    (const SubStr, FullStr: ansistring): Word; overload;
  function PosSameBackward
    (const SubStr, FullStr: ansistring): Word;

  function LeftCopy(const AValue: ansistring; const ACount: Byte): ansistring;
  function RightCopy(const AValue: ansistring; const ACount: Byte): ansistring;

  function TryStrToChar
    (const AValue: ansistring; var Dest: ansichar): Boolean;
  function StrToCharDef
    (const AValue: ansistring; const DefAValue: ansichar): ansichar;
  function StrToChar(const AValue: ansistring): ansichar;

  function CppEscToStr(const AValue: ansistring): ansistring;
  function StrToCppEsc(const AValue: ansistring): ansistring;

(* global procedures *)

  procedure Move
    (var Dest: ansistring; const Source: ansistring; AIndex, ACount: Integer);
  procedure MoveLeft
    (var Dest: ansistring; const Source: ansistring; const ACount: Integer);
  procedure MoveRight
    (var Dest: ansistring; const Source: ansistring; const ACount: Integer);

  procedure UppercaseReplace(var AValue: ansistring); overload;
  procedure LowercaseReplace(var AValue: ansistring); overload;
  procedure TogglecaseReplace(var AValue: ansistring); overload;
  procedure CapitalizeReplace(var AValue: ansistring); overload;

  procedure Empty(var AValue: ansistring);

  procedure Concat
    (var Dest: ansistring; const Source: ansichar); overload;
  procedure Concat
    (var Dest: ansistring; const Source: ansistring); overload;

  function ReplaceForwardCopy
    (const AValue, Source, Dest: ansistring): ansistring;
  function ReplaceForwardCopySame
    (const AValue, Source, Dest: ansistring): ansistring;

  function ReplaceBackwardCopy
    (const AValue, Source, Dest: ansistring): ansistring;
  function ReplaceBackwardCopySame
    (const AValue, Source, Dest: ansistring): ansistring;

  function ReplaceForwardALLCopy
    (const AValue, Source, Dest: ansistring): ansistring;
  function ReplaceBackwardALLCopy
    (const AValue, Source, Dest: ansistring): ansistring;

  procedure ReplaceForwardPosCopy
    (var AValue: ansistring;
     var AIndex: Integer; const Source, Dest: ansistring);
  procedure ReplaceForwardPosCopySame
    (var AValue: ansistring;
     var AIndex: Integer; const Source, Dest: ansistring);

  procedure ReplaceBackwardPosCopy
    (var AValue: ansistring;
     var AIndex: Integer; const Source, Dest: ansistring);
  procedure ReplaceBackwardPosCopySame
    (var AValue: ansistring;
     var AIndex: Integer; const Source, Dest: ansistring);

  function DeleteForwardCopy
    (const AValue, Source: ansistring): ansistring;
  function DeleteBackwardCopy
    (const AValue, Source: ansistring): ansistring;

  function DeleteALLCopy
    (const AValue, Source: ansistring): ansistring;

  function SkipChar
   (const AValue: ansistring; AIndex: Integer; ValidChar: ansichar): Integer;
  function SkipChars
   (const AValue: ansistring; AIndex: Integer; ValidChars: ansicharset): Integer;
  function SkipCharWhile
   (const AValue: ansistring; AIndex: Integer; ValidChar: ansichar): Integer;
  function SkipCharUntil
   (const AValue: ansistring; AIndex: Integer; BreakChar: ansichar): Integer;
  function SkipWhile
   (const AValue: ansistring; AIndex: Integer; ValidChars: ansicharset): Integer;
  function SkipUntil
   (const AValue: ansistring; AIndex: Integer; BreakChars: ansicharset): Integer;
  function SkipToken(const AValue: ansistring; var S, F: Integer): ansistring;
  function SkipDigit(const AValue: ansistring; var I, F: Integer): ansistring;
  function SkipLetter(const AValue: ansistring; var I, F: Integer): ansistring;

  function CopyFrom(const Source: ansistring; Start, Finish: Integer): string;
  function ParseFrom(const Source: ansistring; var Start, Finish: Integer): ansistring;

(* global operators *)

  function Add
    (const A: ansistring;
     const B: ansichar): ansistring; overload; // operator +
  function Add
    (var A, B: ansistring): ansistring; overload; // operator +

  procedure Assign
    (var Dest: ansistring;
     const Source: ansistring); overload; // operator :=
  procedure Assign
    (var Dest: ansistring;
     const Source: ansichar); overload; // operator :=

type
  TCompareANSIStrAt =
    {^}function (const SubStr, Str: ansistring; var AIndex: Word): Boolean;

  function SameStrAt
    (const SubStr, Str: ansistring; var AIndex: Word): Boolean;
  function EqualStrAt
    (const SubStr, Str: ansistring; var AIndex: Word): Boolean;

  function SameStrIn
    (const SubStr, Str: ansistring; var AIndex: Word): Boolean;
  function EqualStrIn
    (const SubStr, Str: ansistring; var AIndex: Word): Boolean;

  function SameStr
    (const SubStr, Str: ansistring; var AIndex: Word): Boolean;
  function EqualStr
    (const SubStr, Str: ansistring; var AIndex: Word): Boolean;

  function Search
    (const SubStr, Str: ansistring; var AIndex: Word;
     CompareStrAt: TCompareANSIStrAt): Boolean;


implementation

(* global properties *)

function getLength(const AValue: ansistring): Word;
begin
  Result := System.Length(AValue);
  // Goal: Returns the length of the given string.
  // Objetivo: Regresa la longuitud de la cadena dada.
end;

(* global functions *)

function IsEmpty(const AValue: ansistring): Boolean;
begin
  Result := Length(AValue) = 0;
  // Goal: Returns if a string is empty.
  // Objetivo: Regresa si una cadena esta vacia.
end;

function SameText(const A, B: ansistring): Boolean;
begin
  Result := SysUtils.SameText(A, B);
  // Goal: Returns if 2 strings are equal, ignores sensitive case.
  // Objetivo: Regresa si 2 cadenas son iguales, ignorar caso sensitivo.
end;

function IsStringOfChar(const S: ansistring; C: ansichar): Boolean;
var I, L: Integer; Match: Boolean;
begin
  L := uktstrings.getLength(S);

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

function Length(const AValue: ansistring): Word;
begin
  Result := System.Length(AValue);
  // Goal: Returns the length of the given string.
  // Objetivo: Regresa la longuitud de la cadena dada.
end;

function UppercaseCopy(const AValue: ansistring): ansistring;
begin
  Result := SysUtils.ANSIUpperCase(AValue);
  // Goal: Returns a uppercase copy of the given string.
  // Objetivo: Regresa una copia en mayusculas de la cadena dada.
end;

function LowercaseCopy(const AValue: ansistring): ansistring;
begin
  Result := SysUtils.ANSILowerCase(AValue);
  // Goal: Returns a lowercase copy of the given string.
  // Objetivo: Regresa una copia en minusculas de la cadena dada.
end;

function TogglecaseCopy(const AValue: ansistring): ansistring;
var I, Last: Integer; C: ansichar;
begin
  Result := '';
  Last := System.Length(AValue);
  for I := 1 to Last do
  begin
    C := AValue[i];
    if (Windows.IsCharLowerA(C))
      then CharUpperBuff(@C, 1)
      else CharLowerBuff(@C, 1);
    Result := Result + C;
  end;
  // Goal: Swaps the sensitive case of each character in the given string.
  // Objetivo: Cambia el caso sensitivo de cada caracter en la cadena dada.
end;

function CapitalizeCopy(const AValue: ansistring): ansistring;
var I, Last: Integer; C: ansichar; MakeUppercase: Boolean;
begin
  Result := '';
  MakeUppercase := TRUE;
  // cambiar a la primera letra en mayusculas
  // change first letter into uppercase

  Last := System.Length(AValue);
  for I := 1 to Last do
  begin
    C := AValue[i];
    if (C <> #32) then
    begin
      if (MakeUppercase) then
      begin
        CharUpperBuff(@C, 1);
        MakeUppercase := FALSE;
      end else CharLowerBuff(@C, 1);
    end else MakeUppercase := TRUE;
    Result := Result + C;
  end;
  // Goal: Returns a copy with uppercase initials of the given string.
  // Objetivo: Regresa una copia con iniciales en mayusculas de la cadena dada.
end;

function ConcatCopy(const A: ansistring; const B: ansichar): ansistring;
begin
  Result := A + B;
  // Objetivo: Agregar un caracter al final de la cadena dada.
  // Goal: Add a character at the end of the given string.
end;

function ConcatCopy(var A, B: ansistring): ansistring;
begin
  Result := A + B;
  // Objetivo: Agregar una cadena al final de la cadena dada.
  // Goal: Add a string at the end of the given string.
end;

function StringOfChar(const AValue: ansichar; const ACount: Byte): ansistring;
begin
  Empty(Result);
  // Result := '';

  setLength(Result, ACount);
  System.FillChar(Result[1], ACount, AValue);
  // Goal: Returns a string of the same character.
  // Objetivo: Regresa una cadena del mismo caracter.
end;

function CAR(const AValue: ansistring): ansistring;
begin
  Result := AValue[1];
  // Objetivo: Regresa el primer caracter de una cadena (como en LISP).
  // Goal: Returns the first character of a string (as in LISP).
end;

function CDR(const AValue: ansistring): ansistring;
begin
  Result := System.Copy(AValue, 2, Pred(System.Length(AValue)));
  // Objetivo: Regresa el resto de una cadena (como en LISP).
  // Goal: Returns the rest of the string (as in LISP).
end;

function CharPos(const SubStr: ansichar; const S: ansistring): Word;
var Len: Word; Found: Boolean;
begin
  Len := Pred(Length(S));
  Result := 0; Found := FALSE;

  while ((not Found) and (Result < Len)) do
  begin
    Found := (S[Result] = SubStr);
    Inc(Result);
  end;

  if (Found)
    then Dec(Result)
    else Result := 0;
  // Objetivo: Regresa la primera posicion del caracter dado,
  // desde el inicio de la cadena.

  // Goal: Returns the first location of the given character,
  // from the start of the string.
end;

function CharPosReverse(const SubStr: ansichar; const S: ansistring): Word;
var Found: Boolean;
begin
  Result := Length(S);
  Found  := FALSE;

  while ((not Found) and (Result > 0)) do
  begin
    Found := (S[Result] = SubStr);
    Dec(Result);
  end;

  if (Found)
    then Inc(Result)
    else Result := 0;
  // Objetivo: Regresa la primera posicion del caracter dado,
  // desde el final de la cadena.

  // Goal: Returns the first location of the given character,
  // from the finish of the string.
end;

function CharSetPos
  (const Items: ansicharset; const S: ansistring): Word;
var AIndex, Len: Word; Found: Boolean;
begin
  Found := FALSE;
  Len   := System.Length(S);
  AIndex := 1;

  while ((AIndex <= Len) and (not Found)) do
  begin
    Found := uktAnsicharsets.IsMember(S[AIndex], Items);
    Inc(AIndex);
  end;

  if (Found)
    then Result := Pred(AIndex)
    else Result := 0;
  // Objetivo: Regresa la primera posicion del caracter dado,
  // desde el inicio de la cadena}

  // Goal: Returns the first location of the given character,
  // from the start of the string.
end;

function CharSetPosReverse
  (const Items: ansicharset; const S: ansistring): Word;
var AIndex: Word; Found: Boolean;
begin
  Found := FALSE;
  AIndex := System.Length(S);

  while ((AIndex > 0) and (not Found)) do
  begin
    Found := uktAnsicharsets.IsMember(S[AIndex], Items);
    Dec(AIndex);
  end;

  if (Found)
    then Result := Succ(AIndex)
    else Result := 0;
  // Objetivo: Regresa la primera posicion del caracter dado,
  // desde el inicio de la cadena}

  // Goal: Returns the first location of the given character,
  // from the start of the string.
end;

function LeadPos
  (const SubStr: ansistring; const S: ansistring): Boolean;
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
  // Objetivo: Regresa si una subcadena es igual o esta al inicio de }
  { otra cadena.}

  // Goal: Returns if a substring is equal o is the start of another string.
end;

function LeadPosSame
  (const SubStr: ansistring; const S: ansistring): Boolean;
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
    Result  := ANSISameText(SubStr, LeadStr);
  end;
  // Objetivo: Regresa si una subcadena es igual o esta al inicio de }
  { otra cadena.}

  // Goal: Returns if a substring is equal o is the start of another string.
end;

function StartsWith(const SubStr: string; const S: string): Boolean;
begin
  Result := LeadPosSame(SubStr, S);
end;

function FinishesWith(const SubStr: string; const S: string): Boolean;
begin
  // @to-do:
  Result := false;
end;

function PosForward
  (const SubStr, FullStr: ansistring): Word;
var Len, AIndex: Integer; Found: Boolean;
begin
  Len := System.Length(FullStr);
  // obtain length of full string
  // obtener longuitud de cadena completa

  AIndex := 1;
  // "AIndex" indicates from which character in full string starts analysis
  // "AIndex" indica de cual caracter eb la cadena completa comienza el analisis

  Found := FALSE;
  while ((not Found) and (AIndex < Len)) do
  begin
    Found := LeadPos(SubStr, System.Copy(FullStr, AIndex, Len));

    Inc(AIndex);
  end;

  if (Found)
    then Result := Pred(AIndex)
    else Result := 0;
  // Objetivo: Regresa el indice de la primer ocurrencia de "Substr".
  // Goal: Returns the AIndex of the first ocurrence of "Substr".
end;

function PosSameForward
  (const SubStr: ansistring; const S: ansistring): Integer;
var Len, AIndex: Integer; Found: Boolean;
begin
  Len := System.Length(S);
  // obtain length of full string
  // obtener longuitud de cadena completa

  AIndex := 1;
  // "AIndex" indicates from which character in full string starts analysis
  // "AIndex" indica de cual caracter eb la cadena completa comienza el analisis

  Found := FALSE;
  while ((not Found) and (AIndex < Len)) do
  begin
    Found := LeadPosSame(SubStr, System.Copy(S, AIndex, Len));

    Inc(AIndex);
  end;

  if (Found)
    then Result := Pred(AIndex)
    else Result := 0;
  // Objetivo: Regresa el indice de la primer ocurrencia de "Substr".
  // Goal: Returns the AIndex of the first ocurrence of "Substr".
end;

function PosBackward
  (const SubStr, FullStr: ansistring): Word;
var Len, AIndex: Integer; Found: Boolean;
begin
  Len := System.Length(FullStr);
  // obtain length of full string
  // obtener longuitud de cadena completa

  AIndex := Len;
  // "AIndex" indicates from which character in full string starts analysis
  // "AIndex" indica de cual caracter eb la cadena completa comienza el analisis

  Found := FALSE;
  while ((not Found) and (AIndex > 0)) do
  begin
    Found := LeadPos(SubStr, System.Copy(FullStr, AIndex, Len));

    Dec(AIndex);
  end;

  if (Found)
    then Result := Succ(AIndex)
    else Result := 0;
  // Objetivo: Regresa el indice de la primer ocurrencia de "Substr".
  // Goal: Returns the AIndex of the first ocurrence of "Substr".
end;

function PosSameBackward
  (const SubStr, FullStr: ansistring): Word;
var Len, AIndex: Integer; Found: Boolean;
begin
  Len := System.Length(FullStr);
  // obtain length of full string
  // obtener longuitud de cadena completa

  AIndex := Len;
  // "AIndex" indicates from which character in full string starts analysis
  // "AIndex" indica de cual caracter eb la cadena completa comienza el analisis

  Found := FALSE;
  while ((not Found) and (AIndex > 0)) do
  begin
    Found := LeadPosSame(SubStr, System.Copy(FullStr, AIndex, Len));

    Dec(AIndex);
  end;

  if (Found)
    then Result := Succ(AIndex)
    else Result := 0;
  // Objetivo: Regresa el indice de la primer ocurrencia de "Substr".
  // Goal: Returns the AIndex of the first ocurrence of "Substr".
end;

function LeftCopy(const AValue: ansistring; const ACount: Byte): ansistring;
begin
  MoveLeft(Result, AValue, ACount);
  // Goal: Returns the leftmost characters of "AValue".
  // Objetivo: Regresa los caracteres mas a la izquierda de "AValue".
end;

function RightCopy(const AValue: ansistring; const ACount: Byte): ansistring;
begin
  MoveRight(Result, AValue, ACount);
  // Goal: Returns the rightmost characters of "AValue".
  // Objetivo: Regresa los caracteres mas a la derecha de "AValue".
end;

function TryStrToChar(const AValue: ansistring; var Dest: ansichar): Boolean;
begin
  Result := (Length(AValue) > 0);
  if (Result)
    then Dest := AValue[1];
  // Goal: To cast a string in to a character.
  // Objetivo: Convertir una cadena en un caracter.
end;

function StrToCharDef(const AValue: ansistring; const DefAValue: ansichar): ansichar;
begin
  if (not TryStrToChar(AValue, Result))
    then Result := DefAValue;
  // Goal: To cast a string in to a character.
  // Objetivo: Convertir una cadena en un caracter.
end;

function StrToChar(const AValue: ansistring): ansichar;
begin
  if (not TryStrToChar(AValue, Result))
    then EInvalidCast.Create(SInvalidCast);
  // Goal: To cast a string in to a character.
  // Objetivo: Convertir una cadena en un caracter.
end;

function CppEscToStr(const AValue: ansistring): ansistring;
begin
  Result := ReplaceForwardALLCopy(AValue, '\n', #13#10);
  // replace escape characters by line break characters
  // reemplazar caracteres de escape por caracteres de salto de linea

  Result := ReplaceForwardALLCopy(Result, '\'#39, #39);
  // replace escape characters by single quote characters
  // reemplazar caracteres de escape por caracteres de comilla simple

  Result := ReplaceForwardALLCopy(Result, '\'#34, #34);
  // replace escape characters by double quote characters
  // reemplazar caracteres de escape por caracteres de comilla doble

  Result := ReplaceForwardALLCopy(Result, '\\', '\');
  // replace escape characters by slash characters
  // reemplazar caracteres de escape por caracteres de diagonal

  // Objetivo: Convertir una cadena con caracteres de escape que comienzan con
  // el caracter "\" y otro caracter a una cadena sin caracteres de escape.

  // Goal: To convert a string with escape characters & another character into
  // a string without escape characters.
end;

function StrToCppEsc(const AValue: ansistring): ansistring;
begin
  Result := ReplaceForwardALLCopy(AValue, #13#10, '\n');
  // replace line break characters by escape characters
  // reemplazar caracteres de salto de linea por caracteres de escape

  Result := ReplaceForwardALLCopy(Result, #39, '$$');
  Result := ReplaceForwardALLCopy(Result, '$$', '\'#39);
  // replace single quote characters by escape characters
  // reemplazar caracteres de comilla simple por caracteres de escape

  Result := ReplaceForwardALLCopy(Result, #34, '$$');
  Result := ReplaceForwardALLCopy(Result, '$$', '\'#34);
  // replace double quote characters by escape characters
  // reemplazar caracteres de comilla doble por caracteres de escape

  Result := ReplaceForwardALLCopy(Result, '\', '$$');
  Result := ReplaceForwardALLCopy(Result, '$$', '\\');
  // replace double quote characters by escape characters
  // reemplazar caracteres de comilla doble por caracteres de escape

  // Objetivo: Convertir una cadena sin caracteres de escape a cadena con
  // caracteres de escape que comienzan con el caracter "\" y otro caracter.

  // Goal: To convert a string without escape characters into
  // a string with escape characters & another character.
end;

{ global procedures }

procedure Move
  (var Dest: ansistring; const Source: ansistring; AIndex, ACount: Integer);
begin
  uktstrings.Empty(Dest);
  System.setLength(Dest, ACount);
  System.Move(Source[AIndex], Dest[1], ACount);
  // Objetivo: Copiar el contenido de una cadena a un nueva cadena.
  // Goal: Copy the contents of a ansistring into a new string.
end;

procedure MoveLeft
  (var Dest: ansistring; const Source: ansistring; const ACount: Integer);
begin
  uktstrings.Move(Dest, Source, 1, ACount);
  // Goal: Returns the leftmost characters of "Source".
  // Objetivo: Regresa los caracteres mas a la izquierda de "Source".
end;

procedure MoveRight
  (var Dest: ansistring; const Source: ansistring; const ACount: Integer);
var AIndex: Integer;
begin
  AIndex := Succ(getLength(Source)-ACount);
  uktstrings.Move(Dest, Source, AIndex, ACount);
  // Goal: Returns the rightmost characters of "Source".
  // Objetivo: Regresa los caracteres mas a la derecha de "Source".
end;

procedure UppercaseReplace(var AValue: ansistring);
begin
  AValue := UpperCaseCopy(AValue);
  // Goal: Changes the given string into uppercase.
  // Objetivo: Cambia la cadena dada a mayusculas.
end;

procedure LowercaseReplace(var AValue: ansistring);
begin
  AValue := LowerCaseCopy(AValue);
  // Goal: Changes the given string into lowercase.
  // Objetivo: Cambia la cadena dada a minusculas.
end;

procedure TogglecaseReplace(var AValue: ansistring);
begin
  AValue := TogglecaseCopy(AValue);
  // Goal: Changes the given string into Togglecase.
  // Objetivo: Cambia la cadena dada a minusculas.
end;

procedure CapitalizeReplace(var AValue: ansistring);
begin
  AValue := CapitalizeCopy(AValue);
  // Goal: Changes the given string into capitalize.
  // Objetivo: Cambia la cadena dada a minusculas.
end;

procedure Empty(var AValue: ansistring);
begin
  AValue := EmptyStr;
  // Goal: Clear a ansi ansistring.
  // Objetivo: Limpia una cadena ansi.
end;

procedure Concat(var Dest: ansistring; const Source: ansichar);
begin
  Dest := Dest + Source;
  // Objetivo: Agregar un caracter al final de la cadena dada.
  // Goal: Add a character at the end of the given string.
end;

procedure Concat(var Dest: ansistring; const Source: ansistring);
begin
  Dest := Dest + Source;
  // Objetivo: Agregar una cadena al final de la cadena dada.
  // Goal: Add a string at the end of the given string.
end;

function ReplaceForwardCopy
  (const AValue, Source, Dest: ansistring): ansistring;
var AIndex: Integer;
begin
  Result := AValue;
  ReplaceForwardPosCopy(Result, AIndex, Source, Dest);
  // Goal: Returns a copy of the string with the given substring replaced.
  // Objetivo: Regresa una copia de la cadena con la subcadena reemplazada.
end;

function ReplaceForwardCopySame
  (const AValue, Source, Dest: ansistring): ansistring;
var AIndex: Integer;
begin
  Result := AValue;
  ReplaceForwardPosCopySame(Result, AIndex, Source, Dest);
  // Goal: Returns a copy of the string with the given substring replaced.
  // Objetivo: Regresa una copia de la cadena con la subcadena reemplazada.
end;

function ReplaceBackwardCopy
  (const AValue, Source, Dest: ansistring): ansistring;
var AIndex: Integer;
begin
  Result := AValue;
  ReplaceBackwardPosCopy(Result, AIndex, Source, Dest);
  // Goal: Returns a copy of the string with the given substring replaced.
  // Objetivo: Regresa una copia de la cadena con la subcadena reemplazada.
end;

function ReplaceBackwardCopySame
  (const AValue, Source, Dest: ansistring): ansistring;
var AIndex: Integer;
begin
  Result := AValue;
  ReplaceBackwardPosCopySame(Result, AIndex, Source, Dest);
  // Goal: Returns a copy of the string with the given substring replaced.
  // Objetivo: Regresa una copia de la cadena con la subcadena reemplazada.
end;

function ReplaceForwardALLCopy
  (const AValue, Source, Dest: ansistring): ansistring;
var AIndex: Integer;
begin
  Result := AValue;
  repeat
    ReplaceForwardPosCopy(Result, AIndex, Source, Dest);
  until (AIndex = 0);
  // Goal: Returns a copy of the string with the given substring replaced.
  // Objetivo: Regresa una copia de la cadena con la subcadena reemplazada.
end;

function ReplaceBackwardALLCopy
  (const AValue, Source, Dest: ansistring): ansistring;
var AIndex: Integer;
begin
  Result := AValue;
  repeat
    ReplaceBackwardPosCopy(Result, AIndex, Source, Dest);
  until (AIndex = 0);
  // Goal: Returns a copy of the string with the given substring replaced.
  // Objetivo: Regresa una copia de la cadena con la subcadena reemplazada.
end;

procedure ReplaceForwardPosCopy
  (var AValue: ansistring;
   var AIndex: Integer; const Source, Dest: ansistring);
var BeforeACount, AfterACount, S, V: Integer;
    BeforeString, AfterString: ansistring;
begin
  AIndex := PosForward(Source, AValue);
  // localizar en que posicion inicia la cadena que se va a reemplazar
  // locate, which location starts the string to be replaced

  if (AIndex > 0) then
  begin
    S := getLength(Source);
    V := getLength(AValue);

    AfterACount := (V - Pred(AIndex + S));
    // obtener la long. de la cadena despues de la parte que se va a reemplazar
    // obtain the length of the string after the section to be replaced

    BeforeACount := Pred(AIndex);
    // obtener la long. de la cadena antes de la parte que se va a reemplazar
    // obtain the length of the string before the section to be replaced

    if (AIndex <> 0) then
    begin
      uktstrings.MoveLeft(BeforeString, AValue, BeforeACount);
      uktstrings.MoveRight(AfterString, AValue, AfterACount);
      // extraer secciones antes y despues de cadena nodeseada
      // extract before & after section of unwanted string

      uktstrings.Empty(AValue);
      uktstrings.Concat(AValue, BeforeString);
      uktstrings.Concat(AValue, Dest);
      uktstrings.Concat(AValue, AfterString);
    end;
  end;
  // Goal: Replaces the first "Source" found, "AValue" with "Dest".
  // Objetivo: Reemplaza la primera "Source" encontrada en "AValue" por "Dest".}
end;

procedure ReplaceForwardPosCopySame
  (var AValue: ansistring;
   var AIndex: Integer; const Source, Dest: ansistring);
var BeforeACount, AfterACount, S, V: Integer;
    BeforeString, AfterString: ansistring;
begin
  BeforeString := '';
  AfterString  := '';
  AIndex := PosSameForward(Source, AValue);
  // localizar en que posicion inicia la cadena que se va a reemplazar
  // locate, which location starts the string to be replaced

  if (AIndex > 0) then
  begin
    S := getLength(Source);
    V := getLength(AValue);

    AfterACount := (V - Pred(AIndex + S));
    // obtener la long. de la cadena despues de la parte que se va a reemplazar
    // obtain the length of the string after the section to be replaced

    BeforeACount := Pred(AIndex);
    // obtener la long. de la cadena antes de la parte que se va a reemplazar
    // obtain the length of the string before the section to be replaced

    if (AIndex <> 0) then
    begin
      uktstrings.MoveLeft(BeforeString, AValue, BeforeACount);
      uktstrings.MoveRight(AfterString, AValue, AfterACount);
      // extraer secciones antes y despues de cadena nodeseada
      // extract before & after section of unwanted ansistring

      uktstrings.Empty(AValue);
      uktstrings.Concat(AValue, BeforeString);
      uktstrings.Concat(AValue, Dest);
      uktstrings.Concat(AValue, AfterString);
    end;
  end;
  // Goal: Replaces the first "Source" found, "AValue" with "Dest".
  // Objetivo: Reemplaza la primera "Source" encontrada en "AValue" por "Dest".}
end;

procedure ReplaceBackwardPosCopy
  (var AValue: ansistring;
   var AIndex: Integer; const Source, Dest: ansistring);
var BeforeACount, AfterACount, S, V: Integer;
    BeforeString, AfterString: ansistring;
begin
  AIndex := PosBackward(Source, AValue);
  // localizar en que posicion inicia la cadena que se va a reemplazar
  // locate, which location starts the string to be replaced

  if (AIndex > 0) then
  begin
    S := getLength(Source);
    V := getLength(AValue);

    AfterACount := (V - Pred(AIndex + S));
    // obtener la long. de la cadena despues de la parte que se va a reemplazar
    // obtain the length of the string after the section to be replaced

    BeforeACount := Pred(AIndex);
    // obtener la long. de la cadena antes de la parte que se va a reemplazar
    // obtain the length of the string before the section to be replaced

    if (AIndex <> 0) then
    begin
      uktstrings.MoveLeft(BeforeString, AValue, BeforeACount);
      uktstrings.MoveRight(AfterString, AValue, AfterACount);
      // extraer secciones antes y despues de cadena nodeseada
      // extract before & after section of unwanted ansistring

      uktstrings.Empty(AValue);
      uktstrings.Concat(AValue, BeforeString);
      uktstrings.Concat(AValue, Dest);
      uktstrings.Concat(AValue, AfterString);
    end;
  end;
  // Goal: Replaces the first "Source" found, "AValue" with "Dest".
  // Objetivo: Reemplaza la primera "Source" encontrada en "AValue" por "Dest".}
end;

procedure ReplaceBackwardPosCopySame
  (var AValue: ansistring;
   var AIndex: Integer; const Source, Dest: ansistring);
var BeforeACount, AfterACount, S, V: Integer;
    BeforeString, AfterString: ansistring;
begin
  BeforeString := '';
  AfterString  := '';
  AIndex := PosSameBackward(Source, AValue);
  // localizar en que posicion inicia la cadena que se va a reemplazar
  // locate, which location starts the string to be replaced

  if (AIndex > 0) then
  begin
    S := getLength(Source);
    V := getLength(AValue);

    AfterACount := (V - Pred(AIndex + S));
    // obtener la long. de la cadena despues de la parte que se va a reemplazar
    // obtain the length of the string after the section to be replaced

    BeforeACount := Pred(AIndex);
    // obtener la long. de la cadena antes de la parte que se va a reemplazar
    // obtain the length of the string before the section to be replaced

    if (AIndex <> 0) then
    begin
      uktstrings.MoveLeft(BeforeString, AValue, BeforeACount);
      uktstrings.MoveRight(AfterString, AValue, AfterACount);
      // extraer secciones antes y despues de cadena nodeseada
      // extract before & after section of unwanted ansistring

      uktstrings.Empty(AValue);
      uktstrings.Concat(AValue, BeforeString);
      uktstrings.Concat(AValue, Dest);
      uktstrings.Concat(AValue, AfterString);
    end;
  end;
  // Goal: Replaces the first "Source" found, "AValue" with "Dest".
  // Objetivo: Reemplaza la primera "Source" encontrada en "AValue" por "Dest".}
end;

function DeleteForwardCopy(const AValue, Source: ansistring): ansistring;
begin
  Result := ReplaceForwardCopy(AValue, Source, '');
  // Goal: Returns a copy of the string with the given substring deleted.
  // Objetivo: Regresa una copia de la cadena con la subcadena eliminada.
end;

function DeleteBackwardCopy(const AValue, Source: ansistring): ansistring;
begin
  Result := ReplaceBackwardCopy(AValue, Source, '');
  // Goal: Returns a copy of the string with the given substring deleted.
  // Objetivo: Regresa una copia de la cadena con la subcadena eliminada.
end;

function DeleteALLCopy(const AValue, Source: ansistring): ansistring;
begin
  Result := ReplaceForwardALLCopy(AValue, Source, '');
  // Goal: Returns a copy of the string with the given substring deleted.
  // Objetivo: Regresa una copia de la cadena con la subcadena eliminada.
end;

function SkipChar
  (const AValue: ansistring; AIndex: Integer; ValidChar: ansichar): Integer;
var L: Integer;
begin
  Result := AIndex; L := Length(AValue);
  if (Result <= L) and (AValue[Result] = ValidChar)
    then Inc(Result);
  // Goal: Returns a single character.
  // Objetivo: Regresa un solo caracter.
end;

function SkipChars
  (const AValue: ansistring; AIndex: Integer; ValidChars: ansicharset): Integer;
var L: Integer;
begin
  Result := AIndex; L := Length(AValue);
  if (Result <= L) and IsMember(AValue[Result], ValidChars)
    then Inc(Result);
  // Goal: Returns a single character.
  // Objetivo: Regresa un solo caracter.
end;

function SkipCharWhile
  (const AValue: ansistring; AIndex: Integer; ValidChar: ansichar): Integer;
var L: Integer;
begin
  Result := AIndex; L := Length(AValue);
  while ((Result <= L) and (AValue[Result] = ValidChar)) do
   Inc(Result);
  // Goal: Returns a group of characters.
  // Objetivo: Regresa un grupo de caracteres.
end;

function SkipCharUntil
  (const AValue: ansistring; AIndex: Integer; BreakChar: ansichar): Integer;
var L: Integer;
begin
  Result := AIndex; L := Length(AValue);
  while ((Result <= L) and (AValue[Result] <> BreakChar)) do
   Inc(Result);
  // Goal: Returns a group of characters.
  // Objetivo: Regresa un grupo de caracteres.
end;

function SkipWhile
  (const AValue: ansistring; AIndex: Integer; ValidChars: ansicharset): Integer;
var L: Integer;
begin
  Result := AIndex; L := Length(AValue);
  while ((Result <= L) and IsMember(AValue[Result], ValidChars)) do
   Inc(Result);
  // Goal: Returns a group of non-space characters.
  // Objetivo: Regresa un grupo de caracteres que no son espacios.
end;

function SkipUntil
  (const AValue: ansistring; AIndex: Integer; BreakChars: ansicharset): Integer;
var L: Integer;
begin
  Result := AIndex; L := Length(AValue);
  while ((Result <= L) and not IsMember(AValue[Result], BreakChars)) do
   Inc(Result);
  // Goal: Returns a group of non-space characters.
  // Objetivo: Regresa un grupo de caracteres que no son espacios.
end;

function SkipToken(const AValue: ansistring; var S, F: Integer): ansistring;
var StartAIndex, FinishAIndex: Integer;
begin
  StartAIndex  := S;

  StartAIndex  := SkipCharWhile(AValue, StartAIndex, #32);
  FinishAIndex := SkipCharUntil(AValue, StartAIndex, #32);
  Result      := CopyFrom(AValue, StartAIndex, FinishAIndex);

  S := StartAIndex;
  F := FinishAIndex;
end;

function SkipDigit(const AValue: ansistring; var I, F: Integer): ansistring;
begin
  I := SkipCharWhile(AValue, I, #32);
  F := SkipWhile(AValue, I, DigitSet);
  Result := CopyFrom(AValue, I, F);
  I := F;
end;

function SkipLetter(const AValue: ansistring; var I, F: Integer): ansistring;
begin
  I := SkipCharWhile(AValue, I, #32);
  F := SkipWhile(AValue, I, AlphaSet);
  Result := CopyFrom(AValue, I, F);
  I := F;
end;

function CopyFrom
  (const Source: ansistring; Start, Finish: Integer): string;
var I, ACount: Integer;
begin
  //uktstrings.Empty(Result);
  Result := '';

  ACount := Succ(Finish - Start);

  for I := 1 to ACount do
     Result := Result + Source[1];
  // Goal: Returns a substring from "start" to "finish".
  // Objetivo: Regresa una subcadena desde "start" hasta "finish".
end;

function ParseFrom
  (const Source: ansistring; var Start, Finish: Integer): ansistring;
var I, ACount: Integer;
begin
  ACount := (Finish - Start);

  Result := '';
  for I := 1 to ACount do
     Result := Result + Source[(Start + i - 1)];

  Start := Finish;
  // Goal: Returns a substring from "start" to "finish" & update "start".
  // Objetivo: Regresa una subcadena desde "start" hasta "finish"
  // y actualiza "start".
end;

(* global operators *)

function Add(const A: ansistring; const B: ansichar): ansistring;
begin
  Result := ConcatCopy(A, B);
  // Objetivo: Agregar un caracter al final de la cadena dada.
  // Goal: Add a character at the end of the given string.
end;

function Add(var A, B: ansistring): ansistring;
begin
  Result := ConcatCopy(A, B);
  // Objetivo: Agregar una cadena al final de la cadena dada.
  // Goal: Add a string at the end of the given string.
end;

procedure Assign
  (var Dest: ansistring; const Source: ansistring);
var ACount: Integer;
begin
  ACount := System.Length(Source);
  System.SetLength(Dest, ACount);
  System.Move(Source[1], Dest[1], ACount);
end;

procedure Assign
  (var Dest: ansistring; const Source: ansichar);
begin
  System.SetLength(Dest, 1);
  Dest[1] := Source;
end;

function SameStrAt
  (const SubStr, Str: ansistring; var AIndex: Word): Boolean;
var Match: Boolean; Last1, Last2, I, J: Integer;
begin
  Last1 := System.Length(Str);
  Last2 := System.Length(SubStr);
  I := 1;
  J := AIndex;

  Match := Last2 <= Succ(Last1 - AIndex);
//  Match := Succ(Last1 - AIndex) >= Last2;
  // verificar que las subcadena sea de menor longuitud que la cadena destino

  if (Match) then
  repeat
    Match := uktansichars.SameText(SubStr[i], Str[j]);

    Inc(I);
    Inc(J);
  until (not Match or (I > Last2));
  Result := Match;
  // Objetivo: Buscar una subcadena en otra cadena,
  // y que inicie a partir de la posicion indicada,
  // ignorando el caso sensitivo.
end;

function EqualStrAt
  (const SubStr, Str: ansistring; var AIndex: Word): Boolean;
var Match: Boolean; Last1, Last2, I, J: Integer;
begin
  Last1 := System.Length(Str);
  Last2 := System.Length(SubStr);
  I := 1;
  J := AIndex;

  Match := Last2 <= Succ(Last1 - AIndex);
//  Match := Succ(Last1 - AIndex) >= Last2;
  // verificar que las subcadena sea de menor longuitud que la cadena destino

  if (Match) then
  repeat
    Match := (SubStr[i] = Str[j]);

    Inc(I);
    Inc(J);
  until (not Match or (I > Last2));
  Result := Match;
  // Objetivo: Buscar una subcadena en otra cadena,
  // y que inicie a partir de la posicion indicada,
  // tomando en cuenta el caso sensitivo.
end;

function SameStrIn
  (const SubStr, Str: ansistring; var AIndex: Word): Boolean;
begin
  AIndex := 1;
  {$ifdef Delphi}
  Result := Search(SubStr, Str, AIndex, (*@*)SameStrAt);
  {$else}
  Result := Search(SubStr, Str, AIndex, @SameStrAt);
  {$endif}
  // Objetivo: Buscar una subcadena en otra cadena,
  // sin importar la posicion,
  // ignorando el caso sensitivo.
end;

function EqualStrIn
  (const SubStr, Str: ansistring; var AIndex: Word): Boolean;
begin
  AIndex := 1;
  {$ifdef Delphi}
  Result := Search(SubStr, Str, AIndex, (*@*)EqualStrAt);
  {$else}
  Result := Search(SubStr, Str, AIndex, @EqualStrAt);
  {$endif}
  // Objetivo: Buscar una subcadena en otra cadena,
  // sin importar la posicion,
  // tomando en cuenta el caso sensitivo.
end;

function SameStr
  (const SubStr, Str: ansistring; var AIndex: Word): Boolean;
var Match: Boolean; Last1, Last2: Integer;
begin
  // @to-do: corregir "AIndex"
  AIndex := 0;

  Last1 := System.Length(Str);
  Last2 := System.Length(SubStr);

  Match := (Last1 = Last2);
  // verificar que las cadenas sean de la misma longuitud

  if (Match)
    then Match := uktstrings.SameText(SubStr, Str);
  Result := Match;
  // Objetivo: Buscar una subcadena en otra cadena,
  // que sean de la misma longuitud,
  // ignorando el caso sensitivo.
end;

function EqualStr
  (const SubStr, Str: ansistring; var AIndex: Word): Boolean;
var Match: Boolean; Last1, Last2: Integer;
begin
  // @to-do: corregir "AIndex"
  AIndex := 0;

  Last1 := System.Length(Str);
  Last2 := System.Length(SubStr);

  Match := (Last1 = Last2);
  // verificar que las cadenas sean de la misma longuitud

  if (Match)
    then Match := (SubStr = Str);
  Result := Match;
  // Objetivo: Buscar una subcadena en otra cadena,
  // que sean de la misma longuitud,
  // tomando en cuenta el caso sensitivo.
end;

function Search
  (const SubStr, Str: ansistring; var AIndex: Word;
   CompareStrAt: TCompareANSIStrAt): Boolean;
var Len: Word; Found: Boolean;
begin
  Len := System.Length(Str);
  Found  := FALSE;

  if (Assigned(CompareStrAt)) then
  while (not Found and (AIndex <= Len)) do
  begin
    if (CompareStrAt(SubStr, Str, AIndex))
      then Found := TRUE
      else Inc(AIndex);
  end;

  Result := Found;
  // Goal: Searches for a substring inside other string
  // beginning at the "AIndex" char and returns the next character.
end;


end.
