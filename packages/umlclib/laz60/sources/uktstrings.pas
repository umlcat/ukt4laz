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

unit uktstrings;

  // note: doesn't check if characters are ANSI or Unicode
  // nota: no he checado si los caracteres son ANSI o Unicode

interface
uses
{$IFDEF MSWINDOWS}
  Windows, //Messages,
{$ENDIF}
{$IFDEF LINUX}
  Types, Libc,
{$ENDIF}
  SysUtils, //Math,
  uktansichars, uktansicharsets, uktcomparisons,
  //ukttextconsts,
  uktansicharsetconsts,
  dummy;

(* global properties *)

  function getLength(const AValue: string): Word; overload;

  function QuotedUppercaseCopy
    (const AValue: string; const A, B: ansichar): string; overload;
  function QuotedLowercaseCopy
    (const AValue: string; const A, B: ansichar): string; overload;
  function QuotedTogglecaseCopy
    (const AValue: string; const A, B: ansichar): string; overload;
  function QuotedCapitalizeCopy
    (const AValue: string; const A, B: ansichar): string; overload;

  function TrimLeftCopy(const AValue: string): string; overload;
  function TrimRightCopy(const AValue: string): string; overload;
  function TrimCopy(const AValue: string): string; overload;

  function UnTrimLeftCopy
    (const AValue: string; ACount: Integer): string; overload;
  function UnTrimRightCopy
    (const AValue: string; ACount: Integer): string; overload;

  function TrimPrefixCopy
    (const FullStr: string; const prefix: string) : string;
  function TrimPosfixCopy
    (const FullStr: string; const posfix: string): string;
  function TrimDelimitersCopy
    (const FullStr: string;
     const prefix: string; const posfix: string): string;

  function PadPrefixCopy
    (const FullStr: string; const prefix: string) : string;
  function PadPosfixCopy
    (const FullStr: string; const posfix: string) : string;
  function PadDelimitersCopy
    (const FullStr: string;
     const prefix: string; const posfix: string): string;

  procedure setLength(var AValue: string; ACount: Byte); overload;

  procedure QuotedUppercase
    (var AValue: string; const A, B: ansichar); overload;
  procedure QuotedLowercase
    (var AValue: string; const A, B: ansichar); overload;
  procedure QuotedTogglecase
    (var AValue: string; const A, B: ansichar); overload;
  procedure QuotedCapitalize
    (var AValue: string; const A, B: ansichar); overload;

  procedure TrimLeft(var AValue: string); overload;
  procedure TrimRight(var AValue: string); overload;
  procedure Trim(var AValue: string); overload;

  procedure UnTrimLeft(var AValue: string; ACount: Integer); overload;
  procedure UnTrimRight(var AValue: string; ACount: Integer); overload;

(* global functions *)

  function UppercaseCopy(const AValue: string): string; overload;
  function LowercaseCopy(const AValue: string): string; overload;
  function TogglecaseCopy(const AValue: string): string; overload;
  function CapitalizeCopy(const AValue: string): string; overload;

  function IsEmpty(const AValue: string): Boolean;
  function IsStringOfChar(const S: string; C: Char): Boolean;
  
  function StringOfChar(const AValue: ansichar; const ACount: Byte): string;

  function CAR(const AValue: string): string; overload;
  function CDR(const AValue: string): string; overload;

  function LeadPos
    (const SubStr: string; const FullStr: string): Boolean;
  function LeadPosSame
    (const SubStr: string; const FullStr: string): Boolean;

  function Pos
    (const SubStr: string; const S: string): Integer;
  function PosSame
    (const SubStr: string; const S: string): Integer;

  function StartsWith
    (const FullStr: string; const SubStr: string): Boolean;
  function FinishesWith
    (const FullStr: string; const SubStr: string): Boolean;

  function CharPos(const SubStr: char; const S: string): Word;
  function CharPosReverse(const SubStr: char; const S: string): Word;

  function CharSetPos
    (const Items: ansicharset; const S: string): Word;
  function CharSetPosReverse
    (const Items: ansicharset; const S: string): Word;

  function Left
    (const AValue: string; const ACount: Byte): string;
  function Right
    (const AValue: string; const ACount: Byte): string;

  function UnLeft
    (const AValue: string; const ACount: Byte): string;
  function UnRightCopy
    (const AValue: string; const ACount: Byte): string;

  function TryStrToChar
    (const AValue: string; var Dest: ansichar): Boolean;
  function StrToCharDef
    (const AValue: string; const DefAValue: ansichar): ansichar;
  function StrToChar
    (const AValue: string): ansichar;

  function PtrToStr(const AValue: pointer): string;
  function StrToPtr(const AValue: string): pointer;

  procedure UppercaseReplace(var AValue: string); overload;
  procedure LowercaseReplace(var AValue: string); overload;
  procedure TogglecaseReplace(var AValue: string); overload;
  procedure CapitalizeReplace(var AValue: string); overload;

  procedure Empty(var AValue: string);

  procedure Move
    (var Dest: string; const Source: string; AIndex, ACount: Integer);
  procedure MoveLeft
    (var Dest: string; const Source: string; const ACount: Integer);
  procedure MoveRight
    (var Dest: string; const Source: string; const ACount: Integer);

  procedure Concat
    (var Dest: string; const Source: ansichar); overload;
  procedure ConcatBack
    (var Dest: string; const Source: ansichar); overload;

  procedure Concat
    (var Dest: string; const Source: string); overload;
  procedure ConcatBack
    (var Dest: string; const Source: string); overload;

  function ConcatCopy
    (const A: string; B: ansichar): string; overload;
  function ConcatCopy
    (const A, B: string): string; overload;

  function MoveCopy
    (const AValue: string; AIndex, ACount: Byte): string; overload;
  function ReverseCopy
    (const AValue: string): string;

  function SubStr
    (const PartialStr, FullStr: string; var AIndex: Integer): Boolean;
  function IsIdentifier(const AValue: string): Boolean;
  function RangeToStr(const Min, Max: ansichar): string;

  function DeleteCharCopy
    (const AValue: string; A: ansichar): string; overload;
  function DeleteCharsCopy
  (const AValue: string; const Source: ansicharset): string; overload;

  function ReplaceCharByCharCopy
    (const AValue: string; A, B: ansichar): string;
  function ReplaceCharByStringCopy
    (const AValue: string; A: ansichar; B: string ): string;

  function ReplaceCharCopy
    (const AValue: string; A, B: ansichar): string; overload;
  function ReplaceCharCopy
    (const AValue: string; A: ansichar; B: string): string; overload;

  function ReplaceCharsCopy
  (const AValue: string;
   const Source: ansicharset; Dest: ansichar): string; overload;

  procedure ReplaceCharByChar
    (var AValue: string; A, B: ansichar);
  procedure ReplaceCharByString
    (var AValue: string; A: ansichar; B: string);

  procedure ReplaceChar
    (var AValue: string; A, B: ansichar); overload;
  procedure ReplaceChar
    (var AValue: string; A: ansichar; B: string); overload;

  procedure ReplaceChars
    (var AValue: string; const Source: ansicharset; Dest: ansichar); overload;

  function ReplaceStrCopy
    (const AValue: string; const Source, Dest: string): string;
  procedure ReplaceStr
    (var AValue: string; const Source, Dest: string);

  function ReplaceCopy
    (const AValue, Source, Dest: string): string;
  function ReplaceCopySame
    (const AValue, Source, Dest: string): string;

  function DeleteCopy
    (const AValue, Source: string): string;

  procedure ReplacePos
    (var AValue: string;
     var AIndex: Integer; const Source, Dest: string);
  procedure ReplacePosSame
    (var AValue: string;
     var AIndex: Integer; const Source, Dest: string);

  procedure Replace
    (var AValue: string; const Source, Dest: string);

  function ReplaceCharToStrCopy
    (const AValue: string;
     const Source: ansichar; const Dest: string): string;

  procedure ReplaceCharToStr
    (var AValue: string; const Source: ansichar; const Dest: string);

  function RemoveCharCopy
   (const AValue: string; IgnoreChar: ansichar): string;
  function RemoveCharsCopy
   (const AValue: string; IgnoreChars: ansicharset): string;

  procedure RemoveChar(var AValue: string; IgnoreChar: ansichar);
  procedure RemoveChars(var AValue: string; IgnoreChars: ansicharset);

type
  TCompareStrAt =
    (* ^ *)function (const SubStr, Str: string; var AIndex: Word): Boolean;

  function SameStrAt
    (const SubStr, Str: string; var AIndex: Word): Boolean;
  function EqualStrAt
    (const SubStr, Str: string; var AIndex: Word): Boolean;

  function SameStrIn
    (const SubStr, Str: string; var AIndex: Word): Boolean;
  function EqualStrIn
    (const SubStr, Str: string; var AIndex: Word): Boolean;

  function SameStr
    (const SubStr, Str: string; var AIndex: Word): Boolean;
  function EqualStr
    (const SubStr, Str: string; var AIndex: Word): Boolean;

  function Search
    (const SubStr, Str: string; var AIndex: Word;
     CompareStrAt: TCompareStrAt): Boolean;

  function ScanEqual
    (const Str: string;
     const SubStr: string; {copy} AIndex: Word): Word; overload;
  function ScanSame
    (const Str: string;
     const SubStr: string; {copy} AIndex: Word): Word; overload;

  function SkipChar
   (const AValue: string; AIndex: Integer; ValidChar: ansichar): Integer;
  function SkipChars
   (const AValue: string; AIndex: Integer; ValidChars: ansicharset): Integer;
  function SkipCharWhile
   (const AValue: string; AIndex: Integer; ValidChar: ansichar): Integer;
  function SkipCharUntil
   (const AValue: string; AIndex: Integer; BreakChar: ansichar): Integer;
  function SkipWhile
   (const AValue: string; AIndex: Integer; ValidChars: ansicharset): Integer;
  function SkipUntil
   (const AValue: string; AIndex: Integer; BreakChars: ansicharset): Integer;
  function SkipToken(const AValue: string; var S, F: Integer): string;
  function SkipDigit(const AValue: string; var I, F: Integer): string;
  function SkipLetter(const AValue: string; var I, F: Integer): string;

  function CopyFromTo(const Source: string; Start, Finish: Integer): string;
  function CopyFrom(const Source: string; AIndex: Integer): string;
  function ParseFrom(const Source: string; var Start, Finish: Integer): string;

  function SkipEmpty(const Start, Finish: Integer): Boolean;

  function ExtractCharUntil
   (const AValue: string; AIndex: Integer; BreakChar: ansichar): string;
  function ExtractCharUntilBack
   (const AValue: string; AIndex: Integer; BreakChar: ansichar): string;

  function ExtractWhile
   (const AValue: string; AIndex: Integer; SkipChars: ansicharset): string;
  function ExtractUntil
   (const AValue: string;
      var AIndex: Integer; BreakChars: ansicharset): string;
  function ExtractString(const AValue: string; AIndex: Integer): string;

  function LastChar(const Astring: string): ansichar;

  function AlignLeft
   (const Source: string; ACount: Integer; AValue: ansichar): string;
  function AlignRight
   (const Source: string; ACount: Integer; AValue: ansichar): string;

  function SameText(const A, B: string): Boolean;

  (* = *) function Equal(const A, B: string): Boolean;
  function Compare(const A, B: string): TComparison;

  function EqualByOptions
    (const A, B: string; Options: TStringOptions): Boolean;
  function CompareByOptions
    (const A, B: string; Options: TStringOptions): TComparison;

(* global random string functions *)

  function RandomAlphaNumString(const ACount: Integer): string;
  function RandomAlphaString(const ACount: Integer): string;
  function RandomNumString(const ACount: Integer): string;

implementation

(* global properties *)

function getLength(const AValue: string): Word;
begin
  Result := System.Length(AValue);
  // Goal: Returns the length of the given string.
  // Objetivo: Regresa la longuitud de la cadena dada.
end;

function QuotedUppercaseCopy
  (const AValue: string; const A, B: ansichar): string;
begin
  Result := AValue;
  uktstrings.QuotedUppercase(Result, A, B);
  // Goal: Returns a uppercase copy of the given string,
  // without modifying delimited substrings.

  // Objetivo: Regresa una copia en mayusculas de la cadena dada,
  // sin modificar a las subcadenas delimitadas.
end;

function QuotedLowercaseCopy
  (const AValue: string; const A, B: ansichar): string;
begin
  Result := AValue;
  uktstrings.QuotedLowercase(Result, A, B);
  // Goal: Returns a lowercase copy of the given nullstring,
  // without modifying delimited substrings.

  // Objetivo: Regresa una copia en minusculas de la cadena terminada en nulo,
  // dada sin modificar a las subcadenas delimitadas.
end;

function QuotedTogglecaseCopy
  (const AValue: string; const A, B: ansichar): string;
begin
  Result := AValue;
  uktstrings.QuotedTogglecase(Result, A, B);
  // Goal: Returns a lowercase copy of the given nullstring,
  // without modifying delimited substrings.

  // Objetivo: Regresa una copia en minusculas de la cadena terminada en nulo,
  // dada sin modificar a las subcadenas delimitadas.
end;

function QuotedCapitalizeCopy
  (const AValue: string; const A, B: ansichar): string;
begin
  Result := AValue;
  uktstrings.QuotedCapitalize(Result, A, B);
  // Goal: Returns a lowercase copy of the given nullstring,
  // without modifying delimited substrings.

  // Objetivo: Regresa una copia en minusculas de la cadena terminada en nulo,
  // dada sin modificar a las subcadenas delimitadas.
end;

function TrimLeftCopy(const AValue: string): string;
begin
  Result := SysUtils.TrimLeft(AValue);
  // Goal: Returns a copy of the given string without leading spaces.
  // Objetivo: Regresa una copia de la cadena dada sin espacios iniciales.
end;

function TrimRightCopy(const AValue: string): string;
begin
  Result := SysUtils.TrimRight(AValue);
  // Goal: Returns a copy of the given string without trailing spaces.
  // Objetivo: Regresa una copia de la cadena dada sin espacios finales.
end;

function TrimCopy(const AValue: string): string;
begin
  Result := SysUtils.Trim(AValue);
  // Goal: Returns a copy of the given string }
  // without leading & trailing spaces.

  // Objetivo: Regresa una copia de la cadena dada
  // sin espacios iniciales y finales.
end;

function UnTrimLeftCopy(const AValue: string; ACount: Integer): string;
var L: Integer;
begin
  L := getLength(AValue);
  Result := StringOfChar(#32, ACount-L) + AValue;
  // Goal: Returns a copy of the given string plus leading spaces.
  // Objetivo: Regresa una copia de la cadena dada mas espacios iniciales.
end;

function UnTrimRightCopy(const AValue: string; ACount: Integer): string;
var L : Integer;
begin
  L := getLength(AValue);
  Result := AValue + StringOfChar(#32, ACount-L);
  // Goal: Returns a copy of the given string plus trailing spaces.
  // Objetivo: Regresa una copia de la cadena dada mas espacios finales.
end;

/// <summary>
/// If founds a string prefix in the given text,
/// returns a copy without the prefix,
/// otherwise, returns the full string.
/// </summary>
/// <param name="prefix">prefix substring</param>
/// <param name="str">Text with an optional prefix</param>
/// <returns>Text without a prefix</returns>
function TrimPrefixCopy
  (const FullStr: string; const prefix: string) : string;
var alen: Integer;
begin
  Result := '';

  if (StartsWith(prefix, FullStr)) then
  begin
    alen   := (Length(FullStr) - Length(prefix));
    Result := Right(FullStr, alen);
  end else
  begin
    Result := FullStr;
  end;
end;

/// <summary>
/// If founds a string prefix in the given text,
/// returns a copy without the posfix,
/// otherwise, returns the full string.
/// </summary>
/// <param name="prefix">prefix substring</param>
/// <param name="str">Text with an optional posfix</param>
/// <returns>Text without a posfix</returns>
function TrimPosfixCopy
  (const FullStr: string; const posfix: string): string;
var alen : Integer;
begin
  Result := '';

  if (FinishesWith(FullStr, posfix)) then
  begin
    alen   := (Length(FullStr) - 1);
    Result := Left(FullStr, alen);
  end else
  begin
    Result := FullStr;
  end;
end;

/// <summary>
/// Returns a copy of the <code>str</code> string,
/// where the given prefix &  posfix substrings, are removed.
/// </summary>
/// <param name="prefix"></param>
/// <param name="posfix"></param>
/// <param name="str"></param>
/// <returns></returns>
function TrimDelimitersCopy
  (const FullStr: string;
   const prefix: string; const posfix: string): string;
begin
  Result := '';
  Result := TrimPrefixCopy(FullStr, prefix);
  Result := TrimPosfixCopy(Result, posfix);
end;

function PadPrefixCopy
  (const FullStr: string; const prefix: string): string;
var alen: Integer;
begin
  Result := '';

  if (not StartsWith(prefix, FullStr)) then
  begin
    Result := (prefix + FullStr);
  end else
  begin
    Result := FullStr;
  end;
  // if a string doesn't have some prefix,
  // add its.
end;

function PadPosfixCopy
  (const FullStr: string; const posfix: string): string;
var alen : Integer;
begin
  Result := '';

  if (not FinishesWith(FullStr, posfix)) then
  begin
    Result := (FullStr + posfix);
  end else
  begin
    Result := FullStr;
  end;
  // if a string doesn't have some posfix,
  // add its.
end;

function PadDelimitersCopy
  (const FullStr: string;
   const prefix: string;
   const posfix: string): string;
begin
  Result := '';
  Result := PadPrefixCopy(FullStr, prefix);
  Result := PadPosfixCopy(Result, posfix);
end;

procedure setLength(var AValue: string; ACount: Byte);
begin
  System.SetLength(AValue, ACount);
end;

procedure QuotedUppercase(var AValue: string; const A, B: ansichar);
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

    if (not InsideString and IsCharLower(C))
      then CharLowerBuff(@C, 1);
    AValue[AIndex] := C;
    // replace characters
    // reemplazar caracteres
  end;
  // Goal: Changes the given string into uppercase,
  // without modifying delimited substrings.

  // Objetivo: Cambia la cadena dada a mayusculas.
  // sin modificar a las subcadenas delimitadas.
end;

procedure QuotedLowercase(var AValue: string; const A, B: ansichar);
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
      then CharUpperBuff(@C, 1);
    AValue[AIndex] := C;
    // replace characters
    // reemplazar caracteres
  end;
  // Goal: Changes the given string into lowercase,
  // without modifying delimited substrings.

  // Objetivo: Cambia la cadena dada a minusculas.
  // sin modificar a las subcadenas delimitadas.
end;

procedure QuotedTogglecase(var AValue: string; const A, B: ansichar);
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
  // Goal: Changes the given string into Togglecase,
  // without modifying delimited substrings.

  // Objetivo: Cambia la cadena dada a minusculas.
  // sin modificar a las subcadenas delimitadas.
end;

procedure QuotedCapitalize(var AValue: string; const A, B: ansichar);
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
  // Goal: Changes the given string into capitalize,
  // without modifying delimited substrings.

  // Objetivo: Cambia la cadena dada a minusculas.
  // sin modificar a las subcadenas delimitadas.
end;

procedure TrimLeft(var AValue: string);
begin
  AValue := SysUtils.TrimLeft(AValue);
  // Goal: Returns the given string without leading spaces.
  // Objetivo: Regresa la cadena dada sin espacios iniciales.
end;

procedure TrimRight(var AValue: string);
begin
  AValue := SysUtils.TrimRight(AValue);
  // Goal: Returns the given string without trailing spaces.
  // Objetivo: Regresa la cadena dada sin espacios finales.
end;

procedure Trim(var AValue: string);
begin
  AValue := SysUtils.Trim(AValue);
  // Goal: Returns the given string without leading & trailing spaces.
  // Objetivo: Regresa la cadena dada sin espacios iniciales y finales.
end;

procedure UnTrimLeft(var AValue: string; ACount: Integer);
var L: Integer;
begin
  L := Length(AValue);
  Concat(AValue, StringOfChar(#32, ACount-L));
  Concat(AValue, AValue);
  // Goal: Returns the given string plus leading spaces.
  // Objetivo: Regresa la cadena dada mas espacios iniciales.
end;

procedure UnTrimRight(var AValue: string; ACount: Integer);
var L: Integer;
begin
  L := Length(AValue);
  Concat(AValue, AValue);
  Concat(AValue, StringOfChar(#32, ACount-L));
  // Goal: Returns the given string plus trailing spaces.
  // Objetivo: Regresa la cadena dada mas espacios finales.
end;

{ global functions }

function UppercaseCopy(const AValue: string): string;
begin
  Result := SysUtils.ANSIUpperCase(AValue);
  // Goal: Returns a uppercase copy of the given string.
  // Objetivo: Regresa una copia en mayusculas de la cadena dada.
end;

function LowercaseCopy(const AValue: string): string;
begin
  Result := SysUtils.ANSILowerCase(AValue);
  // Goal: Returns a lowercase copy of the given string.
  // Objetivo: Regresa una copia en minusculas de la cadena dada.
end;

function TogglecaseCopy(const AValue: string): string;
var I, Last: Integer; C: char;
begin
  Result := '';
  Last := System.Length(AValue);
  for I := 1 to Last do
  begin
    C := AValue[i];
    if (IsCharLower(C))
      then CharUpperBuff(@C, 1)
      else CharLowerBuff(@C, 1);
    Result := Result + C;
  end;
  // Goal: Swaps the sensitive case of each character, the given ansiarray.
  // Objetivo: Cambia el caso sensitivo de cada caracter en la cadena.
end;

function CapitalizeCopy(const AValue: string): string;
var I, Last: Integer; C: char; MakeUppercase: Boolean;
begin
  Result := '';
  MakeUppercase := TRUE;
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
  // Goal: Returns a copy with uppercase initials of the given ansiarray.
  // Objetivo: Regresa una copia con iniciales en mayusculas de la cadena dada.
end;

function IsEmpty(const AValue: string): Boolean;
begin
  Result := getLength(AValue) = 0;
  // Goal: Returns if a string is empty.
  // Objetivo: Regresa si una cadena esta vacia.
end;

function IsStringOfChar(const S: string; C: Char): Boolean;
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

function StringOfChar(const AValue: ansichar; const ACount: Byte): string;
begin
  Empty(Result);
  setLength(Result, ACount);
  System.FillChar(Result[1], ACount, AValue);
  // Goal: Returns a string of the same ansichar.
  // Objetivo: Regresa una cadena del mismo caracter.
end;

function CAR(const AValue: string): string; overload;
begin
  Result := AValue[1];
  // Objetivo: Regresa el primer caracter de una cadena.
  // Goal: Returns the first character of a string.
end;

function CDR(const AValue: string): string; overload;
begin
  Result := System.Copy(AValue, 2, Pred(System.Length(AValue)));
  // Objetivo: Regresa el resto de una cadena.
  // Goal: Returns the rest of the string.
end;

function LeadPos(const SubStr: string; const FullStr: string): Boolean;
var Shorter, Len: Integer; LeadStr: string;
begin
  Shorter := System.Length(SubStr);
  // obtain length of substring
  // obtener longuitud de subcadena

  Len := System.Length(FullStr);
  // obtain length of full string
  // obtener longuitud de cadena completa

  Result := not (Shorter > Len);
  // substring must be shorter or equal size than full string
  // la subcadena debe ser mas corta o de igual tamaño que la cadena completa

  if (Result) then
  begin
    LeadStr := System.Copy(FullStr, 1, Shorter);
    Result  := (SubStr = LeadStr);
  end;
  // Objetivo: Regresa si una subcadena es igual o esta al inicio de
  // otra cadena.

  // Goal: Returns if a substring is equal o is the start of another string.
end;

function LeadPosSame(const SubStr: string; const FullStr: string): Boolean;
var Shorter, Len: Integer; LeadStr: string;
begin
  Shorter := System.Length(SubStr);
  // obtain length of substring
  // obtener longuitud de subcadena

  Len := System.Length(FullStr);
  // obtain length of full string
  // obtener longuitud de cadena completa

  Result := not (Shorter > Len);
  // substring must be shorter or equal size than full string
  // la subcadena debe ser mas corta o de igual tamaño que la cadena completa

  if (Result) then
  begin
    LeadStr := System.Copy(FullStr, 1, Shorter);
    Result  := ANSISameText(SubStr, LeadStr);
  end;
  // Objetivo: Regresa si una subcadena es igual o esta al inicio de }
  { otra cadena.}

  // Goal: Returns if a substring is equal o is the start of another string.
end;

function Pos(const SubStr: string; const S: string): Integer;
var Len, AIndex: Integer; Found: Boolean;
begin
//  Result := System.Pos(SubStr, S);

  Len := System.Length(S);
  // obtain length of full string
  // obtener longuitud de cadena completa

  AIndex := 1;
  // "AIndex" indicates from which character in full string starts analysis
  // "AIndex" indica de cual caracter eb la cadena completa comienza el analisis

  Found := FALSE;
  while ((not Found) and (AIndex < Len)) do
  begin
    Found := LeadPos(SubStr, System.Copy(S, AIndex, Len));

    Inc(AIndex);
  end;

  if (Found)
    then Result := Pred(AIndex)
    else Result := 0;
  // Objetivo: Regresa el indice de la primer ocurrencia de "Substr".
  // Goal: Returns the AIndex of the first ocurrence of "Substr".
end;

function PosSame(const SubStr: string; const S: string): Integer;
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

function StartsWith
  (const FullStr: string; const SubStr: string): Boolean;
begin
  Result := LeadPos(FullStr, SubStr);
end;

function FinishesWith
  (const FullStr: string; const SubStr: string): Boolean;
var Shorter, Len, AIndex: Integer; TrailingStr: string;
begin
  Shorter := System.Length(SubStr);
  // obtain length of substring
  // obtener longuitud de subcadena

  Len := System.Length(FullStr);
  // obtain length of full string
  // obtener longuitud de cadena completa

  Result := not (Shorter > Len);
  // substring must be shorter or equal size than full string
  // la subcadena debe ser mas corta o de igual tamaño que la cadena completa

  if (Result) then
  begin
    AIndex      := (Len - Shorter + 1);
    TrailingStr := System.Copy(FullStr, AIndex, Shorter);
    Result      := (SubStr = TrailingStr);
  end;
end;

function CharPos(const SubStr: char; const S: string): Word;
var AIndex: Integer; Found: Boolean;
begin
  Found := FALSE; AIndex := System.Length(S);
  while ((AIndex > 0) and (not Found)) do
  begin
    Found := (S[AIndex] = SubStr);
    Inc(AIndex);
  end;

  if (Found)
    then Result := Pred(AIndex)
    else Result := 0;
  // Objetivo: Regresa la primera posicion del caracter dado,
  // desde el inicio de la cadena.

  // Goal: Returns the first location of the given character,
  // from the start of the string.
end;

function CharPosReverse(const SubStr: char; const S: string): Word;
var AIndex: Integer; Found: Boolean;
begin
  Found := FALSE; AIndex := System.Length(S);
  while ((AIndex > 0) and (not Found)) do
  begin
    Found := (S[AIndex] = SubStr);
    Dec(AIndex);
  end;

  if (Found)
    then Result := Succ(AIndex)
    else Result := 0;
  // Objetivo: Regresa la primera posicion del caracter dado,
  // desde el final de la cadena.

  // Goal: Returns the first location of the given character,
  // from the finish of the string.
end;

function CharSetPos
  (const Items: ansicharset; const S: string): Word;
var AIndex, Len: Word; Found: Boolean;
begin
  Found := FALSE;
  Len   := System.Length(S);
  AIndex := 1;

  while ((AIndex <= Len) and (not Found)) do
  begin
    Found := uktansicharsets.IsMember(S[AIndex], Items);
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
  (const Items: ansicharset; const S: string): Word;
var AIndex: Word; Found: Boolean;
begin
  Found := FALSE;
  AIndex := System.Length(S);

  while ((AIndex > 0) and (not Found)) do
  begin
    Found := uktansicharsets.IsMember(S[AIndex], Items);
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

function Left(const AValue: string; const ACount: Byte): string;
var ThisACount: Byte;
begin
  ThisACount := uktstrings.getLength(AValue);
  if (ThisACount > ACount) then
  begin
    ThisACount := ACount;
  end;

  uktstrings.MoveLeft(Result, AValue, ThisACount);
  // Goal: Returns the leftmost characters of "AValue".
  // Objetivo: Regresa los caracteres mas a la izquierda de "AValue".
end;

function Right(const AValue: string; const ACount: Byte): string;
var ThisACount: Byte;
begin
  ThisACount := uktstrings.getLength(AValue);
  if (ThisACount > ACount) then
  begin
    ThisACount := ACount;
  end;

  uktstrings.MoveRight(Result, AValue, ThisACount);
  // Goal: Returns the rightmost characters of "AValue".
  // Objetivo: Regresa los caracteres mas a la derecha de "AValue".
end;

function UnLeft(const AValue: string; const ACount: Byte): string;
begin
  uktstrings.Move(Result, AValue, ACount, getLength(AValue));
  // Goal: Removes the leftmost characters of "AValue".
  // Objetivo: Remueve los caracteres mas a la izquierda de "AValue".
end;

function UnRightCopy(const AValue: string; const ACount: Byte): string;
var AIndex: Integer;
begin
  AIndex := Succ(getLength(AValue)-ACount);
  uktstrings.Move(Result, AValue, 1, AIndex);
  // Goal: Removes the RightCopymost characters of "AValue".
  // Objetivo: Remueve los caracteres mas a la derecha de "AValue".
end;

function TryStrToChar(const AValue: string; var Dest: ansichar): Boolean;
begin
  Result := (getLength(AValue) > 0);
  if (Result)
    then Dest := AValue[1];
  // Goal: To cast a "string", to a "ansichar".
  // Objetivo: Convertir un "string" en un "ansichar".
end;

function StrToCharDef(const AValue: string; const DefAValue: ansichar): ansichar;
begin
  Result := #0;
  if (not TryStrToChar(AValue, Result))
    then Result := DefAValue;
  // Goal: To cast a "string", to a "ansichar".
  // Objetivo: Convertir un "string" en un "ansichar".
end;

function StrToChar(const AValue: string): ansichar;
begin
  Result := #0;
  TryStrToChar(AValue, Result)
  // Goal: To cast a "string", to a "ansichar".
  // Objetivo: Convertir un "string" en un "ansichar".
end;

function PtrToStr(const AValue: pointer): string;
begin
  Result := Pstring(AValue)^;
  // Goal: Copy an string into a dynamic string.
  // Objetivo: Copiar una cadena a una cadena dinamica.
end;

function StrToPtr(const AValue: string): pointer;
begin
  Result := @AValue;
  // Goal: Copy an string into a dynamic string.
  // Objetivo: Copiar una cadena a una cadena dinamica.
end;

procedure UppercaseReplace(var AValue: string);
begin
  AValue := UpperCaseCopy(AValue);
  // Goal: Changes the given string into uppercase.
  // Objetivo: Cambia la cadena dada a mayusculas.
end;

procedure LowercaseReplace(var AValue: string);
begin
  AValue := LowerCaseCopy(AValue);
  // Goal: Changes the given string into lowercase.
  // Objetivo: Cambia la cadena dada a minusculas.
end;

procedure TogglecaseReplace(var AValue: string);
begin
  AValue := TogglecaseCopy(AValue);
  // Goal: Changes the given string into Togglecase.
  // Objetivo: Cambia la cadena dada a minusculas.
end;

procedure CapitalizeReplace(var AValue: string);
begin
  AValue := CapitalizeCopy(AValue);
  // Goal: Changes the given string into capitalize.
  // Objetivo: Cambia la cadena dada a minusculas.
end;

procedure Empty(var AValue: string);
begin
  AValue := EmptyStr;
  // Goal: Clear a ansi string.
  // Objetivo: Limpia una cadena ansi.
end;

procedure Move
  (var Dest: string; const Source: string; AIndex, ACount: Integer);
begin
  uktstrings.Empty(Dest);
  uktstrings.setLength(Dest, ACount);
  System.Move(Source[AIndex], Dest[1], ACount);
  // Objetivo: Copiar el contenido de una cadena a un nueva cadena.
  // Goal: Copy the contents of a string into a new string.
end;

procedure MoveLeft
  (var Dest: string; const Source: string; const ACount: Integer);
begin
  uktstrings.Move(Dest, Source, 1, ACount);
  // Goal: Returns the leftmost characters of "AValue".
  // Objetivo: Regresa los caracteres mas a la izquierda de "AValue".
end;

procedure MoveRight
  (var Dest: string; const Source: string; const ACount: Integer);
var AIndex: Integer;
begin
  AIndex := Succ(getLength(Source)-ACount);
  uktstrings.Move(Dest, Source, AIndex, ACount);
  // Goal: Returns the rightmost characters of "AValue".
  // Objetivo: Regresa los caracteres mas a la derecha de "AValue".
end;

procedure Concat(var Dest: string; const Source: ansichar);
begin
  Dest := Dest + Source;
  // Objetivo: Agregar  un caracter al final de la cadena dada.
  // Goal: Add a character at the end of the given string.
end;

procedure ConcatBack(var Dest: string; const Source: ansichar);
begin
  Dest := Source + Dest;
  // Objetivo: Agregar  un caracter al final de la cadena dada.
  // Goal: Add a character at the end of the given string.
end;

procedure Concat(var Dest: string; const Source: string);
begin
  Dest := Dest + Source;
end;

procedure ConcatBack(var Dest: string; const Source: string);
begin
  Dest := Source + Dest;
end;

function ConcatCopy(const A: string; B: ansichar): string;
begin
  Result := A + B;
  // Objetivo: Agregar  un caracter al final de la cadena dada.
  // Goal: Add a character at the end of the given string.
end;

function ConcatCopy(const A, B: string): string;
begin
  Result := A + B;
end;

function MoveCopy
  (const AValue: string; AIndex, ACount: Byte): string;
begin
  uktstrings.Empty(Result);
  uktstrings.Move(Result, AValue, AIndex, ACount);
  // Objetivo: Copiar el contenido de una cadena a un nueva cadena.
  // Goal: Copy the contents of a string into a new string.
end;

function ReverseCopy(const AValue: string): string;
var I, C: Integer;
begin
  Result := ''; C := System.Length(AValue);
  for I := C downto 1 do
  begin
    Result := Result + AValue[i];
  end;
  // Objetivo: Regresa una copia de la cadena dada al reves.
  // Goal: Returns a copy of the given string backwards.
end;

function SubStr(const PartialStr, FullStr: string; var AIndex: Integer): Boolean;
begin
  AIndex := Pos(PartialStr, FullStr);
  Result := (AIndex > 0);
  // Goal: Returns if a string is contained by other string.
  // Objetivo: Regresa si una cadena esta contenida en otra.
end;

function IsIdentifier(const AValue: string): Boolean;
var I: Integer;
begin
  Result := FALSE;
  if (getLength(AValue) = 0) or (not IsMember(AValue[1], AlphaSet))
    then Exit;

  for I := 2 to getLength(AValue) do
    if (not IsMember(AValue[I], IDSet))
      then Exit;
      
  Result := TRUE;
  // Goal: To return if a string is a valid identifier.
  // Objetivo: Regresar si una cadena es un identificador valido.
end;

function RangeToStr(const Min, Max: ansichar): string;
var I: ansichar; { s := 'a' .. 'z'; }
begin
  uktstrings.Empty(Result);
  for i := Min to Max do
    Concat(Result, i);
  // Goal: Transform a range of characters into a "string".
  // Objetivo: Transformar un rango de caracteres en un "string".

  // Warning: "Min" must be lesser than "Max".
  // Advertencia: "Min" debera ser menor que "Max".
end;

function DeleteCharCopy
  (const AValue: string; A: ansichar): string;
var I, L: Integer;
begin
  Result := ''; L := getLength(AValue);
  for i := 1 to L do
    if (Result[i] <> A)
      then Result := Result + A;
  // Goal: Replace a specific character from a string.
  // Objetivo: Reemplazar un caracter en especifico de una cadena.
end;

function DeleteCharsCopy
(const AValue: string; const Source: ansicharset): string;
var I: Integer;
begin
  uktstrings.Empty(Result);
  for i := 1 to getLength(AValue) do
    if (not IsMember(AValue[i], Source))
      then Concat(Result, AValue[i]);
  // Goal: Replace a specific character set from a string.
  // Objetivo: Reemplazar un conjunto caracter en especifico de una cadena.
end;

function ReplaceCharByCharCopy
  (const AValue: string; A, B: ansichar): string;
var I, L: Integer;
begin
  Result := AValue; L := getLength(AValue);
  for i := 1 to L do
  begin
    if (Result[i] = A) then
    begin
      Result[i] := B;
    end;
  end;
  // Goal: Replace a specific character from a string.
  // Objetivo: Reemplazar un caracter en especifico de una cadena.
end;

function ReplaceCharByStringCopy
  (const AValue: string; A: ansichar; B: string): string;
var I, L: Integer;
begin
  Result := ''; L := getLength(AValue);
  for i := 1 to L do
  begin
    if (AValue[i] = A) then
    begin
      Result := Result + B;
    end else
    begin
      Result := Result + AValue[i];
    end;
  end;
  // Goal: Replace a specific character from a string.
  // Objetivo: Reemplazar un caracter en especifico de una cadena.
end;

function ReplaceCharCopy(const AValue: string; A, B: ansichar): string;
begin
  Result := ReplaceCharByCharCopy(AValue, A, B);
  // Goal: Replace a specific character from a string.
  // Objetivo: Reemplazar un caracter en especifico de una cadena.
end;

function ReplaceCharCopy
  (const AValue: string; A: ansichar; B: string): string;
begin
  Result := ReplaceCharByStringCopy(AValue, A, B);
  // Goal: Replace a specific character from a string.
  // Objetivo: Reemplazar un caracter en especifico de una cadena.
end;

function ReplaceCharsCopy
 (const AValue: string;
  const Source: ansicharset; Dest: ansichar): string;
var I: Integer;
begin
  uktstrings.Empty(Result);
  for i := 1 to getLength(AValue) do
    if (IsMember(AValue[i], Source))
      then Concat(Result, Dest)
      else Concat(Result, AValue[i]);
  // Goal: Replace a specific character set from a string.
  // Objetivo: Reemplazar un conjunto caracter en especifico de una cadena.
end;

procedure ReplaceCharByChar(var AValue: string; A, B: ansichar);
var I, L: Integer;
begin
  L := getLength(AValue);
  for i := 1 to L do
  begin
    if (AValue[i] = A) then
    begin
      AValue[i] := B;
    end;
  end;
  // Goal: Replace a specific character from a string.
  // Objetivo: Reemplazar un caracter en especifico de una cadena.
end;

procedure ReplaceCharByString
  (var AValue: string; A: ansichar; B: string);
begin
  AValue := ReplaceCharByStringCopy(AValue, A, B);
end;

procedure ReplaceChar(var AValue: string; A, B: ansichar);
begin
  ReplaceCharByChar(AValue, A, B);
  // Goal: Replace a specific character from a string.
  // Objetivo: Reemplazar un caracter en especifico de una cadena.
end;

procedure ReplaceChar
  (var AValue: string; A: ansichar; B: string);
begin
  ReplaceCharByString(AValue, A, B);
  // Goal: Replace a specific character from a string.
  // Objetivo: Reemplazar un caracter en especifico de una cadena.
end;

procedure ReplaceChars
 (var AValue: string; const Source: ansicharset; Dest: ansichar);
var I: Integer;
begin
  uktstrings.Empty(AValue);
  for i := 1 to getLength(AValue) do
  begin
    if (IsMember(AValue[i], Source))
      then Concat(AValue, Dest)
      else Concat(AValue, AValue[i]);
  end;
  // Goal: Replace a specific character set from a string.
  // Objetivo: Reemplazar un conjunto caracter en especifico de una cadena.
end;

function ReplaceStrCopy
  (const AValue: string; const Source, Dest: string): string;
var Finished: Boolean; AIndex: Integer; temp: ansistring;
begin
  AIndex := 1;
  Result := AValue;
  repeat
    temp := Result;
    ReplacePos(temp, AIndex, Source, Dest);
    Result := temp;
    Finished := (AIndex = 0);
  until Finished;
  // Goal: Replaces all the "Source" found, "AValue" with "Dest".
  // Objetivo: Reemplaza todas las "Source" encontradas en "AValue" por "Dest".}
end;

procedure ReplaceStr
  (var AValue: string; const Source, Dest: string);
var Finished: Boolean; AIndex: Integer; temp: ansistring;
begin
  AIndex := 1;
  repeat
    temp := AValue;
    ReplacePos(temp, AIndex, Source, Dest);
    AValue := temp;
    Finished := (AIndex = 0);
  until Finished;
  // Goal: Replaces all the "Source" found, "AValue" with "Dest".
  // Objetivo: Reemplaza todas las "Source" encontradas en "AValue" por "Dest".}
end;

function ReplaceCopy
  (const AValue, Source, Dest: string): string;
var AIndex: Integer;
begin
  Result := AValue;
  ReplacePos(Result, AIndex, Source, Dest);
  // Goal: Returns a copy of the string with the given substring replaced.
  // Objetivo: Regresa una copia de la cadena con la subcadena reemplazada.
end;

function ReplaceCopySame
  (const AValue, Source, Dest: string): string;
var AIndex: Integer;
begin
  Result := AValue;
  ReplacePosSame(Result, AIndex, Source, Dest);
  // Goal: Returns a copy of the string with the given substring replaced.
  // Objetivo: Regresa una copia de la cadena con la subcadena reemplazada.
end;

function DeleteCopy(const AValue, Source: string): string;
begin
  Result := ReplaceCopy(AValue, Source, '');
  // Goal: Returns a copy of the string with the given substring deleted.
  // Objetivo: Regresa una copia de la cadena con la subcadena eliminada.
end;

procedure ReplacePos
  (var AValue: string;
   var AIndex: Integer; const Source, Dest: string);
var BeforeACount, AfterACount, S, V: Integer;
    BeforeString, AfterString: string;
begin
  AIndex := Pos(Source, AValue);
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

procedure ReplacePosSame
  (var AValue: string;
   var AIndex: Integer; const Source, Dest: string);
var BeforeACount, AfterACount, S, V: Integer;
    BeforeString, AfterString: string;
begin
  AIndex := PosSame(Source, AValue);
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

procedure Replace
  (var AValue: string; const Source, Dest: string);
var AIndex: Integer;
begin
  ReplacePos(AValue, AIndex, Source, Dest);
  // Goal: Replaces the first "Source" found, "AValue" with "Dest".
  // Objetivo: Reemplaza la primera "Source" encontrada en "AValue" por "Dest".}
end;

function ReplaceCharToStrCopy
  (const AValue: string;
   const Source: ansichar; const Dest: string): string;
var I: Integer;
begin
  uktstrings.Empty(Result);
  for i := 1 to Length(AValue) do
    if (AValue[i] <> Source)
      then uktstrings.Concat(Result, AValue[i])
      else uktstrings.Concat(Result, Dest)
  // Goal: Replaces the first "Source" found, "AValue" with "Dest".
  // Objetivo: Reemplaza la primera "Source" encontrada en "AValue" por "Dest".}
end;

procedure ReplaceCharToStr
  (var AValue: string; const Source: ansichar; const Dest: string);
begin
  AValue := ReplaceCharToStrCopy(AValue, Source, Dest);
  // Goal: Replaces the first "Source" found, "AValue" with "Dest".
  // Objetivo: Reemplaza la primera "Source" encontrada en "AValue" por "Dest".}
end;

function RemoveCharCopy
  (const AValue: string; IgnoreChar: ansichar): string;
var I: Integer;
begin
  uktstrings.Empty(Result);
  for i := 1 to Length(AValue) do
    if (AValue[i] <> IgnoreChar)
      then uktstrings.Concat(Result, AValue[i]);
  // Goal: Delete a specific character from a string.
  // Objetivo: Eliminar un caracter en especifico de una cadena.
end;

function RemoveCharsCopy
  (const AValue: string; IgnoreChars: ansicharset): string;
var I: Integer; R: string;
begin
  R := Result;
  uktstrings.Empty(R);
  for i := 1 to Length(AValue) do
    if (not IsMember(AValue[i], IgnoreChars))
      then uktstrings.Concat(R, AValue[i]);
  Result := R;
  // Goal: Delete a specific character set from a string.
  // Objetivo: Eliminar un conjunto caracter en especifico de una cadena.
end;

procedure RemoveChar(var AValue: string; IgnoreChar: ansichar);
begin
  AValue := RemoveCharCopy(AValue, IgnoreChar);
  // Goal: Delete a specific character from a string.
  // Objetivo: Eliminar un caracter en especifico de una cadena.
end;

procedure RemoveChars(var AValue: string; IgnoreChars: ansicharset);
begin
  AValue := RemoveCharsCopy(AValue, IgnoreChars);
  // Goal: Delete a specific character set from a string.
  // Objetivo: Eliminar un conjunto caracter en especifico de una cadena.
end;

function SameStrAt
  (const SubStr, Str: string; var AIndex: Word): Boolean;
var Match: Boolean; LastStr, LastSubStr, I, J: Integer;
begin
  LastStr := System.Length(Str);
  LastSubStr := System.Length(SubStr);
  I := 1;
  J := AIndex;

  Match := LastSubStr <= Succ(LastStr - AIndex);
//  Match := Succ(LastStr - AIndex) >= LastSubStr;
  // verificar que las subcadena sea de menor longuitud que la cadena destino

  if (Match) then
  repeat
    Match := uktansichars.SameText(SubStr[i], Str[j]);

    Inc(I);
    Inc(J);
  until not Match or (I > LastSubStr);
  Result := Match;
  // Objetivo: Buscar una subcadena en otra cadena,
  // y que inicie a partir de la posicion indicada,
  // ignorando el caso sensitivo.
end;

function EqualStrAt
  (const SubStr, Str: string; var AIndex: Word): Boolean;
var Match: Boolean; LastStr, LastSubStr, I, J: Integer;
begin
  LastStr := System.Length(Str);
  LastSubStr := System.Length(SubStr);
  I := 1;
  J := AIndex;

  Match := LastSubStr <= Succ(LastStr - AIndex);
//  Match := Succ(LastStr - AIndex) >= LastSubStr;
  // verificar que las subcadena sea de menor longuitud que la cadena destino

  if (Match) then
  repeat
    Match := (SubStr[i] = Str[j]);

    Inc(I);
    Inc(J);
  until not Match or (I > LastSubStr);
  Result := Match;
  // Objetivo: Buscar una subcadena en otra cadena,
  // y que inicie a partir de la posicion indicada,
  // tomando en cuenta el caso sensitivo.
end;

function SameStrIn
  (const SubStr, Str: string; var AIndex: Word): Boolean;
begin
  Result := false;
  AIndex := 1;
  //Result := Search(SubStr, Str, AIndex, {@}SameStrAt);
  Result := Search(SubStr, Str, AIndex, @SameStrAt);
  // Objetivo: Buscar una subcadena en otra cadena,
  // sin importar la posicion,
  // ignorando el caso sensitivo.
end;

function EqualStrIn
  (const SubStr, Str: string; var AIndex: Word): Boolean;
begin
  Result := false;
  AIndex := 1;
  //Result := Search(SubStr, Str, AIndex, {@}EqualStrAt);
  Result := Search(SubStr, Str, AIndex, @EqualStrAt);
  // Objetivo: Buscar una subcadena en otra cadena,
  // sin importar la posicion,
  // tomando en cuenta el caso sensitivo.
end;

function SameStr
  (const SubStr, Str: string; var AIndex: Word): Boolean;
var Match: Boolean; Last1, Last2: Integer;
begin
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
  (const SubStr, Str: string; var AIndex: Word): Boolean;
var Match: Boolean; Last1, Last2: Integer;
begin
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
  (const SubStr, Str: string; var AIndex: Word;
   CompareStrAt: TCompareStrAt): Boolean;
var Len: Word; Found: Boolean;
begin
  Len := System.Length(Str);
  Found  := FALSE;

  if (CompareStrAt <> nil) then
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

function ScanEqual
 (const Str: string; const SubStr: string; {copy} AIndex: Word): Word;
var Len: Word; Found: Boolean;
begin
  // strings starts with "1"
  Result := 0;

  Len := System.Length(Str);
  Found  := FALSE;

  while (not Found and (AIndex <= Len)) do
  begin
    Found := EqualStrAt(SubStr, Str, AIndex);
    if (not Found) then
    begin
      Inc(AIndex);
    end;
  end;

  if (Found)
    then Result := AIndex
    else Result := 0;
  // Goal: Searches for a substring inside other string
  // beginning at the "AIndex" char and returns the next character,
  // is case sensitive.

  // Objetivo: Busca una subcadena adentro de otra cadena
  // comenzando en el caracter numero "indice" y regresa el siguiente caracter,
  // es sensible al caso.
end;

function ScanSame
 (const Str: string; const SubStr: string; {copy} AIndex: Word): Word;
var Len: Word; Found: Boolean;
begin
  // strings starts with "1"
  Result := 0;

  Len := System.Length(Str);
  Found  := FALSE;

  while (not Found and (AIndex <= Len)) do
  begin
    Found := SameStrAt(SubStr, Str, AIndex);
    if (not Found) then
    begin
      Inc(AIndex);
    end;
  end;

  if (Found)
    then Result := AIndex
    else Result := 0;
  // Goal: Searches for a substring inside other string
  // beginning at the "AIndex" char and returns the next character,
  // is case sensitive.

  // Objetivo: Busca una subcadena adentro de otra cadena
  // comenzando en el caracter numero "indice" y regresa el siguiente caracter,
  // NO es sensible al caso.
end;

function SkipChar
  (const AValue: string; AIndex: Integer; ValidChar: ansichar): Integer;
var L: Integer;
begin
  Result := AIndex; L := Length(AValue);
  if (Result <= L) and (AValue[Result] = ValidChar) then
  begin
    Inc(Result);
  end;
  // Goal: Returns a single character.
  // Objetivo: Regresa un solo caracter.
end;

function SkipChars
  (const AValue: string; AIndex: Integer; ValidChars: ansicharset): Integer;
var L: Integer;
begin
  Result := AIndex; L := Length(AValue);
  if (Result <= L) and IsMember(AValue[Result], ValidChars) then
  begin
    Inc(Result);
  end;
  // Goal: Returns a single character.
  // Objetivo: Regresa un solo caracter.
end;

function SkipCharWhile
  (const AValue: string; AIndex: Integer; ValidChar: ansichar): Integer;
var L: Integer;
begin
  Result := AIndex; L := Length(AValue);
  while ((Result <= L) and (AValue[Result] = ValidChar)) do
  begin
   Inc(Result);
  end;
  // Goal: Returns a group of characters.
  // Objetivo: Regresa un grupo de caracteres.
end;

function SkipCharUntil
  (const AValue: string; AIndex: Integer; BreakChar: ansichar): Integer;
var L: Integer;
begin
  Result := AIndex; L := Length(AValue);
  while ((Result <= L) and (AValue[Result] <> BreakChar)) do
  begin
   Inc(Result);
  end;
  // Goal: Returns a group of characters.
  // Objetivo: Regresa un grupo de caracteres.
end;

function SkipWhile
  (const AValue: string; AIndex: Integer; ValidChars: ansicharset): Integer;
var L: Integer;
begin
  Result := AIndex; L := Length(AValue);
  while ((Result <= L) and IsMember(AValue[Result], ValidChars)) do
  begin
   Inc(Result);
  end;
  // Goal: Returns a group of non-space characters.
  // Objetivo: Regresa un grupo de caracteres que no son espacios.
end;

function SkipUntil
  (const AValue: string; AIndex: Integer; BreakChars: ansicharset): Integer;
var L: Integer;
begin
  Result := AIndex; L := Length(AValue);
  while ((Result <= L) and not IsMember(AValue[Result], BreakChars)) do
  begin
   Inc(Result);
  end;
  // Goal: Returns a group of non-space characters.
  // Objetivo: Regresa un grupo de caracteres que no son espacios.
end;

function SkipToken(const AValue: string; var S, F: Integer): string;
var StartAIndex, FinishAIndex: Integer;
begin
  StartAIndex  := S;

  StartAIndex  := SkipCharWhile(AValue, StartAIndex, #32);
  FinishAIndex := SkipCharUntil(AValue, StartAIndex, #32);
  Result      := CopyFromTo(AValue, StartAIndex, FinishAIndex);

  S := StartAIndex;
  F := FinishAIndex;
end;

function SkipDigit(const AValue: string; var I, F: Integer): string;
begin
  I := SkipCharWhile(AValue, I, #32);
  F := SkipWhile(AValue, I, DigitSet);
  Result := CopyFromTo(AValue, I, F);
  I := F;
end;

function SkipLetter(const AValue: string; var I, F: Integer): string;
begin
  I := SkipCharWhile(AValue, I, #32);
  F := SkipWhile(AValue, I, AlphaSet);
  Result := CopyFromTo(AValue, I, F);
  I := F;
end;

function CopyFromTo
  (const Source: string; Start, Finish: Integer): string;
var I, ACount: Integer;
begin
  ACount := Succ(Finish - Start);

  Result := '';
  for I := 1 to ACount do
  begin
     Result := Result + Source[1];
  end;
//  uktstrings.Move(Result[1], Source[1], Start, ACount);
  // Goal: Returns a substring from "start" to "finish".
  // Objetivo: Regresa una subcadena desde "start" hasta "finish".
end;

function CopyFrom
  (const Source: string; AIndex: Integer): string;
var I, ACount: Integer;
begin
  ACount := Length(Source);
  ACount := Succ(ACount - AIndex);

  Result := '';
  for I := 1 to ACount do
  begin
    Result := Result + Source[1];
  end;
//  uktstrings.Move(Result[1], Source[1], Start, ACount);
  // Goal: Returns a substring from "start" to "finish".
  // Objetivo: Regresa una subcadena desde "start" hasta "finish".
end;

function ParseFrom
  (const Source: string; var Start, Finish: Integer): string;
var I, ACount: Integer;
begin
  ACount := (Finish - Start);

  Result := '';
  for I := 1 to ACount do
  begin
     Result := Result + Source[(Start + i - 1)];
  end;

  Start  := Finish;
  // Goal: Returns a substring from "start" to "finish" & update "start".

  // Objetivo: Regresa una subcadena desde "start" hasta "finish"
  // y actualiza "start.
end;

function SkipEmpty(const Start, Finish: Integer): Boolean;
begin
  Result := ((Finish - Start) = 0);
  // Goal: Returns if there is a SubStr between the given indices is empty.
  // Objetivo: Regresa si hay una subcadena entre los indices dados esta vacia.
end;

function ExtractCharUntil
 (const AValue: string; AIndex: Integer; BreakChar: ansichar): string;
var I, L: Integer;
begin
  uktstrings.Empty(Result);

  I := AIndex; L := Length(AValue);
  while ((I <= L) and (AValue[I] = #32)) do
  begin
    Inc(I);
  // Jump leading spaces
  // Brincar espacios al inicio
  end;

  while ((I <= L) and (AValue[I] <> BreakChar)) do
  begin
    uktstrings.Concat(Result, AValue[I]);
    Inc(I);
  end;
  // Goal: Returns a list of characters until a special marker is found.

  // Objetivo: Regresa una lista de caracteres hasta
  // que un marcador especial es encontrado.
end;

function ExtractCharUntilBack
 (const AValue: string; AIndex: Integer; BreakChar: ansichar): string;
var I, L: Integer;
begin
  uktstrings.Empty(Result);

  L := Length(AValue);
  I := L;  

  while ((I > AIndex) and (AValue[I] <> BreakChar)) do
  begin
    uktstrings.ConcatBack(Result, AValue[I]);
    Dec(I);
  end;
  // Goal: Returns a list of characters until a special marker is found.

  // Objetivo: Regresa una lista de caracteres hasta
  // que un marcador especial es encontrado.
end;

function ExtractWhile
  (const AValue: string; AIndex: Integer; SkipChars: ansicharset): string;
var I, L: Integer;
begin
  uktstrings.Empty(Result);

  I := AIndex; L := Length(AValue);
  while ((I <= L) and (AValue[I] = #32)) do
  begin
    Inc(I);
  // Jump leading spaces
  // Brincar espacios al inicio
  end;

  while ((I <= L) and IsMember(AValue[I], SkipChars)) do
  begin
    uktstrings.Concat(Result, AValue[I]);
    Inc(I);
  end;
  // Goal: Returns a group of non-space selected characters.
  // Objetivo: Regresa un grupo de caracteres seleccionados que no son espacios.
end;

function ExtractUntil
  (const AValue: string;
     var AIndex: Integer; BreakChars: ansicharset): string;
var I, L: Integer;
begin
  uktstrings.Empty(Result);

  I := AIndex; L := Length(AValue);
  while ((I <= L) and (AValue[I] = #32)) do
  begin
    Inc(I);
  // Jump leading spaces
  // Brincar espacios al inicio
  end;

  while ((I <= L) and not IsMember(AValue[I], BreakChars)) do
  begin
    uktstrings.Concat(Result, AValue[I]);
    Inc(I);
  end;

  AIndex := I;
  // Goal: Returns a group of non-space characters.
  // Objetivo: Regresa un grupo de caracteres que no son espacios.
end;

function ExtractString(const AValue: string; AIndex: Integer): string;
var I, L: Integer;
begin
  uktstrings.Empty(Result);

  I := AIndex; L := Length(AValue);
  while ((I <= L) and (AValue[I] = #32)) do
  begin
    Inc(I);
  // Jump leading spaces
  // Brincar espacios al inicio
  end;

  while ((I <= L) and (AValue[I] <> #32)) do
  begin
    uktstrings.Concat(Result, AValue[I]);
    Inc(I);
  end;
  // Goal: Returns a group of non-space characters.
  // Objetivo: Regresa un grupo de caracteres que no son espacios.
end;

function LastChar(const Astring: string): ansichar;
var L: Integer;
begin
  L := Length(Astring);
  if (L < 1)
    then Result := #0
    else Result := Astring[ L ];
  // Goal: To return the last ansichar of a string.
  // Objetivo: Regresar el ultimo caracter de una cadena.
end;

function AlignLeft
 (const Source: string; ACount: Integer; AValue: ansichar): string;
var Difference: Integer;
begin
  Difference := ACount - getLength(Source);
  uktstrings.Empty(Result);
  uktstrings.Concat(Result, Source);
  uktstrings.Concat(Result, StringOfChar(AValue, Difference));
  // Goal: Add characters to the right of a string.
  // Objetivo: Agregar caracteres a la derecha o final de una cadena.
end;

function AlignRight
 (const Source: string; ACount: Integer; AValue: ansichar): string;
var Difference: Integer;
begin
  Difference := ACount - getLength(Source);
  uktstrings.Empty(Result);
  uktstrings.Concat(Result, StringOfChar(AValue, Difference));
  uktstrings.Concat(Result, Source);
  // Goal: Add characters to the left of a string.
  // Objetivo: Agregar caracteres a la izquierda o inicio de una cadena.
end;

function SameText(const A, B: string): Boolean;
begin
  Result := SysUtils.SameText(A, B);
  // Goal: Returns if 2 strings are equal, ignores sensitive case.
  // Objetivo: Regresa si 2 cadenas son iguales, ignorar caso sensitivo.
end;

function Equal(const A, B: string): Boolean;
begin
  Result := (A = B);
  // Goal: Returns if 2 strings are equal.
  // Objetivo: Regresa si 2 cadenas son iguales.
end;

function Compare(const A, B: string): TComparison;
begin
  if (A = B)
    then Result := cmpEqual
  else if (A < B)
    then Result := cmpLower
  else Result := cmpHigher
  // Goal: Returns the comparison between 2 strings.
  // Objetivo: Regresa la comparacion de 2 cadenas.
end;

function EqualByOptions
  (const A, B: string; Options: TStringOptions): Boolean;
begin
  case (Options) of
    soExactMatch: Result := (A = B);
    soForceMatch: Result := (UppercaseCopy(A) = B);
    soForceFirst: Result := (UppercaseCopy(A) = UppercaseCopy(B));
    soForceLast:  Result := (A = UppercaseCopy(B));
    else Result := FALSE;
  end;
  // Goal: Returns if 2 strings are equals uppon the given options.
  // Objetivo: Regresa si 2 cadenas son iguales basado en las opciones dadas.
end;

function CompareByOptions
  (const A, B: string; Options: TStringOptions): TComparison;
begin
  case Options of
    soExactMatch: Result := Compare(A, B);
    soForceMatch: Result := Compare(UppercaseCopy(A), UppercaseCopy(B));
    soForceFirst: Result := Compare(UppercaseCopy(A), B);
    soForceLast:  Result := Compare(A, UppercaseCopy(B));
    else          Result := cmpHigher;
  end;
  // Goal: Returns if 2 strings are equals upon the given options.
  // Objetivo: Regresa si 2 cadenas son iguales basado en las opciones dadas.
end;

(* global random string functions *)

function Internal_RandomRange( AMin, AMax: Byte ): Byte;
begin
  Result := Random(AMax-AMin)+AMin;
  // Objetivo: Obtener un numero aleatorio en el rango entero indicado.
  // Goal: Obtain a random number in the indicated integer range.
end;

function RandomAlphaNumString(const ACount: Integer): string;
begin
  Result := StringOfChar( #32, ACount );

  // keep first character as letter
  Result[1] := Chr(Internal_RandomRange(65, 90));


  // Objetivo: Obtener una cadena de caracteres alfanumericos aleatorios,
  // con la longuitud indicada.
  // Goal: Obtain a string of alphanumeric random characters,
  // with the given length.
end;

function RandomAlphaString(const ACount: Integer): string;
var i: Integer;
begin
  Result := StringOfChar( #32, ACount );
  for i := 1 to ACount do
  begin
    Result[i] := Chr(Internal_RandomRange(65, 90));
  end;
  // Objetivo: Obtener una cadena de caracteres alfabeticos aleatorios,
  // con la longuitud indicada.
  // Goal: Obtain a string of alphabetic random characters,
  // with the given length.
end;

function RandomNumString(const ACount: Integer): string;
var i: Integer;
begin
  Result := StringOfChar( #32, ACount );
  for i := 1 to ACount do
  begin
    Result[i] := Chr(Internal_RandomRange(48, 57));
  end;
  // Objetivo: Obtener una cadena de caracteres numericos aleatorios,
  // con la longuitud indicada.
  // Goal: Obtain a string of random numerical characters,
  // with the given string.
end;

end.


