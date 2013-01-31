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

unit uktansinullstrs;

     // Nombre   : ansinullstring*s
     // Objetivo : Es una Unidad para definir Funciones
     //            para Cadenas terminadas en Nulo;
     // Autor    : Marco Aurelio Ramirez Carrillo;
     // Fecha    : Domingo 31 de Diciembre de 1995;

interface
uses
{$IFDEF MSWINDOWS}
  Windows, Messages,
{$ENDIF}
{$IFDEF LINUX}
  Types, Libc,
{$ENDIF}
  uktfloats, uktcomparisons, uktansimemos, uktansiarrays,
  uktansichars,
  uktansicharsets,
  ukttextconsts,
  uktansicharsetconsts,
  dummy;

type
  ansinullstring = pansichar;

//  tansichararray = array[0..100] of char;
//  pansichararray = ^tansichararray;

{ global properties }

  function getQuotedUppercase
    (const AValue: ansinullstring; const A, B: char): ansinullstring; overload;
  function getQuotedLowercase
    (const AValue: ansinullstring; const A, B: char): ansinullstring; overload;

  procedure setQuotedUppercase
    (const AValue: ansinullstring; const A, B: char); overload;
  procedure setQuotedLowercase
    (const AValue: ansinullstring; const A, B: char); overload;

{ global functions }

  function SameText(const A, B: ansinullstring): Boolean;

  function IsNull(const AValue: ansinullstring): Boolean; overload;
  function IsEmpty(const AValue: ansinullstring): Boolean; overload;
  function IsStringOfChar(const S: ansinullstring; C: Char): Boolean;

  function Length(const AValue: ansinullstring): word; overload;
  function LengthSize(const AValue: ansinullstring; const MaxSize: Integer): word; overload;

  function UppercaseCopy
   (const AValue: ansinullstring): ansinullstring; overload;
  function LowercaseCopy
   (const AValue: ansinullstring): ansinullstring; overload;
  function TogglecaseCopy
   (const AValue: ansinullstring): ansinullstring; overload;
  function CapitalizeCopy
   (const AValue: ansinullstring): ansinullstring; overload;

  function SameChar(const AValue: Char; const ACount: Word): ansinullstring;

  function StartsWith
    (const SubStr: ansinullstring; const S: ansinullstring): Boolean;
  function FinishesWith
    (const SubStr: ansinullstring; const S: ansinullstring): Boolean;

  function CharPos
    (const SubStr: char; const S: ansinullstring): ansinullstring; overload;
  function CharPosReverse
    (const SubStr: char; const S: ansinullstring): ansinullstring; overload;

  function Left
   (const AValue: ansinullstring; const ACount: Word): ansinullstring;
  function Right
   (const AValue: ansinullstring; const ACount: Word): ansinullstring;

  procedure UppercaseReplace
   (const AValue: ansinullstring); overload;
  procedure LowercaseReplace
   (const AValue: ansinullstring); overload;
  procedure TogglecaseReplace
   (const AValue: ansinullstring); overload;
  procedure CapitalizeReplace
   (const AValue: ansinullstring); overload;

  procedure MoveNullStr
   (const Source: ansinullstring; var Dest: ansinullstring; const ACount: Word);
  function CopyNullStr
    (const Source: ansinullstring; var Dest: ansinullstring): Word;
  function DuplicateNullStr(const AValue: ansinullstring): ansinullstring;
  function ReverseNullStr(const AValue: ansinullstring): ansinullstring;

  function AllocNullStr(const Size: Word): ansinullstring;
  procedure FreeNullStr(var AValue: ansinullstring);

  function StrToNullStr(const AValue: string): ansinullstring;

  function ClearNullStr(var AValue: ansinullstring; Size: Word): ansinullstring;

  function IsAssigned(var AValue: ansinullstring): Boolean;
  function IsTerminated(var AValue: ansinullstring): Boolean;
  procedure AssignEmpty(var AValue: ansinullstring);

  // operator =(const A, B: ansinullstring): Boolean; overload;
  function Equal(const A, B: ansinullstring): Boolean;

  function EqualForceMatch(const A, B: ansinullstring): Boolean;
  function EqualForceFirst(const A, B: ansinullstring): Boolean;
  function EqualForceLast(const A, B: ansinullstring): Boolean;
  function EqualByOptions
    (const A, B: ansinullstring; Options: TStringOptions): Boolean;

  function Compare(const A, B: ansinullstring): TComparison;

  function CompareForceMatch(const A, B: ansinullstring): TComparison;
  function CompareForceFirst(const A, B: ansinullstring): TComparison;
  function CompareForceLast(const A, B: ansinullstring): TComparison;
  function CompareByOptions
    (const A, B: ansinullstring; Options: TStringOptions): TComparison;

  function StrAsNullStr(var AValue: shortstring): ansinullstring;
  function MemoAsNullStr(var AValue: ansimemo): ansinullstring;

  procedure AssignNullStr(var Dest: ansinullstring; const AValue: ansinullstring);
  procedure AssignRange(var Dest: ansinullstring; const Min, Max: Char);

  function ScanNull
    (const AValue: ansinullstring): ansinullstring;

  function Scan
    (const Str, SubStr: ansinullstring): ansinullstring; overload;
  function Scan
    (const Str: ansinullstring; const SubStr: char): ansinullstring; overload;
  function Scan
    (const Str: ansinullstring; const SubStr: string): ansinullstring; overload;
  function ScanSet
    (const Str: ansinullstring; const SubStr: ansicharset): ansinullstring; overload;

  function ReverseScan
    (const Str, SubStr: ansinullstring): ansinullstring; overload;
  function ReverseScan
    (const Str: ansinullstring; const SubStr: char): ansinullstring; overload;
  function ReverseScan
    (const Str: ansinullstring; const SubStr: string): ansinullstring; overload;
  function ReverseScanSet
    (const Str: ansinullstring; const SubStr: ansicharset): ansinullstring; overload;

  function MatchWhile
 ({ref} AValue: ansinullstring; const ValidChars: ansicharset): ansinullstring; overload;
  function StringInSet
 ({ref} AValue: ansinullstring; const ValidChars: ansicharset): Boolean; overload;

  function ReplaceChar
    ({ref} AValue: ansinullstring; A, B: Char): ansinullstring; overload;

  procedure SkipCharWhile(var AValue: ansinullstring; ValidChar: Char);
  procedure SkipCharUntil(var AValue: ansinullstring; BreakChar: Char);

  procedure SkipCharWhileCount
    (var AValue: ansinullstring; ValidChar: Char; const ACount: Word);

  procedure SkipWhile(var AValue: ansinullstring; ValidChars: ansicharset);
  procedure SkipUntil(var AValue: ansinullstring; BreakChars: ansicharset);

  function NullStrToChar(const AValue: ansinullstring): char;
  function NullStrToStr(const AValue: ansinullstring): string;
  function NullStrToDate(const AValue: ansinullstring): TDateTime;
  function NullStrToTime(const AValue: ansinullstring): TDateTime;
  function NullStrToInt(const AValue: ansinullstring): Integer;
  function NullStrToFloat(const AValue: ansinullstring): TDBFloat;
  function NullStrToCurr(const AValue: ansinullstring): Currency;

  function AssignStr(var Dest: ansinullstring; const AValue: string): ansinullstring;
  function AssignDate(var Dest: ansinullstring; const AValue: TDateTime): ansinullstring;
  function AssignTime(var Dest: ansinullstring; const AValue: TDateTime): ansinullstring;
  function AssignInt(var Dest: ansinullstring; const AValue: Integer): ansinullstring;
  function AssignFloat(var Dest: ansinullstring; const AValue: TDBFloat): ansinullstring;
  function AssignCurr(var Dest: ansinullstring; const AValue: Currency): ansinullstring;

  function CopyToStr(var AValue: ansinullstring; const ACount: Word): string;

  function SkipChar(var AValue: ansinullstring): char;
  procedure SkipBlanks(var AValue: ansinullstring);

  function ExtractCharWhile
    (var AValue: ansinullstring; ValidChar: Char): string;
  function ExtractCharUntil
    (var AValue: ansinullstring; BreakChar: Char): string;
  function ExtractWhile
    (var AValue: ansinullstring; ValidChars: ansicharset): string;
  function ExtractUntil
    (var AValue: ansinullstring; BreakChars: ansicharset): string;

  function ParseWhile
    (const AValue: ansinullstring; ValidChars: ansicharset): string;

  function IsWildcard({ref} AValue: ansinullstring): Boolean;

  function ExtractToken(var AValue: ansinullstring): string;
  function ExtractDelimiter
    (var AValue: ansinullstring; const Delimiter: char): string;
  function ExtractInt(var AValue: ansinullstring): Integer;
  function ExtractAlpha(var AValue: ansinullstring): string;
  function ExtractID(var AValue: ansinullstring): string;
  function ExtractChar(var AValue: ansinullstring): Char;

  function ExtractCount(var AValue: ansinullstring; const ACount: Word): string;

  function ExtractSQuoted(var AValue: ansinullstring): string;
  function ExtractDQuoted(var AValue: ansinullstring): string;

  function ExtractTrimSQuoted(var AValue: ansinullstring): string;
  function ExtractTrimDQuoted(var AValue: ansinullstring): string;

  function ExtractWildcard(var AValue: ansinullstring): string;

  function ExtractWhileMatch(var AValue: ansinullstring; const S: string): string;

  procedure ReadLn(var T: TextFile; var AValue: ansinullstring);

implementation
uses SysUtils, Math;

{ global properties }

function getQuotedUppercase
  (const AValue: ansinullstring; const A, B: char): ansinullstring;
begin
  Result := DuplicateNullStr(AValue);
  uktansinullstrs.setQuotedUppercase(Result, A, B);
  // Goal: Returns a uppercase copy of the given string,
  // without modifying delimited substrings.

  // Objetivo: Regresa una copia en mayusculas de la cadena dada,
  // sin modificar a las subcadenas delimitadas.
end;

function getQuotedLowercase
  (const AValue: ansinullstring; const A, B: char): ansinullstring;
begin
  Result := DuplicateNullStr(AValue);
  uktansinullstrs.setQuotedLowercase(Result, A, B);
  // Goal: Returns a lowercase copy of the given ansinullstring,
  // without modifying delimited substrings.

  // Objetivo: Regresa una copia en minusculas de la cadena terminada en nulo,
  // dada sin modificar a las subcadenas delimitadas.
end;

procedure setQuotedUppercase(const AValue: ansinullstring; const A, B: char);
var InsideString: Boolean; P: ansinullstring;
begin
  InsideString := FALSE; P := AValue;
  while (not IsTerminated(P)) do
  begin
    if (InsideString)
      then InsideString := not (P^= B)
      else InsideString := (P^= A);
    // initial or final delimiter found ?
    // delimitador inicial o final encontrado ?

    if (not InsideString)
      then P^ := System.UpCase(P^);
    // replace characters
    // reemplazar caracteres

    Inc(P);
  end;
  // Goal: Changes the given null terminated string into uppercase,
  // without modifying delimited substrings.

  // Objetivo: Cambia la cadena terminada en nulo dada a mayusculas.
  // sin modificar a las subcadenas delimitadas.
end;

procedure setQuotedLowercase(const AValue: ansinullstring; const A, B: char);
var InsideString: Boolean; P: ansinullstring;
begin
  InsideString := FALSE; P := AValue;
  while (not IsTerminated(P)) do
  begin
    if (InsideString)
      then InsideString := not (P^= B)
      else InsideString := (P^= A);
    // initial or final delimiter found ?
    // delimitador inicial o final encontrado ?

    if (not InsideString)
      then P^ := Chr(Ord(P^) + 32);
    // replace characters
    // reemplazar caracteres

    Inc(P);
  end;
  // Goal: Changes the given null terminated string into lowercase,
  // without modifying delimited substrings.

  // Objetivo: Cambia la cadena terminada en nulo dada a minusculas.
  // sin modificar a las subcadenas delimitadas.
end;

{ global functions }

function SameText(const A, B: ansinullstring): Boolean;
var C, D: pansichar;
begin
  Result := FALSE;
  C := A; D := B;

  if (Assigned(A) and Assigned(B)) then
  while (not IsNull(C^) and not IsNull(D^) and (not Result)) do
  begin
    Result := uktansichars.SameText(C^, D^);
    Inc(C);
    Inc(D);
  end;
//  Result := SysUtils.SameText(A, B);
  // Goal: Returns if 2 strings are equal, ignores sensitive case.
  // Objetivo: Regresa si 2 cadenas son iguales, ignora caso sensitivo.
end;

function IsNull(const AValue: ansinullstring): Boolean;
begin
  Result := (AValue^ = #0);
  // Goal: Returns if a string points to a null character.
  // Objetivo: Regresa si una cadena apunta a un caracter nulo.
end;

function IsEmpty(const AValue: ansinullstring): Boolean;
begin
  Result := uktansinullstrs.Length(AValue) = 0;
  // Goal: Returns if a string is empty.
  // Objetivo: Regresa si una cadena esta vacia.
end;

function IsStringOfChar(const S: ansinullstring; C: Char): Boolean;
var I, L: Integer; Match: Boolean;
begin
  L := uktansinullstrs.Length(S);

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

function Length(const AValue: ansinullstring): word;
begin
  Result := SysUtils.StrLen(pansichar(AValue));
  // Objetivo: Regresa la cantidad de caracteres de una cadena.
  // Goal: Returns the character ACount of a string.
end;

function LengthSize(const AValue: ansinullstring; const MaxSize: Integer): word;
var P: ansinullstring; ACount: Integer;
begin
  Result := 0;
  
  if (AValue <> nil) then
  begin
    P := AValue;
    ACount := 0;
    while ((P^ <> #0) and (ACount < MaxSize)) do
    begin	
	  Inc(P);
      Inc(ACount);
    end;
  end;  
end;

function UppercaseCopy(const AValue: ansinullstring): ansinullstring;
begin
  Result := DuplicateNullStr(AValue);
  UppercaseReplace(Result);
  // Goal: Returns a uppercase copy of the given null ansi string.
  // Objetivo: Regresa una copia en mayusculas de la cadena ansi nula dado.
end;

function LowercaseCopy(const AValue: ansinullstring): ansinullstring;
begin
  Result := DuplicateNullStr(AValue);
  LowercaseReplace(Result);
  // Goal: Returns a lowercase copy of the given null ansi string.
  // Objetivo: Regresa una copia en minusculas de la cadena ansi nula dado.
end;

function TogglecaseCopy(const AValue: ansinullstring): ansinullstring;
begin
  Result := DuplicateNullStr(AValue);
  ToggleCaseReplace(Result);
  // Goal: Returns a lowercase copy of the given null ansi string.
  // Objetivo: Regresa una copia en minusculas de la cadena ansi nula dado.
end;

function CapitalizeCopy(const AValue: ansinullstring): ansinullstring;
begin
  Result := DuplicateNullStr(AValue);
  CapitalizeReplace(Result);
  // Goal: Returns a lowercase copy of the given null ansi string.
  // Objetivo: Regresa una copia en minusculas de la cadena ansi nula dado.
end;

function SameChar(const AValue: Char; const ACount: Word): ansinullstring;
begin
  Result := AllocNullStr(ACount);
  System.FillChar(Result^, ACount, AValue);
  // Goal: Returns a null terminated string of the same char.
  // Objetivo: Regresa una cadena terminada en nulo del mismo caractaer.
end;

function StartsWith
 (const SubStr: ansinullstring; const S: ansinullstring): Boolean;
begin
  // @to-do:
  Result := false;
end;

function FinishesWith
 (const SubStr: ansinullstring; const S: ansinullstring): Boolean;
begin
  // @to-do:
  Result := false;
end;

function CharPos
  (const SubStr: char; const S: ansinullstring): ansinullstring;
begin
  Result := S;
  while (IsAssigned(Result) and (Result^ <> SubStr)) do
    Inc(Result);
  // Objetivo: Regresa el indice de la primer ocurrencia de "Substr".
  // Goal: Returns the AIndex of the first ocurrence of "Substr".
end;

function CharPosReverse
  (const SubStr: char; const S: ansinullstring): ansinullstring;
begin
  Result := uktansinullstrs.CharPos(ansinullchar, S);
  while ((Result <> S) and (Result^ <> SubStr)) do
    Dec(Result);
  // Objetivo: Regresa el indice de la primer ocurrencia de "Substr".
  // Goal: Returns the AIndex of the first ocurrence of "Substr".
end;

function Left(const AValue: ansinullstring; const ACount: Word): ansinullstring;
begin
  Result := AllocNullStr(ACount);
  MoveNullStr(AValue, Result, ACount);
  // Goal: Returns the leftmost characters of "AValue".
  // Objetivo: Regresa los caracteres mas a la izquierda de "AValue".
end;

function Right(const AValue: ansinullstring; const ACount: Word): ansinullstring;
var Source: ansinullstring;
begin
  Result := AllocNullStr(ACount);
  // obtain destination null terminated string
  // obtener cadena terminada en nulo destino

  Source := uktansinullstrs.CharPos(ansinullchar, AValue);
  System.Dec(Source, ACount);
  // obtain source null terminated string
  // obtener cadena terminada en nulo fuente

  MoveNullStr(Source, Result, ACount);
  // Goal: Returns the rightmost characters of "AValue".
  // Objetivo: Regresa los caracteres mas a la derecha de "AValue".
end;

procedure UppercaseReplace(const AValue: ansinullstring);
var P: pansichar;
begin
  P := AValue;
  while (IsAssigned(P)) do
  begin
    CharLowerBuff(P, 1);
    Inc(P);
  end;
//  CharUpper(@AValue);
  // Goal: Changes the given null terminated ansi string into uppercase.
  // Objetivo: Cambia la cadena ansi terminada en nulo dada a mayusculas.
end;

procedure LowercaseReplace(const AValue: ansinullstring);
var P: pansichar;
begin
  P := AValue;
  while (IsAssigned(P)) do
  begin
    CharLowerBuff(P, 1);
    Inc(P);
  end;
//  CharLower(@AValue);
  // Goal: Changes the given null terminated ansi string into lowercase.
  // Objetivo: Cambia la cadena ansi terminada en nulo dada a minusculas.
end;

procedure TogglecaseReplace(const AValue: ansinullstring);
var P: pansichar;
begin
  P := AValue;
  while (IsAssigned(P)) do
  begin
    if (IsCharLower(P^))
      then CharUpperBuff(P, 1)
      else CharLowerBuff(P, 1);
    Inc(P);
  end;
  // Goal: Swaps the sensitive case of each character, the given ansiarray.
  // Objetivo: Cambia el caso sensitivo de cada caracter en la cadena.
end;

procedure CapitalizeReplace(const AValue: ansinullstring);
var P: pansichar; MakeUppercase: Boolean;
begin
  P := AValue;
  MakeUppercase := TRUE;
  while (IsAssigned(P)) do
  begin
    if (P^ <> #32) then
    begin
      if (MakeUppercase) then
      begin
        CharUpperBuff(P, 1);
        MakeUppercase := FALSE;
      end else CharLowerBuff(P, 1);
    end else MakeUppercase := TRUE;
    Inc(P);
  end;
  // Goal: Changes the given null terminated ansi string into lowercase.
  // Objetivo: Cambia la cadena ansi terminada en nulo dada a minusculas.
end;

procedure MoveNullStr
 (const Source: ansinullstring; var Dest: ansinullstring; const ACount: Word);
begin
  System.Move(Source^, Dest^, ACount);
  // Goal: Copies "ACount" characters from a ansinullstring to another.
  // Objetivo: Copia "ACount" caracteres de una cadena terminada en nulo a otra.
end;

function CopyNullStr
 (const Source: ansinullstring; var Dest: ansinullstring): Word;
begin
  Result := uktansinullstrs.Length(Source);
  MoveNullStr(Source, Dest, Result);
  // Goal: Generate a copy of "Source" & returns character ACount.

  // Objetivo: Produce una copia de "Source" y
  // regresa la cantidad de caracteres.
end;

function DuplicateNullStr(const AValue: ansinullstring): ansinullstring;
var ACount: Word;
begin
  ACount := uktansinullstrs.Length(AValue);
  Result := uktansinullstrs.AllocNullStr(Succ(ACount));
  MoveNullStr(AValue, Result, ACount);
  // Goal: Returns a copy of "AValue".
  // Objetivo: Regresa una copia de "AValue".
end;

function ReverseNullStr(const AValue: ansinullstring): ansinullstring;
var I, ACount: Word; S, D: ansinullstring;
begin
  ACount := uktansinullstrs.Length(AValue);
  Result := uktansinullstrs.AllocNullStr(Succ(ACount));

  S := ScanNull(AValue); System.Dec(S);
  D := Result;  

  I := 0;
  while (IsAssigned(S) and (I <= ACount)) do
  begin
    D^:= S^;
    Inc(I);
    Dec(S);
    Inc(D);    
  end;
  // Goal: Returns a reversed copy of "AValue".
  // Objetivo: Regresa una copia al reves de "AValue".
end;

function AllocNullStr(const Size: Word): ansinullstring;
begin
  System.GetMem(Result, Succ(Size));
  // add 1 byte for the null char mark
  // agregar 1 byte para la marca de caracter nulo

  ClearNullStr(Result, Succ(Size));
  // fill with null chars
  // llenar con carateres nulos

  // Objetivo: Reserva de memoria una nueva cadena terminada en nulo
  // con el tamano indicado.

  // Goal: Reserves from memory a new null terminated string
  // with the given size.
end;

procedure FreeNullStr(var AValue: ansinullstring);
var SizeInBytes: Word;
begin
  SizeInBytes := SysUtils.StrLen(AValue);
  System.FreeMem(AValue, SizeInBytes);
  AValue := nil;
  // Objetivo: Liberar de la memoria una cadena constante terminada en nulo.
  // Goal: Release from memory a null terminated string constant.
end;

function StrToNullStr(const AValue: string): ansinullstring;
var SizeInBytes: Word;
begin
  SizeInBytes := System.Length(AValue);
  Result := AllocNullStr(Succ(SizeInBytes));
  System.Move(AValue[1], Result^, SizeInBytes);
  // Objetivo: Regresa una copia en memoria dinamica de una cadena pascal.
  // Goal: Returns a dynamic memory copy of a pascal string.
end;

function ClearNullStr(var AValue: ansinullstring; Size: Word): ansinullstring;
begin
  FillChar(AValue^, Size, #0);
  Result := AValue;
  // Goal: Clear a null terminated string.
  // Objetivo: Limpiar una cadena terminada en nulo.
end;

function IsAssigned(var AValue: ansinullstring): Boolean;
begin
  Result := (AValue^ <> ansinullchar);
  // Goal: Returns if a string pointer haven't reached the null marker.
  // Objetivo: Regresa si un apuntador a cadena no alcanzo el marcador nulo.
end;

function IsTerminated(var AValue: ansinullstring): Boolean;
begin
  Result := (AValue^ = ansinullchar);
  // Goal: Returns if a string pointer reached the null marker.
  // Objetivo: Regresa si un apuntador a cadena alcanzo el marcador nulo.
end;

procedure AssignEmpty(var AValue: ansinullstring);
begin
  AValue := nil;
  // Goal: An empty string is the same as "nil", null terminated strings.
  // Objetivo: Una cadena vacia es tratada como "nil"  en cadenas terminadas
  // en nulo.
end;

function Equal(const A, B: ansinullstring): Boolean;
begin
  Result := (SysUtils.StrComp(A, B) = 0);
  // Objetivo: Comparar 2 cadenas lexicograficamente.
  // Goal: Compare lexicographically 2 strings.
end;

function EqualForceMatch(const A, B: ansinullstring): Boolean;
var C, D: ansinullstring;
begin
  C := uktansinullstrs.UppercaseCopy(A);
  D := uktansinullstrs.UppercaseCopy(B);
  Result := (SysUtils.StrComp(C, D) = 0);
  FreeNullStr(C);
  FreeNullStr(D);
  // Goal: Checks if "A" & "B" nullstrings are equal,
  // both variables are changed to uppercase on the process.

  // Objetivo: Revisa si las cadenasnulas "A" y "B" son iguales,
  // ambas variables, son cambiadas a mayusculas, en el proceso.
end;

function EqualForceFirst(const A, B: ansinullstring): Boolean;
var C: ansinullstring;
begin
  C := uktansinullstrs.UppercaseCopy(A);
  Result := (SysUtils.StrComp(C, B) = 0);
  FreeNullStr(C);
  // Goal: Checks if "A" & "b" nullstrings are equal,
  // "A" is suppose to be, uppercase,
  // and a copy of "B" is changed to uppercase on the fly.

  // Objetivo: Revisa si las cadenasnulas "A" y "B" son iguales,
  // Se supone que "A" esta en mayusculas,
  // y una copia de "B" es cambiada a mayusculas en el proceso.
end;

function EqualForceLast(const A, B: ansinullstring): Boolean;
var C: ansinullstring;
begin
  C := uktansinullstrs.UppercaseCopy(B);
  Result := (SysUtils.StrComp(A, C) = 0);
  FreeNullStr(C);
  // Goal: Checks if "A" & "b" nullstrings are equal,
  // "A" is suppouse to be, uppercase,
  // and a copy of "B" is changed to uppercase on the fly.

  // Objetivo: Revisa si las cadenasnulas "A" y "B" son iguales,
  // Se supone que "A" esta en mayusculas,
  // y una copia de "B" es cambiada a mayusculas en el proceso.
end;

function EqualByOptions
  (const A, B: ansinullstring; Options: TStringOptions): Boolean;
begin
  case Options of
    soExactMatch: Result := Equal(A, B);
    soForceMatch: Result := EqualForceMatch(A, B);
    soForceFirst: Result := EqualForceFirst(A, B);
    soForceLast:  Result := EqualForceLast(A, B);
    else Result := FALSE;
  end;
  // Goal: Returns if 2 strings are equals uppon the given options.
  // Objetivo: Regresa si 2 cadenas son iguales basado en las opciones dadas.
end;

function Compare(const A, B: ansinullstring): TComparison;
begin
  Result := SysUtils.StrComp(A, B);
  Result := Math.Max(Result, -1); // "-3" -> "-1"
  Result := Math.Min(Result, +1); // "+3" -> "+1"
  // Goal: Compares 2 null terminated strings.
  // Objetivo: Compara 2 cadenas terminadas en nulo.
end;

function CompareForceMatch(const A, B: ansinullstring): TComparison;
var C, D: ansinullstring;
begin
  C := uktansinullstrs.UppercaseCopy(A);
  D := uktansinullstrs.UppercaseCopy(B);
  Result := SysUtils.StrComp(C, D);
  FreeNullStr(C);
  FreeNullStr(D);
  // Goal: Checks if "A" & "b" nullstrings are equal,
  // "A" is suppouse to be, uppercase,
  // and a copy of "B" is changed to uppercase on the fly.

  // Objetivo: Revisa si las cadenasnulas "A" y "B" son iguales,
  // Se supone que "A" esta en mayusculas,
  // y una copia de "B" es cambiada a mayusculas en el proceso.
end;

function CompareForceFirst(const A, B: ansinullstring): TComparison;
var C: ansinullstring;
begin
  C := uktansinullstrs.UppercaseCopy(A);
  Result := SysUtils.StrComp(C, B);
  FreeNullStr(C);
  // Goal: Checks if "A" & "b" nullstrings are equal,
  // "A" is suppouse to be, uppercase,
  // and a copy of "B" is changed to uppercase on the fly.

  // Objetivo: Revisa si las cadenasnulas "A" y "B" son iguales,
  // Se supone que "A" esta en mayusculas,
  // y una copia de "B" es cambiada a mayusculas en el proceso.
end;

function CompareForceLast(const A, B: ansinullstring): TComparison;
var C: ansinullstring;
begin
  C := uktansinullstrs.UppercaseCopy(B);
  Result := SysUtils.StrComp(A, C);
  FreeNullStr(C);
  // Goal: Checks if "A" & "b" nullstrings are equal,
  // "A" is suppouse to be, uppercase,
  // and a copy of "B" is changed to uppercase on the fly.

  // Objetivo: Revisa si las cadenasnulas "A" y "B" son iguales,
  // Se supone que "A" esta en mayusculas,
  // y una copia de "B" es cambiada a mayusculas en el proceso.
end;

function CompareByOptions
  (const A, B: ansinullstring; Options: TStringOptions): TComparison;
begin
  case Options of
    soExactMatch: Result := Compare(A, B);
    soForceMatch: Result := CompareForceMatch(A, B);
    soForceFirst: Result := CompareForceFirst(A, B);
    soForceLast:  Result := CompareForceLast(A, B);
    else Result := 0;
  end;
  // Goal: Returns if 2 strings are equals uppon the given options.
  // Objetivo: Regresa si 2 cadenas son iguales basado en las opciones dadas.
end;

function StrAsNullStr(var AValue: shortstring): ansinullstring;
var Len: Byte;
begin
  Len := Succ(System.Length(AValue));
  AValue[Len] := #0;
  Result := @(AValue[1]);
  // Goal: force a string to act as a null terminated string
  // by adding a trailing hidden null char.

  // Objetivo: forzar a una cadena a actuar como una cadena terminada en nulo
  // agregando un caracter nulo oculto al final.
end;

function MemoAsNullStr(var AValue: ansimemo): ansinullstring;
var MemoRec: PansimemoRec absolute AValue; AIndex: Word;
begin
  AIndex := Succ(MemoRec^.Header.Len);
  MemoRec^.Items[AIndex] := ansinullchar;
  Result := uktansimemos.PtrOfMemo(MemoRec);
  // Goal: force a ansimemo to act as a null terminated string
  // by adding a trailing hidden null char.

  // Objetivo: forzar a un ansimemo a actuar como una cadena terminada en nulo
  // agregando un caracter nulo oculto al final.
end;

procedure AssignNullStr(var Dest: ansinullstring; const AValue: ansinullstring);
var Size: Word;
begin
  if (Assigned(Dest))
    then FreeNullStr(Dest);

  Size := Succ(Length(AValue));
  Dest := AllocNullStr(Size);
  System.Move(AValue^, Dest^, Size);
  // Objetivo: Reemplazar el contenido de una cadena terminada en nulo por
  // otra cadena terminada en nulo.
  // Goal: Replace a null terminated string for another null terminated string.
end;

procedure AssignRange(var Dest: ansinullstring; const Min, Max: Char);
var Size: Word; S: ansinullstring; C: Char;
begin {RangeToNull}
  if (Assigned(Dest))
    then FreeNullStr(Dest);

  Size := Succ(Ord(Min) - Ord(Max));
  Dest := AllocNullStr(Size);

  S := Dest;
  for C := Min to Max do
  begin
    S^ := C;
    Inc(S);
  end;
  // Objetivo: Reemplazar el contenido de una cadena terminada en nulo por
  // un lista consecutiva de caracteres.
  // Goal: Replace a null terminated string for a consecutive character list.
end;

function ScanNull(const AValue: ansinullstring): ansinullstring;
begin
  Result := SysUtils.StrEnd(AValue);
  // Objetivo: Regresa un apuntador al marcador de cadena nula.
  // Goal: Returns a pointer to the null string marker.
end;

function Scan(const Str, SubStr: ansinullstring): ansinullstring;
begin
  Result := SysUtils.StrPos(Str, SubStr);
  // Objetivo: Regresa la 1ra. ocurrencia de "SubStr" en "Str".
  // Goal: Returns the first ocurrence of "SubStr", "Str".
end;

function Scan(const Str: ansinullstring; const SubStr: Char): ansinullstring;
var P: ansinullstring; S: shortstring;
begin
  S := SubStr;
  P := StrAsNullStr(S);
  Result := SysUtils.StrPos(Str, P);
  // Objetivo: Regresa la 1ra. ocurrencia de "SubStr" en "Str".
  // Goal: Returns the first ocurrence of "SubStr", "Str".
end;

function Scan(const Str: ansinullstring; const SubStr: string): ansinullstring;
var P: ansinullstring; S: shortstring;
begin
  S := SubStr;
  P := StrAsNullStr(S);
  Result := SysUtils.StrPos(Str, P);
  // Objetivo: Regresa la 1ra. ocurrencia de "SubStr" en "Str".
  // Goal: Returns the first ocurrence of "SubStr", "Str".
end;

function ScanSet(const Str: ansinullstring; const SubStr: ansicharset): ansinullstring;
begin
  Result := nil;
  // Objetivo: Regresa la 1ra. ocurrencia de "SubStr" en "Str".
  // Goal: Returns the first ocurrence of "SubStr", "Str".
end;

function ReverseScan(const Str, SubStr: ansinullstring): ansinullstring;
begin
  Result := SysUtils.StrPos(Str, SubStr);
  // Objetivo: Regresa la 1ra. ocurrencia de "SubStr" en "Str".
  // Goal: Returns the first ocurrence of "SubStr", "Str".
end;

function ReverseScan(const Str: ansinullstring; const SubStr: Char): ansinullstring;
var S: shortstring;
begin
  S := SubStr;
  Result := ScanNull(Str);
  while ((Result <> Str) and (Result^ <> SubStr)) do
    Dec(Result);

  if (Result^ <> SubStr)
    then Result := nil;
  // Objetivo: Regresa la 1ra. ocurrencia de "SubStr" en "Str".
  // Goal: Returns the first ocurrence of "SubStr", "Str".
end;

function ReverseScan(const Str: ansinullstring; const SubStr: string): ansinullstring;
var P: ansinullstring; S: shortstring;
begin
  S := SubStr;
  P := StrAsNullStr(S);
  Result := SysUtils.StrPos(Str, P);
  // Objetivo: Regresa la 1ra. ocurrencia de "SubStr" en "Str".
  // Goal: Returns the first ocurrence of "SubStr", "Str".
end;

function ReverseScanSet(const Str: ansinullstring; const SubStr: ansicharset): ansinullstring;
begin
  Result := nil;
  // Objetivo: Regresa la 1ra. ocurrencia de "SubStr" en "Str".
  // Goal: Returns the first ocurrence of "SubStr", "Str".
end;

function MatchWhile
  ({ref} AValue: ansinullstring; const ValidChars: ansicharset): ansinullstring;
var CanContinue: Boolean;
begin
  CanContinue := TRUE;
  while (IsAssigned(AValue) and CanContinue) do
  begin
    CanContinue := IsMember(AValue^, ValidChars);
    Inc(AValue);
  end;

  if (not CanContinue) then
  begin
    Dec(AValue);
    Result := AValue;
  end else Result := nil
  // Goal: Returns if all the characters, a string are valid.
  // Objetivo: Regresa si todos los caracteres en una cadena son validos.
end;

function StringInSet
  ({ref} AValue: ansinullstring; const ValidChars: ansicharset): Boolean;
begin
  Result := (MatchWhile(AValue, ValidChars) = nil);
  // Goal: Returns if all the characters, a string are valid.
  // Objetivo: Regresa si todos los caracteres en una cadena son validos.
end;

function ReplaceChar
  ({ref} AValue: ansinullstring; A, B: Char): ansinullstring;
begin
  Result := AValue;
  while (IsAssigned(AValue)) do
  begin
    if (AValue^ = A)
      then AValue^ := B;
    Inc(AValue);
  end;
  // Goal: Replace a specific character  from a string.
  // Objetivo: Reemplazar un caracter en especifico de una cadena.
end;

procedure SkipCharWhile(var AValue: ansinullstring; ValidChar: Char);
begin
  while (IsAssigned(AValue) and (AValue^ = ValidChar)) do
    Inc(AValue);
  // Goal: Returns a group of characters.
  // Objetivo: Regresa un grupo de caracteres.
end;

procedure SkipCharUntil(var AValue: ansinullstring; BreakChar: Char);
begin
  while (IsAssigned(AValue) and (AValue^ <> BreakChar)) do
    Inc(AValue);
  // Goal: Returns a group of characters.
  // Objetivo: Regresa un grupo de caracteres.
end;

procedure SkipCharWhileCount
 (var AValue: ansinullstring; ValidChar: Char; const ACount: Word);
var I: Word;
begin
  I := 0;
  while (IsAssigned(AValue) and (AValue^ = ValidChar) and (I <= ACount)) do
  begin
    Inc(I);
    Inc(AValue);
  end;
  // Goal: Returns a group of non-space characters.
  // Objetivo: Regresa un grupo de caracteres que no son espacios.
end;

procedure SkipWhile(var AValue: ansinullstring; ValidChars: ansicharset);
begin
  while (IsAssigned(AValue) and IsMember(AValue^, ValidChars)) do
    Inc(AValue);
  // Goal: Returns a group of non-space characters.
  // Objetivo: Regresa un grupo de caracteres que no son espacios.
end;

procedure SkipUntil(var AValue: ansinullstring; BreakChars: ansicharset);
begin
  while (IsAssigned(AValue) and not IsMember(AValue^, BreakChars)) do
   Inc(AValue);
  // Goal: Returns a group of non-space characters.
  // Objetivo: Regresa un grupo de caracteres que no son espacios.
end;

function NullStrToChar(const AValue: ansinullstring): char;
begin
  Result := AValue^;
  // Objetivo: Regresa un caracter a partir de una cadena nula.
  // Goal: Returns a character from a null string.
end;

function NullStrToStr(const AValue: ansinullstring): string;
begin
  System.SetLength(Result, StrLen(AValue));
  Result := AValue;
  // Objetivo: Regresa una cadena pascal de copia de una cadena nula.
  // Goal: Returns a pascal string copy of a null string.
end;

function NullStrToDate(const AValue: ansinullstring): TDateTime;
begin
  Result := StrToDate(NullStrToStr(AValue));
  // Objetivo: Regresa una fecha a partir de una cadena terminada en nulo.
  // Goal: Returns a date from a null terminated string.
end;

function NullStrToTime(const AValue: ansinullstring): TDateTime;
begin
  Result := StrToTime(NullStrToStr(AValue));
  // Objetivo: Regresa una hora a partir de una cadena terminada en nulo.
  // Goal: Returns a time from a null terminated string.
end;

function NullStrToInt(const AValue: ansinullstring): Integer;
begin
  Result := StrToIntDef(NullStrToStr(AValue), 0);
  // Objetivo: Regresa un entero a partir de una cadena terminada en nulo.
  // Goal: Returns an integer from a null terminated string.
end;

function NullStrToFloat(const AValue: ansinullstring): TDBFloat;
begin
  Result := StrToFloat(NullStrToStr(AValue));
  // Objetivo: Regresa un numero de punto flotante
  // a partir de una cadena terminada en nulo.

  // Goal: Returns a floating decimal number
  // from a null terminated string.
end;

function NullStrToCurr(const AValue: ansinullstring): Currency;
begin
  Result := StrToCurr(NullStrToStr(AValue));
  // Objetivo: Regresa un valor moneda
  // a partir de una cadena terminada en nulo.
  // Goal: Returns a currency AValue from a null terminated string.
end;

function AssignStr(var Dest: ansinullstring; const AValue: string): ansinullstring;
begin
//if Assigned(Dest)
//  then FreeNullStr(Dest);
  Dest := StrToNullStr(AValue);
  Result := Dest;
  // Objetivo: Reemplazar el contenido de una cadena terminada en nulo por
  // una cadena pascal.
  // Goal: Replace a null terminated string for a pascal string.
end;

function AssignDate(var Dest: ansinullstring; const AValue: TDateTime): ansinullstring;
begin
  if (Assigned(Dest))
    then FreeNullStr(Dest);
  Dest := StrToNullStr(DateToStr(AValue));
  Result := Dest;
  // Objetivo: Reemplazar el contenido de una cadena terminada en nulo por
  // una fecha.
  // Goal: Replace a null terminated string for a date.
end;

function AssignTime(var Dest: ansinullstring; const AValue: TDateTime): ansinullstring;
begin {TimeToNull}
  if (Assigned(Dest))
    then FreeNullStr(Dest);
  Dest := StrToNullStr(TimeToStr(AValue));
  Result := Dest;
  // Objetivo: Reemplazar el contenido de una cadena terminada en nulo por
  // una fecha.
  // Goal: Replace a null terminated string for a date.
end;

function AssignInt(var Dest: ansinullstring; const AValue: Integer): ansinullstring;
begin {IntToNull}
  if (Assigned(Dest))
    then FreeNullStr(Dest);
  Dest := StrToNullStr(IntToStr(AValue));
  Result := Dest;
  // Objetivo: Reemplazar el contenido de una cadena terminada en nulo por
  // un entero.
  // Goal: Replace a null terminated string for an integer.
end;

function AssignFloat(var Dest: ansinullstring; const AValue: TDBFloat): ansinullstring;
begin {FloatToNull}
  if (Assigned(Dest))
    then FreeNullStr(Dest);
  Dest := StrToNullStr(FloatToStr(AValue));
  Result := Dest;
  // Objetivo: Reemplazar el contenido de una cadena terminada en nulo por
  // un numero de punto flotante.
  // Goal: Replace a null terminated string for an floating decimal number.
end;

function AssignCurr(var Dest: ansinullstring; const AValue: Currency): ansinullstring;
begin {CurrToNull}
  if (Assigned(Dest))
    then FreeNullStr(Dest);
  Dest := StrToNullStr(CurrToStr(AValue));
  Result := Dest;
  // Objetivo: Reemplazar el contenido de una cadena terminada en nulo por
  // un numero de punto flotante.
  // Goal: Replace a null terminated string for an floating decimal number.
end;

function CopyToStr(var AValue: ansinullstring; const ACount: Word): string;
var I: Word;
begin
  I := 0; Result := '';
  while (IsAssigned(AValue) and (I < ACount)) do
  begin
    Result := Result + AValue^;
    Inc(I);
    Inc(AValue);
  end;
  // Objetivo: Returns "ACount" charactes.
  // Goal: Regresa "ACount" caracteres.
end;

function SkipChar(var AValue: ansinullstring): char;
begin
  Result := ansinullchar;
  if (IsAssigned(AValue)) then
  begin
    Result := AValue[0];
    Inc(AValue);
  end;
  // Objetivo: Regresa el 1er. caracter.
  // Goal: Returns the first character.
end;

procedure SkipBlanks(var AValue: ansinullstring);
begin
  SkipWhile(AValue, BlanksSet);
  // Objetivo: Regresa el 1er. grupo de no-espacios encontrado.
  // Goal: Returns the first non-blanks found.
end;

function ExtractCharWhile
 (var AValue: ansinullstring; ValidChar: Char): string;
begin
  Result := '';
  while (IsAssigned(AValue) and (AValue^ = ValidChar)) do
  begin
    Result := Result + ValidChar;
    Inc(AValue);
  end
  // Objetivo: Regresa una cadena cuyos caracteres coincidan con el
  // caracter indicado.
  // Goal: Returns a string which characters match the given char.
end;

function ExtractCharUntil
 (var AValue: ansinullstring; BreakChar: Char): string;
begin
  Result := '';
  while (IsAssigned(AValue) and (AValue^ <> BreakChar)) do
  begin
    Result := Result + AValue^;
    Inc(AValue);
  end;
  // Objetivo: Regresa una cadena cuyos caracteres no coincidan con el
  // caracter indicado.
  // Goal: Returns a string which characters doesn't match the given char.
end;

function ExtractWhile
 (var AValue: ansinullstring; ValidChars: ansicharset): string;
begin
  Result := '';
  while (IsAssigned(AValue) and IsMember(AValue^, ValidChars)) do
  begin
    Result := Result + AValue^;
    Inc(AValue);
  end;
  // Objetivo: Regresa una cadena cuyos caracteres coincidan con el
  // conjunto indicado.
  // Goal: Returns a string which characters match the given set.
end;

function ExtractUntil
 (var AValue: ansinullstring; BreakChars: ansicharset): string;
begin
  Result := '';
  while (IsAssigned(AValue) and not IsMember(AValue^, BreakChars)) do
  begin
    Result := Result + AValue^;
    Inc(AValue);
  end;
  // Objetivo: Regresa una cadena cuyos caracteres no coincidan con el
  // conjunto indicado.
  // Goal: Returns a string which characters doesn't match the given set.
end;

function ParseWhile
  (const AValue: ansinullstring; ValidChars: ansicharset): string;
var P: ansinullstring;
begin
  P := AValue; uktansinullstrs.SkipWhile(P, ValidChars);
  Result := uktansinullstrs.ExtractWhile(P, ValidChars);
  // Objetivo: Busca una cadena que contenga los caracteres indicados.
  // Goal: Seraches for a string that contains the given characters.
end;

function ExtractToken(var AValue: ansinullstring): string;
begin
  SkipBlanks(AValue);
  Result := ExtractUntil(AValue, BlanksSet);
  // Objetivo: Regresa la 1era. subcadena de no-espacios encontrado.
  // Goal: Returns the first non-blanks substring found.
end;

function ExtractDelimiter
 (var AValue: ansinullstring; const Delimiter: char): string;
begin
  SkipBlanks(AValue);
  // Skip leading spaces
  // Saltar espacios iniciales

  Result := '';
  while (IsAssigned(AValue) and
        (AValue^ <> Delimiter) and not IsMember(AValue^, BlanksSet)) do
  begin
    Result := Result + AValue^;
    Inc(AValue);
  end;
  // locate delimiter or space
  // localizar delimitador o espacio

  SkipBlanks(AValue);
  // Skip trailing spaces
  // Saltar espacios finales

  if (AValue^ = Delimiter)
    then SkipChar(AValue);
  // Skip delimiter
  // Saltar delimitador

  // Objetivo: Regresa la 1era. subcadena de no-espacios encontrado.
  // Goal: Returns the first non-blanks substring found.
end;

function ExtractInt(var AValue: ansinullstring): Integer;
begin
  SkipBlanks(AValue);
  Result := StrToIntDef(ExtractWhile(AValue, DigitSet), 0);
  // Objetivo: Regresa la 1era. subcadena de digitos encontrado.
  // Goal: Returns the first digits substring found.
end;

function ExtractAlpha(var AValue: ansinullstring): string;
begin
  SkipBlanks(AValue);
  Result := ExtractWhile(AValue, AlphaSet);
  // Objetivo: Regresa la 1era. subcadena de letras encontrado.
  // Goal: Returns the first letters substrings found.
end;

function ExtractID(var AValue: ansinullstring): string;
begin
  SkipBlanks(AValue);
  Result := ExtractWhile(AValue, IDSet);
  // Objetivo: Regresa la 1era. subcadena de letras o digitos encontrado.
  // Goal: Returns the first letters or digits substrings found.
end;

function ExtractChar(var AValue: ansinullstring): Char;
begin
  SkipBlanks(AValue);
  Result := SkipChar(AValue);
  // Objetivo: Regresa el 1er. caracter de la cadena dada.
  // Goal: Returns the first character from the given string.
end;

function ExtractCount(var AValue: ansinullstring; const ACount: Word): string;
var I: Word;
begin
  Result := ''; I := 0;
  while (IsAssigned(AValue) and (I <= ACount)) do
  begin
    Result := Result + AValue^;
    Inc(AValue);
    Inc(I);
  end;
  // Objetivo: Regresar "ACount" caracteres de "AValue" saltando espacios.
  // Goal: Returns "ACount" characters from "AValue" skipping spaces.
end;

(*
function ExtractCount(var AValue: ansinullstring; const ACount: Word): string;
var I: Word; CanContinue: Boolean;
begin
  Result := ''; I := 0;

  CanContinue := (AValue^ = #32);
  if (CanContinue) then
  begin
    repeat
      //CanContinue := FALSE;
      Inc(AValue);
      Inc(I);
    until IsTerminated(AValue) or (I <= ACount) or (AValue^ <> #32);
    //CanContinue := (I <= ACount);
  end;

  while (IsAssigned(AValue) and (I <= ACount) and (AValue^ <> #32)) do
  begin
    Result := Result + AValue^;
    Inc(AValue);
    Inc(I);
  end;
  // Objetivo: Regresar "ACount" caracteres de "AValue" saltando espacios.
  // Goal: Returns "ACount" characters from "AValue" skipping spaces.
end;
*)

function ExtractSQuoted(var AValue: ansinullstring): string;
begin
  SkipBlanks(AValue);
  SkipCharWhile(AValue, ansiSingleQuote);
  Result := ExtractCharUntil(AValue, ansiSingleQuote);
  SkipCharWhile(AValue, ansiSingleQuote);
  // Objetivo: Regresa una cadena limitada por comillas simples.
  // Goal: Returns a string delimited by single quotes.
end;

function ExtractDQuoted(var AValue: ansinullstring): string;
begin
  SkipBlanks(AValue);
  SkipCharWhile(AValue, ansiDoubleQuote);
  Result := ExtractCharUntil(AValue, ansiDoubleQuote);
  SkipCharWhile(AValue, ansiDoubleQuote);
  // Objetivo: Regresa una cadena limitada por comillas dobles.
  // Goal: Returns a string delimited by double quotes.
end;

function ExtractTrimSQuoted(var AValue: ansinullstring): string;
begin
  SkipBlanks(AValue);
  SkipCharWhile(AValue, ansiSingleQuote);
  Result := ExtractCharUntil(AValue, ansiSingleQuote);
  // Objetivo: Regresa una cadena sin las comillas simples.
  // Goal: Returns a string without single quotes.
end;

function ExtractTrimDQuoted(var AValue: ansinullstring): string;
var PrevAValue: ansinullstring;
begin
  PrevAValue := AValue;
  SkipBlanks(AValue);

  if (AValue^ = ansiDoubleQuote) then
  begin
    SkipCharWhile(AValue, ansiDoubleQuote);
    Result := ExtractCharUntil(AValue, ansiDoubleQuote);

    if (AValue^ <> ansiDoubleQuote) then
    begin
      Result := '';
      AValue  := PrevAValue;
    end else SkipCharWhile(AValue, ansiDoubleQuote)
  end else
  begin
    Result := '';
    AValue  := PrevAValue;
  end;
  // Objetivo: Regresa una cadena sin comillas dobles.
  // Goal: Returns a string without double quotes.
end;

function IsWildcard({ref} AValue: ansinullstring): Boolean;
begin
  Result := StringInSet(AValue, WildcardSet);
  // Goal: Returns if a string is a wildcard.
  // Objetivo: Regresa si una cadena es un comodin.
end;

function ExtractWildcard(var AValue: ansinullstring): string;
begin
  SkipBlanks(AValue);
  Result := ExtractWhile(AValue, WildcardSet);
  // Objetivo: Regresa la 1era. subcadena de letras o digitos encontrado.
  // Goal: Returns the first letters or digits substrings found.
end;

function ExtractWhileMatch(var AValue: ansinullstring; const S: string): string;
var AIndex, Len: Word;
begin
  Result := '';
  SkipBlanks(AValue);

  AIndex := 1; Len := System.Length(S);
  while (IsAssigned(AValue) and (AValue^ = S[AIndex]) and (AIndex <= Len)) do
  begin
    Result := Result + AValue^;
    Inc(AIndex);
    Inc(AValue);
  end;
  // Objetivo: Revisa que coincidan ambas cadenas dada.
  // Goal: Checks that both strings matches.
end;

procedure ReadLn(var T: TextFile; var AValue: ansinullstring);
var S: string;
begin
  System.ReadLn(T, S);
  AValue := StrToNullStr(S);
  // Objetivo: Leer una cadena de un archivo de texto, debe liberarse despues.
  // Goal: Read a string from a textfile, must be released later.
end;

end.
