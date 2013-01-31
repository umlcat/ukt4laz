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

unit uktansimemos;

(* Espanol *)

(**
 ** "ansimemo" es un tipo primitivo que permite el uso de
 ** cadenas de longuitud variable sin el uso de referencias.

 ** su uso es similar al string standard de pascal,
 ** se maneja como un arreglo de caracteres,
 ** con un campo reservado para la cantidad actual,
 ** pero soporta una cantidad mayor que los 256 bytes.

 ** Reemplaza al "infame" tipo string de delphi
 ** que reserva y libera memoria arbitrariamente.
 *)

(* English *)

(**
 ** "ansimemo" is a primitive type that allows the use of
 ** variable length strings, without the use of references.

 ** Replaces the "infamous" delphi string type
 ** that allocs & release memory arbitrary.

 ** Its used, similar to the standard pascal string,
 ** using a character array,
 ** with a reserved field for the current quantity,
 ** but, supports more than 256 bytes.
 *)

interface
uses
{$IFDEF MSWINDOWS}
  Windows, Messages,
{$ENDIF}
{$IFDEF LINUX}
  Types, Libc,
{$ENDIF}
  SysUtils, Math,
  uktcomparisons,
  uktansichars,
  uktansicharsets, uktansicharsetconsts,
  dummy;

type

{ global types }

   ansimemo = type pointer;

   TansimemoArray = array[0 .. 65535] of ansichar;
   PansimemoArray = ^TansimemoArray;
   // Used for access individual characters
   // Utilizado para accesar caracteres individuales

   TansimemoHeader = record
     Len:    Word;
     MaxLen: Word;
   end;
   // ansimemo's fixed type (header)
   // Tipo fijo del ansimemo (encabezado)

   TansimemoRec = record
     Header: TansimemoHeader;
     Items:  TansimemoArray;
   end;
   PansimemoRec = ^TansimemoRec;
   // ansimemo's variable length type
   // Tipo de longuitud variable del ansimemo

{ global properties }

  function getQuotedUppercase
   (const AValue: ansimemo;
    const LeftQuote, RightQuote: ansichar): ansimemo; overload;
  function getQuotedLowercase
   (const AValue: ansimemo;
    const LeftQuote, RightQuote: ansichar): ansimemo; overload;

  function getTrimLeft(const AValue: ansimemo): ansimemo; overload;
  function getTrimRight(const AValue: ansimemo): ansimemo; overload;
  function getTrim(const AValue: ansimemo): ansimemo; overload;

  function getUnTrimLeft
   (const AValue: ansimemo; ACount: Integer): ansimemo; overload;
  function getUnTrimRight
   (const AValue: ansimemo; ACount: Integer): ansimemo; overload;

  procedure setQuotedUppercase
   (const AValue: ansimemo; const LeftQuote, RightQuote: ansichar); overload;
  procedure setQuotedLowercase
   (const AValue: ansimemo; const LeftQuote, RightQuote: ansichar); overload;

  procedure setTrimLeft(var AValue: ansimemo); overload;
  procedure setTrimRight(var AValue: ansimemo); overload;
  procedure setTrim(var AValue: ansimemo); overload;

  procedure setUnTrimLeft(var AValue: ansimemo; ACount: Integer); overload;
  procedure setUnTrimRight(var AValue: ansimemo; ACount: Integer); overload;

{ global functions }

  function Length(const AValue: ansimemo): Word;
  function MaxLength(const AValue: ansimemo): Word;

  function SameText(const A, B: ansimemo): Boolean;

  function UppercaseCopy(const AValue: ansimemo): ansimemo; overload;
  function LowercaseCopy(const AValue: ansimemo): ansimemo; overload;
  function TogglecaseCopy(const AValue: ansimemo): ansimemo; overload;
  function CapitalizeCopy(const AValue: ansimemo): ansimemo; overload;

  function IsEmpty(const AValue: ansimemo): Boolean; overload;
  function IsStringOfChar(const S: ansimemo; C: ansichar): Boolean;

  function SameChar(const AValue: ansichar; const ACount: Word): ansimemo;

  function CharPos
    (const SubStr: ansichar; const S: ansimemo): Word;
  function CharPosReverse
    (const SubStr: ansichar; const S: ansimemo): Word;

  function Pos(const SubStr: ansistring; const S: ansimemo): Word; overload;
  function Pos(const SubStr: ansimemo; const S: ansimemo): Word; overload;

  function StartsWith
    (const SubStr: ansimemo; const S: ansimemo): Boolean; overload;
  function FinishesWith
    (const SubStr: ansimemo; const S: ansimemo): Boolean; overload;

  function StartsWith
    (const SubStr: ansistring; const S: ansimemo): Boolean; overload;
  function FinishesWith
    (const SubStr: ansistring; const S: ansimemo): Boolean; overload;

  function Left(const AValue: ansimemo; const ACount: Word): ansimemo;
  function Right(const AValue: ansimemo; const ACount: Word): ansimemo;

  procedure Empty(var AValue: ansimemo); overload;

  procedure Copy
     (const Source: ansimemo; AIndex, ACount: Word; var Dest: ansistring); overload;
  procedure Copy
     (const Source: ansimemo; AIndex, ACount: Word; var Dest: ansimemo); overload;

  function SubStr
     (const PartialStr, FullStr: ansimemo; var AIndex: Integer): Boolean;
  function IsIdentifier(const AValue: ansimemo): Boolean;
  function RangeToStr(const Min, Max: ansichar): ansimemo;

  function ReplaceChar
    (const AValue: ansimemo; A, B: ansichar): ansimemo; overload;
//  function ReplaceChars
//    (const AValue: ansimemo; Source: ansicharset; Dest: ansichar): ansimemo; overload;

  function ReplaceStrALL
     (var FullStr: ansimemo; const SubStr, AValue: ansistring): Integer;
  function ReplaceStr
     (var FullStr: ansimemo; const SubStr, AValue: ansistring): Integer;

  function ReplaceCopy(const FullStr, SubStr, AValue: ansistring): ansimemo;
  function DeleteCopy(const FullStr, SubStr: ansistring): ansimemo;

  procedure UppercaseReplace(var AValue: ansimemo); overload;
  procedure LowercaseReplace(var AValue: ansimemo); overload;
  procedure TogglecaseReplace(var AValue: ansimemo); overload;
  procedure CapitalizeReplace(var AValue: ansimemo); overload;

  procedure Clear(var AValue: ansimemo);

  function StrToMemo(const AValue: ansistring): ansimemo;
  function MemoToStr(const AValue: ansimemo): ansistring;

  procedure AssignStr(var Dest: ansimemo; const Source: ansistring);
  procedure AssignMemo(var Dest: ansimemo; const Source: ansimemo);

  function getAt(const List: ansimemo; const AIndex: Word): ansichar;
  procedure setAt(const List: ansimemo; const AIndex: Word; AValue: ansichar);

  function PtrOfMemo(const AValue: ansimemo): pointer;

  procedure Append(var Dest: ansimemo; const Source: ansichar);
  procedure InsertAt(var Dest: ansimemo; AIndex: Word; AValue: ansichar);
  procedure DeleteAt(var Dest: ansimemo; AIndex: Word);

  procedure Concat(var Dest: ansimemo; const Source: ansistring); overload;
  procedure Concat(var Dest: ansimemo; const Source: ansimemo); overload;

  procedure Insert
    (const Source: ansistring; var S: ansimemo; AIndex: Word); overload;
  procedure Insert
    (const Source: ansimemo; var S: ansimemo; AIndex: Word); overload;
  procedure Delete(var S: ansimemo; AIndex, ACount: Word);

  { = } function Equal(const A, B: ansimemo): Boolean;
  { > } function Greater(const A, B: ansimemo): Boolean;
  { < } function Lesser(const A, B: ansimemo): Boolean;

  function Compare(const A, B: ansimemo): TComparison;

  procedure AllocMemo(var AValue: ansimemo; const ACount: Word);
  procedure FreeMemo(var AValue: ansimemo);

  function DuplicateMemo(const AValue: ansimemo): ansimemo;
  procedure ReAllocMemo(var AValue: ansimemo; const ACount: Word);

implementation

{ global properties }

function getQuotedUppercase
  (const AValue: ansimemo; const LeftQuote, RightQuote: ansichar): ansimemo;
begin
  Result := DuplicateMemo(AValue);
 uktansimemos.setQuotedUppercase(Result, LeftQuote, RightQuote);
  // Goal: Returns a uppercase copy of the given ansimemo,
  // without modifying delimited subansimemos.

  // Objetivo: Regresa una copia en mayusculas del ansimemo dado,
  // sin modificar a los subansimemos delimitados.
end;

function getQuotedLowercase
  (const AValue: ansimemo; const LeftQuote, RightQuote: ansichar): ansimemo;
begin
  Result := DuplicateMemo(AValue);
 uktansimemos.setQuotedLowercase(Result, LeftQuote, RightQuote);
  // Goal: Returns a lowercase copy of the given ansimemo,
  // without modifying delimited substrings.

  // Objetivo: Regresa una copia en minusculas deansimemo dado,
  // sin modificar a los subansimemos delimitados.
end;

function getTrimLeft(const AValue: ansimemo): ansimemo;
begin
  Result := nil;
  // Goal: Returns a copy of the given ansimemo without leading spaces.
  // Objetivo: Regresa una copia de la cadena dada sin espacios iniciales.
end;

function getTrimRight(const AValue: ansimemo): ansimemo;
begin
  Result := nil;
  // Goal: Returns a copy of the given ansimemo without trailing spaces.
  // Objetivo: Regresa una copia de la cadena dada sin espacios finales.
end;

function getTrim(const AValue: ansimemo): ansimemo;
begin
  Result := nil;
  // Goal: Returns a copy of the given ansimemo
  // without leading & trailing spaces.

  // Objetivo: Regresa una copia de la cadena dada
  // sin espacios iniciales y finales.
end;

function getUnTrimLeft(const AValue: ansimemo; ACount: Integer): ansimemo;
begin
  Result := nil;
  // Goal: Returns a copy of the given ansimemo plus leading spaces.
  // Objetivo: Regresa una copia de la cadena dada mas espacios iniciales.
end;

function getUnTrimRight(const AValue: ansimemo; ACount: Integer): ansimemo;
begin
  Result := nil;
  // Goal: Returns a copy of the given ansimemo plus trailing spaces.
  // Objetivo: Regresa una copia de la cadena dada mas espacios finales.
end;

procedure setQuotedUppercase
  (const AValue: ansimemo; const LeftQuote, RightQuote: ansichar);
var InsideString: Boolean; P: pansichar;
begin
  InsideString := FALSE; P := PtrOfMemo(AValue);
  while not (P^ = #0) do
  begin
    if (InsideString)
      then InsideString := not (P^= RightQuote)
      else InsideString := (P^= LeftQuote);
    // initial or final delimiter found ?
    // delimitador inicial o final encontrado ?

    if (not InsideString)
      then uktansichars.UppercaseReplace(P^);
    // replace characters
    // reemplazar caracteres

    Inc(P);
  end;
  // Goal: Changes the given ansimemo into uppercase,
  // without modifying delimited subansimemos.

  // Objetivo: Cambia el ansimemo dado a mayusculas.
  // sin modificar a los subansimemos delimitados.
end;

procedure setQuotedLowercase
  (const AValue: ansimemo; const LeftQuote, RightQuote: ansichar); overload;
var InsideString: Boolean; P: pansichar;
begin
  InsideString := FALSE; P := PtrOfMemo(AValue);
  while not (P^ = #0) do
  begin
    if (InsideString)
      then InsideString := not (P^= RightQuote)
      else InsideString := (P^= LeftQuote);
    // initial or final delimiter found ?
    // delimitador inicial o final encontrado ?

    if (not InsideString)
      then uktansichars.LowercaseReplace(P^);
    // replace characters
    // reemplazar caracteres

    Inc(P);
  end;
  // Goal: Changes the given ansimemo into lowercase,
  // without modifying delimited subansimemos.

  // Objetivo: Cambia el ansimemo dado a minusculas.
  // sin modificar a los subansimemos delimitados.
end;

procedure setTrimLeft(var AValue: ansimemo);
begin
//  AValue := SysUtils.TrimLeft(AValue);
  // Goal: Returns the given ansimemo without leading spaces.
  // Objetivo: Regresa la cadena dada sin espacios iniciales.
end;

procedure setTrimRight(var AValue: ansimemo);
begin
//  AValue := SysUtils.TrimRight(AValue);
  // Goal: Returns the given ansimemo without trailing spaces.
  // Objetivo: Regresa la cadena dada sin espacios finales.
end;

procedure setTrim(var AValue: ansimemo);
begin
//  AValue := SysUtils.Trim(AValue);
  // Goal: Returns the given ansimemo without leading & trailing spaces.
  // Objetivo: Regresa la cadena dada sin espacios iniciales y finales.
end;

procedure setUnTrimLeft(var AValue: ansimemo; ACount: Integer);
var SourceRec: PansimemoRec absolute AValue;
begin
  // Goal: Returns the given ansimemo plus leading spaces.
  // Objetivo: Regresa la cadena dada mas espacios iniciales.
end;

procedure setUnTrimRight(var AValue: ansimemo; ACount: Integer);
var DestRec: PansimemoRec absolute AValue; L, C: Integer;
begin
  if (Assigned(DestRec)) then
  begin
    if (ACount <= DestRec^.Header.MaxLen) then
    begin
      L := DestRec^.Header.Len;
      C := (ACount- L);

      if (C > 0) then
      begin
        System.FillChar(DestRec^.Items[L], C, #32);
        DestRec^.Header.Len := ACount;
      end;
    end;
  end;
  // Goal: Returns the given ansistring plus trailing spaces.
  // Objetivo: Regresa la cadena dada mas espacios finales.
end;

{ global functions }

function Length(const AValue: ansimemo): Word;
begin
  Result := 0;
  if (Assigned(AValue))
    then Result := PansimemoRec(AValue)^.Header.Len;
  // Objetivo: Regresa el numero utilizado de caracteres.
  // Goal: Returns the used number of characters.
end;

function MaxLength(const AValue: ansimemo): Word;
begin
  Result := 0;
  if (Assigned(AValue))
    then Result := PansimemoRec(AValue)^.Header.MaxLen;
  // Objetivo: Regresa el numero maximo de caracteres.
  // Goal: Returns the max number of characters.
end;

function SameText(const A, B: ansimemo): Boolean;
var AIndex, Len: Integer; C, D: ansichar;
begin
  Result := TRUE;
  AIndex := 0; Len :=uktansimemos.Length(A);
  while ((AIndex < Len) and Result) do
  begin
    C := getAt(A, AIndex);
    D := getAt(B, AIndex);
    Result := uktansichars.SameText(C, D);
    Inc(AIndex);
  end;
//  Result := SysUtils.SameText(A, B);
  // Goal: Returns if 2 memos are equal, ignores sensitive case.
  // Objetivo: Regresa si 2 memos son iguales, ignora caso sensitivo.
end;

function UppercaseCopy(const AValue: ansimemo): ansimemo;
begin
  Result := DuplicateMemo(AValue);
  UppercaseReplace(Result);
  // Goal: Returns a uppercase copy of the given ansimemo.
  // Objetivo: Regresa una copia en mayusculas del ansimemo dado.
end;

function LowercaseCopy(const AValue: ansimemo): ansimemo;
begin
  Result := DuplicateMemo(AValue);
  LowercaseReplace(Result);
  // Goal: Returns a lowercase copy of the given ansimemo.
  // Objetivo: Regresa una copia en minusculas del ansimemo dado.
end;

function TogglecaseCopy(const AValue: ansimemo): ansimemo;
begin
  Result := DuplicateMemo(AValue);
  TogglecaseReplace(Result);
  // Goal: Returns a copy of the given string.
  // Objetivo: Regresa una copia de la cadena dada.
end;

function CapitalizeCopy(const AValue: ansimemo): ansimemo;
begin
  Result := DuplicateMemo(AValue);
  CapitalizeReplace(Result);
  // Goal: Returns a copy with uppercase initials of the given ansistring.
  // Objetivo: Regresa una copia con iniciales en mayusculas de la cadena dada.
end;

function IsEmpty(const AValue: ansimemo): Boolean;
var ansimemoRec: PansimemoRec absolute AValue;
begin
  Result := (ansimemoRec^.Header.Len = 0);
  // Goal: Returns if a ansimemo is empty.
  // Objetivo: Regresa si un ansimemo esta vacio.
end;

function IsStringOfChar(const S: ansimemo; C: ansichar): Boolean;
var I, L: Integer; Match: Boolean;
begin
  L :=uktansimemos.Length(S);

  Result := (L > 0);
  if (Result) then
  begin
    I := 1; Match := TRUE;
    while ((I <= L) and (Match)) do
    begin
      Match := (getAt(S ,i) = C);
      Inc(I);
    end;

    Result := Match;
  end;
  // Objetivo: Regresa si una cadena esta compuesta solo del mismo caracter.
  // Goal: Returns if a string is composed with the same character.
end;

function SameChar(const AValue: ansichar; const ACount: Word): ansimemo;
var ansimemoRec: PansimemoRec absolute AValue;
begin
  AllocMemo(Result, ACount);
  System.FillChar(ansimemoRec^.Items, ACount, AValue);
  // Goal: Returns a ansimemo of the same ansichar.
  // Objetivo: Regresa una cadena del mismo caractaer.
end;

function CharPos(const SubStr: ansichar; const S: ansimemo): Word;
var ansimemoRec: PansimemoRec absolute SubStr; Len: Word; Found: Boolean;
begin
  Len := Pred(Length(S));
  Result := 0; Found := FALSE;

  while (not Found) and (Result < Len) do
  begin
    Found := (ansimemoRec^.Items[Result] = SubStr);
    Inc(Result);
  end;

  if (Found)
    then Dec(Result)
    else Result := 0;
  // Objetivo: Regresa el indice de la primer ocurrencia de "Substr".
  // Goal: Returns the AIndex of the first ocurrence of "Substr".
end;

function CharPosReverse
  (const SubStr: ansichar; const S: ansimemo): Word;
var ansimemoRec: PansimemoRec absolute SubStr; Found: Boolean;
begin
  Result := Length(S);
  Found  := FALSE;

  while (not Found) and (Result > 0) do
  begin
    Found := (ansimemoRec^.Items[Result] = SubStr);
    Dec(Result);
  end;

  if (Found)
    then Inc(Result)
    else Result := 0;
  // Objetivo: Regresa el indice de la primer ocurrencia de "Substr".
  // Goal: Returns the AIndex of the first ocurrence of "Substr".
end;

function Pos(const SubStr: ansistring; const S: ansimemo): Word;
begin
  Result := 0;
  // Objetivo: Regresa el indice de la primer ocurrencia de "Substr".
  // Goal: Returns the AIndex of the first ocurrence of "Substr".
end;

function Pos(const SubStr: ansimemo; const S: ansimemo): Word;
begin
  Result := 0;
  // Objetivo: Regresa el indice de la primer ocurrencia de "Substr".
  // Goal: Returns the AIndex of the first ocurrence of "Substr".
end;

function StartsWith(const SubStr: ansimemo; const S: ansimemo): Boolean;
begin
  // @to-do:
  Result := false;
end;

function FinishesWith(const SubStr: ansimemo; const S: ansimemo): Boolean;
begin
  // @to-do:
  Result := false;
end;

function StartsWith(const SubStr: ansistring; const S: ansimemo): Boolean;
begin
  // @to-do:
  Result := false;
end;

function FinishesWith(const SubStr: ansistring; const S: ansimemo): Boolean;
begin
  // @to-do:
  Result := false;
end;

function Left(const AValue: ansimemo; const ACount: Word): ansimemo;
var ansimemoRec: PansimemoRec absolute AValue; Source, Dest: pansichar;
begin
  AllocMemo(Result, ACount);
  Source := PtrOfMemo(AValue);
  Dest   := PtrOfMemo(Result);
  // obtain address of source & destination characters
  // obtener diorecccion de caracteres fuente y destino

  System.Move(Source^, Dest^, ACount);
  // trasnfer characters
  // transferir caracteres

  ansimemoRec^.Header.Len := ACount;
  // update length
  // actualizar longuitud

  // Goal: Returns the leftmost characters of "AValue".
  // Objetivo: Regresa los caracteres mas a la izquierda de "AValue".
end;

function Right(const AValue: ansimemo; const ACount: Word): ansimemo;
var SourceRec, DestRec: PansimemoRec;
    Source, Dest: pansichar; AIndex: Word;
begin
  AllocMemo(Result, ACount);
  SourceRec := Result;
  Source    := @(SourceRec^.Items[0]);
  // obtain address of source characters
  // obtener direcccion de caracteres fuente

  AIndex   :=uktansimemos.Pos(ansinullchar, AValue) - ACount;
  DestRec := AValue;
  Dest    := @(DestRec^.Items[AIndex]);
  // obtain address of destination characters
  // obtener direcccion de caracteres destino

  System.Move(Source^, Dest^, ACount);
  Result := DestRec;
  // Goal: Returns the rightmost characters of "AValue".
  // Objetivo: Regresa los caracteres mas a la derecha de "AValue".
end;

procedure Empty(var AValue: ansimemo);
begin
  Clear(AValue);
  // Goal: Clear a ansimemo.
  // Objetivo: Limpia un ansimemo.
end;

procedure Copy(const Source: ansimemo; AIndex, ACount: Word; var Dest: ansistring);
var SourceRec: PansimemoRec absolute Source;
begin
  if (Assigned(SourceRec)) then
  begin
    System.SetLength(Dest, ACount);
    System.Move(SourceRec^.Items[AIndex], Dest[1], ACount);
  end;
  // Objetivo: Copiar el contenido de un ansimemo a un nuevo ansistring.
  // Goal: Copy the contents of a ansimemo into a new ansistring.
end;

procedure Copy(const Source: ansimemo; AIndex, ACount: Word; var Dest: ansimemo);
var SourceRec: PansimemoRec absolute Source;
    DestRec: PansimemoRec absolute Dest;
begin
  if (Assigned(Dest))
    then FreeMemo(Dest);
  // limpiar valores previos
  // clear previous AValues

  if (Assigned(Source)) then
  begin
    AllocMemo(Dest, ACount);
    System.Move(SourceRec^.Items[AIndex], DestRec^.Items[0], ACount);
  end;
  // Objetivo: Copiar el contenido de un ansimemo a otro nuevo ansimemo.
  // Goal: Copy the contents of a ansimemo into a new ansimemo.
end;

function SubStr(const PartialStr, FullStr: ansimemo; var AIndex: Integer): Boolean;
begin
  AIndex := Pos(PartialStr, FullStr);
  Result := (AIndex > 0);
  // Goal: Returns if a ansimemo is contained by other ansimemo.
  // Objetivo: Regresa si una cadena esta contenida en otra.
end;

function IsIdentifier(const AValue: ansimemo): Boolean;
var ansimemoRec: PansimemoRec absolute AValue;
    I, L: Integer;
begin
  Result := FALSE;
  L :=uktansimemos.Length(AValue);
  
  if ((L = 0) or not IsMember(ansimemoRec^.Items[0], AlphaSet))
    then Exit;

  for I := 1 to L do
    if (not IsMember(ansimemoRec^.Items[I], IDSet))
      then Exit;

  Result := TRUE;
  // Goal: To return if a ansimemo is a valid identifier.
  // Objetivo: Regresar si una cadena es un identificador valido.
end;

function RangeToStr(const Min, Max: ansichar): ansimemo;
var ansimemoRec: PansimemoRec absolute Result;
    AIndex: Word; C: ansichar; ACount: Byte; { s := 'a' .. 'z'; }
begin
  ACount := Succ(Ord(Max) - Ord(Min));
  AllocMemo(Result, ACount);

  AIndex := 0;
  for C := Min to Max do
  begin
    ansimemoRec^.Items[AIndex] := C;
    Inc(AIndex);
  end;
  // Goal: Transform a range of characters into a "ansimemo".
  // Objetivo: Transformar un rango de caracteres en un "ansimemo".

  // Warning: "Min" must be lesser than "Max".
  // Advertencia: "Min" debera ser menor que "Max".
end;

function ReplaceChar(const AValue: ansimemo; A, B: ansichar): ansimemo;
var ansimemoRec: PansimemoRec absolute AValue;
    AIndex, Len: Integer; P: pansichar;
begin
  Result := DuplicateMemo(ansimemoRec);
  Len    := Pred(Length(ansimemoRec));
  P      := PtrOfMemo(ansimemoRec);
  for AIndex := 0 to Len do
    if (P^ = A)
      then P^ := B;
  // Goal: Replace a specific character from a ansimemo.
  // Objetivo: Reemplazar un caracter en especifico de un ansimemo.
end;

(*
function ReplaceChars
 (const AValue: ansimemo; Source: ansicharset; Dest: ansichar): ansimemo;
var ansimemoRec: PansimemoRec absolute AValue;
    AIndex, Len: Integer; P: pansichar;
begin
  Result := DuplicateMemo(ansimemoRec);
  Len    := Pred(Length(ansimemoRec));
  P      := PtrOfMemo(ansimemoRec);
  for AIndex := 0 to Len do
    if (P^ in Source)
      then P^ := Dest;
  // Goal: Replace a specific character set from a ansimemo.
  // Objetivo: Reemplazar un conjunto caracter en especifico de un ansimemo.
end;
*)

function ReplaceStrALL
  (var FullStr: ansimemo; const SubStr, AValue: ansistring): Integer;
var Finished: Boolean;
begin
  Result := 0;
  repeat
    Finished := (ReplaceStr(FullStr, SubStr, AValue) = 0);
    if (not Finished)
      then Inc(Result);
  until Finished;
  // Goal: Replaces all the "SubStr" found in "FullStr" with "AValue".
  // Objetivo: Reemplaza todas las "SubStr" encontradas en "FullStr" por "AValue".}
end;

function ReplaceStr
  (var FullStr: ansimemo; const SubStr, AValue: ansistring): Integer;
var B, ACount: Integer; Temp: ansimemo;
begin
  Result := Pos(SubStr, FullStr);
  if (Result > 0) then
  begin
    B := Result + System.Length(SubStr);
    ACount := Length(FullStr) - System.Length(SubStr);

    if (Result <> 0) then
    begin
     uktansimemos.Copy(FullStr, 0, Pred(Result), FullStr);
     uktansimemos.Concat(FullStr, AValue);
     uktansimemos.Copy(FullStr, B, ACount, Temp);
     uktansimemos.Concat(FullStr, Temp);
    end;
  end;
  // Goal: Replaces the first "SubStr" found in "FullStr" with "AValue".
  // Objetivo: Reemplaza la primera "SubStr" encontrada en "FullStr" por "AValue".}
end;

function ReplaceCopy(const FullStr, SubStr, AValue: ansistring): ansimemo;
//var A, B, ACount: Integer;
begin
  Result := nil;
(*
  A := System.Pos(SubStr, FullStr);
  if (A > 0) then
  begin
    B := A + System.Length(SubStr);
    ACount := System.Length(FullStr) - System.Length(SubStr);

    if (A = 0)
      then Result := FullStr
      else
    Result := Copy(FullStr, 0, Pred(A)) + AValue + Copy(FullStr, B, ACount);
  end;
*)  
  // Goal: Returns a copy of the ansistring with the given SubStr replaced.
  // Objetivo: Regresa una copia de la cadena con la subcadena reemplazada.
end;

function DeleteCopy(const FullStr, SubStr: ansistring): ansimemo;
begin
  Result := ReplaceCopy(FullStr, SubStr, '');
  // Goal: Returns a copy of the ansistring with the given SubStr deleted.
  // Objetivo: Regresa una copia de la cadena con la subcadena eliminada.
end;

procedure UppercaseReplace(var AValue: ansimemo);
var Len: Integer; P: pansichar;
begin
  Len := Length(AValue);
  P   := PtrOfMemo(AValue);
{$IFDEF MSWINDOWS}
  Windows.CharUpperBuffA(P, Len);
{$ENDIF}
{$IFDEF LINUX}
//
{$ENDIF}
  // Goal: Changes the given ansimemo into uppercase.
  // Objetivo: Cambia el ansimemo dado a mayusculas.
end;

procedure LowercaseReplace(var AValue: ansimemo);
var Len: Integer; P: pansichar;
begin
  Len := Length(AValue);
  P   := PtrOfMemo(AValue);
{$IFDEF MSWINDOWS}
  Windows.CharLowerBuffA(P, Len)
{$ENDIF}
{$IFDEF LINUX}
//
{$ENDIF}
  // Goal: Changes the given ansimemo into lowercase.
  // Objetivo: Cambia el ansimemo dado a minusculas.
end;

procedure TogglecaseReplace(var AValue: ansimemo);
var P: pansichar;
begin
  P := PtrOfMemo(AValue);
  while ((P <> nil) and (P^ <> #0)) do
  begin
{$IFDEF MSWINDOWS}
    if (IsCharLowerA(P^))
      then CharUpperBuffA(P, 1)
      else CharLowerBuffA(P, 1);
{$ENDIF}
{$IFDEF LINUX}
//
{$ENDIF}
    Inc(P);
  end;
  // Goal: Swaps the sensitive case of each character in the given ansiarray.
  // Objetivo: Cambia el caso sensitivo de cada caracter en la cadena.
end;

procedure CapitalizeReplace(var AValue: ansimemo);
var P: pansichar; MakeUppercase: Boolean; Q: ansichar;
begin
  P := PtrOfMemo(AValue);
  MakeUppercase := TRUE;
  while ((P <> nil) and (P^ <> #0)) do
  begin
    Q := P^;
    if (Q <> #32) then
    begin
      if (MakeUppercase) then
      begin
        {$IFDEF MSWINDOWS}
        CharUpperBuffA(@Q, 1);
        {$ENDIF}
        {$IFDEF LINUX}
        {$ENDIF}
        MakeUppercase := FALSE;
        {$IFDEF MSWINDOWS}
      end else CharLowerBuffA(@Q, 1);
        {$ENDIF}
        {$IFDEF LINUX}
      end else ;
        {$ENDIF}
    end else MakeUppercase := TRUE;
    P^ := Q;
    Inc(P);
  end;
  // Goal: Returns a copy with uppercase initials of the given ansiarray.
  // Objetivo: Regresa una copia con iniciales en mayusculas de la cadena dada.
end;

procedure Clear(var AValue: ansimemo);
var ansimemoRec: PansimemoRec absolute AValue;
begin
  FillChar(ansimemoRec^.Items, ansimemoRec^.Header.MaxLen, #0);
  // fills the characters buffer with null chars
  // llena el buffer de caracteres con caracteres nulos

  ansimemoRec^.Header.Len := 0;
  // clears used character ACount
  // limpia cantidad de caracteres utilizados

  // Goal: Assign a null ansistring to a ansimemo.
  // Objetivo: Asignar una cadena nula a un ansimemo.
end;

function StrToMemo(const AValue: ansistring): ansimemo;
var ansimemoRec: PansimemoRec; ACount: Word;
begin
  ACount := System.Length(AValue);
  // check if there's enough space
  // checar si hay suficiente espacio

  AllocMemo(ansimemo(ansimemoRec), ACount);
  System.Move(AValue[1], ansimemoRec^.Items[0], ACount);
  // transfer characters
  // transferir caracteres

  ansimemoRec^.Header.Len    := ACount;
  ansimemoRec^.Header.MaxLen := ACount;
  // update length
  // actualizar longuitud

  Result := ansimemoRec;
  // Goal: Assign a regular ansistring to a ansimemo ansistring.
  // Objetivo: Asigna una cadena regular a una cadena ansimemo.

  // Warning: "Dest" must be assigned.
  // Advertencia: "Dest" debera estar asignado.
end;

function MemoToStr(const AValue: ansimemo): ansistring;
var ansimemoRec: PansimemoRec absolute AValue; ACount: Word;
begin
  ACount := ansimemoRec^.Header.Len;
  System.SetLength(Result, ACount);
  System.Move(ansimemoRec^.Items[0], Result[1], ACount);
  // Goal: Returns the ansistring equivalent of a ansimemo.
  // Objetivo: Regresa el equivalente cadena de un ansimemo.
end;

procedure AssignStr(var Dest: ansimemo; const Source: ansistring);
var ansimemoRec: PansimemoRec absolute Dest; ACount: Word;
begin
  ACount := System.Length(Source);
  // check if there's enough space
  // checar si hay suficiente espacio

  if (ACount <= MaxLength(Dest)) then
  begin
    System.Move(Source[1], ansimemoRec^.Items[0], ACount);
    ansimemoRec^.Header.Len := ACount;
  end;
  // Goal: Assign a ansistring to a ansimemo.
  // Objetivo: Asignar una cadena a un ansimemo.
end;

procedure AssignMemo(var Dest: ansimemo; const Source: ansimemo);
var DestRec: PansimemoRec absolute Dest;
    SourceRec: PansimemoRec absolute Source;
    ACount: Word;
begin
  ACount := Length(Source);
  // check if there's enough space
  // checar si hay suficiente espacio

  FreeMemo(Dest);
  AllocMemo(Dest, ACount);
  // get a new ansimemo
  // obtener un nuevo ansimemo

  System.Move(SourceRec^.Items, DestRec^.Items, ACount);
  // copy characters
  // copiar caracteres

  // Goal: Assign a null ansistring to a ansimemo.
  // Objetivo: Asignar una cadena nula a un ansimemo.
end;

function getAt(const List: ansimemo; const AIndex: Word): ansichar;
var ansimemoRec: PansimemoRec absolute List; Items: PansimemoArray;
begin
  Result := #0;
  if (Assigned(ansimemoRec) and (AIndex < ansimemoRec^.Header.Len)) then
  begin
    Items  := @ansimemoRec^.Items;
    Result := Items^[AIndex];
  end;
  // Goal: Returns the indicated character.
  // Objetivo: Regresa el caracter indicado.
end;

procedure setAt(const List: ansimemo; const AIndex: Word; AValue: ansichar);
var ansimemoRec: PansimemoRec absolute List; Items: PansimemoArray;
begin
  if (Assigned(ansimemoRec) and (AIndex < ansimemoRec^.Header.Len)) then
  begin
    Items := @ansimemoRec^.Items;
    Items^[AIndex] := AValue;
  end;
  // Goal: Changes the indicated character.
  // Objetivo: Cambia el caracter indicado.
end;

function PtrOfMemo(const AValue: ansimemo): pointer;
var ansimemoRec: PansimemoRec absolute AValue;
begin
  Result := @(ansimemoRec^.Items[0]);
  // Goal: Returns the address of the first item.
  // Objetivo: Regresa la direccion del primer elemento.
end;

procedure Append(var Dest: ansimemo; const Source: ansichar);
var DestRec: PansimemoRec absolute Dest;
    OldLen, MaxLen, NewLen: Word;
begin
  if (Assigned(Dest)) then
  begin
    OldLen :=uktansimemos.Length(Dest);
    MaxLen :=uktansimemos.MaxLength(Dest);
    NewLen := Succ(OldLen);

    if (NewLen <= MaxLen) then
    begin
      DestRec^.Items[NewLen] := Source;
      DestRec^.Header.Len := NewLen;
    end;
  end;
  // Objetivo: Agregar un valor al final del ansimemo.
  // Goal: Add a AValue at the end of the ansimemo.
end;

procedure InsertAt(var Dest: ansimemo; AIndex: Word; AValue: ansichar);
var DestRec: PansimemoRec absolute Dest;
    Len, I: Word; MoveAValue: ansichar;
begin
  if (Assigned(Dest)) then
  begin
    Len :=uktansimemos.Length(Dest);
    if (AIndex <= Len) then
    begin
      for I := Len downto AIndex do
      begin
        MoveAValue := getAt(Dest, I);
        // obtain previous AValue
        // obtener valor previo

        setAt(Dest, Succ(I), MoveAValue);
        // replace previous AValue
        // reemplazar valor anterior
      end;
      // move AValues
      // mover valores

      setAt(Dest, AIndex, AValue);
      // erase last AValue due to shift
      // borrar ultimo valor debido a recorrido

      DestRec^.Header.Len := Succ(Len);
      // update length
      // actualizar longuitud
    end;
  end;
  // Objetivo: Insertar un valor en la posicion indicada del ansimemo.
  // Goal: Insert a AValue in the given position of the ansimemo.
end;

procedure DeleteAt(var Dest: ansimemo; AIndex: Word);
var DestRec: PansimemoRec absolute Dest;
    Len, I: Word; MoveAValue: ansichar;
begin
  if (Assigned(Dest)) then
  begin
    Len :=uktansimemos.Length(Dest);
    if (AIndex <= Len) then
    begin
      for I := AIndex to Pred(Len) do
      begin
        MoveAValue := getAt(Dest, Succ(I));
        // obtain next AValue
        // obtener valor siguiente

        setAt(Dest, I, MoveAValue);
        // replace previous AValue
        // reemplazar valor anterior
      end;
      // move AValues
      // mover valores

      setAt(Dest, Len, ansinullchar);
      // erase last AValue due to shift
      // borrar ultimo valor debido a recorrido

      DestRec^.Header.Len := Pred(Len);
      // update length
      // actualizar longuituf
    end;
  end;
  // Objetivo: Remover un valor en la posicion indicada del ansimemo.
  // Goal: Remove a AValue in the given position of the ansimemo.
end;

procedure Concat(var Dest: ansimemo; const Source: ansistring);
var DestRec: PansimemoRec absolute Dest;
    SourceLen, DestLen, MaxLen: Word;
begin
  if (Assigned(Dest)) then
  begin
    SourceLen := System.Length(Source);
    DestLen := Succ(uktansimemos.Length(Dest));
    MaxLen := SourceLen + DestLen;
    if (MaxLen >uktansimemos.MaxLength(Dest))
      then ReAllocMemo(Dest, MaxLen);
    // update length
    // actualizar longuitud

    System.Move(Source[1], DestRec^.Items[DestLen], SourceLen);
    DestRec^.Header.Len := Pred(MaxLen);
    // transfer characters
    // transferir caracteres
  end;
  // Objetivo: Concatenar 2 cadenas.
  // Goal: To concat 2 strings.
end;

procedure Concat(var Dest: ansimemo; const Source: ansimemo);
var DestRec: PansimemoRec absolute Dest;
    SourceRec: PansimemoRec absolute Source;
    SourceLen, DestLen, MaxLen: Word;
begin
  if (Assigned(Dest)) then
  begin
    SourceLen := uktansimemos.Length(Source);
    DestLen := Succ(uktansimemos.Length(Dest));
    MaxLen := SourceLen + DestLen;
    if (MaxLen >uktansimemos.MaxLength(Dest))
      then ReAllocMemo(Dest, MaxLen);
    // update length
    // actualizar longuitud

    System.Move(SourceRec^.Items[0], DestRec^.Items[DestLen], SourceLen);
    DestRec^.Header.Len := Pred(MaxLen);
    // transfer characters
    // transferir caracteres
  end;
  // Objetivo: Concatenar 2 cadenas.
  // Goal: To concat 2 strings.
end;

procedure Insert(const Source: ansistring; var S: ansimemo; AIndex: Word);
begin
//
end;

procedure Insert(const Source: ansimemo; var S: ansimemo; AIndex: Word);
begin
//
end;

procedure Delete(var S: ansimemo; AIndex, ACount: Word);
begin
//
end;

function Equal(const A, B: ansimemo): Boolean;
begin
  Result := FALSE; // to-do
  // Goal: Returns if 2 memos are equal.
  // Objetivo: Regresa si 2 memos son iguales.
end;

function Greater(const A, B: ansimemo): Boolean;
begin
  Result := FALSE; // to-do
  // Goal: Returns if "A > B".
  // Objetivo: Regresa si "A > B".
end;

function Lesser(const A, B: ansimemo): Boolean;
begin
  Result := FALSE; // to-do
  // Goal: Returns if "A < B".
  // Objetivo: Regresa si "A < B".
end;

function Compare(const A, B: ansimemo): TComparison;
begin
  if (Equal(A, B))
    then Result := cmpEqual
  else if (Greater(A, B))
    then Result := cmpLower
  else Result := cmpHigher
  // Goal: Returns the comparison between 2 ansimemos.
  // Objetivo: Regresa la comparacion de 2 ansimemos.
end;

procedure AllocMemo(var AValue: ansimemo; const ACount: Word);
var ansimemoRec: PansimemoRec absolute AValue;
    ItemsSizeInBytes, SizeInBytes: Word;
begin
  ItemsSizeInBytes := (ACount * SizeOf(ansichar));
  // obtain size in bytes of characters
  // obtener tamaño en bytes de caracteres

  SizeInBytes := SizeOf(TansimemoHeader) + ItemsSizeInBytes;
  // obtain size of header plus size of characters
  // obtener tamaño de encabezado mas tamaño de caracteres

  System.GetMem(ansimemoRec, SizeInBytes);
  System.FillChar(ansimemoRec^, SizeInBytes, ansinullchar);
  // obtain memory
  // obtener memoria

  ansimemoRec^.Header.Len := 0;
  ansimemoRec^.Header.MaxLen := ACount;
  // update length
  // actualizar longuitud

  // Objetivo: Crear un memo ANSI con la longuitud maxima expecificada.
  // Goal: Create an ANSI memo with the given specified maximun length.
end;

procedure FreeMemo(var AValue: ansimemo);
var ansimemoRec: PansimemoRec absolute AValue;
    ItemsSizeInBytes, SizeInBytes, ACount: Word;
begin
  ACount := ansimemoRec^.Header.MaxLen;
  ItemsSizeInBytes := (ACount * SizeOf(ansichar));
  // obtain size in bytes of characters
  // obtener tamaño en bytes de caracteres

  SizeInBytes := SizeOf(TansimemoHeader) + ItemsSizeInBytes;
  // obtain size of header plus size of characters
  // obtener tamaño de encabezado mas tamaño de caracteres

  System.FreeMem(AValue, SizeInBytes);
  AValue := nil;
  // Objetivo: Liberar un memo ANSI de memoria.
  // Goal: Release an ANSI memo from memory.
end;

function DuplicateMemo(const AValue: ansimemo): ansimemo;
var SourceRec: PansimemoRec absolute AValue;
    DestRec: PansimemoRec absolute Result;
    ACount, SizeInBytes: Word;
begin
  Result := nil;

  ACount := Length(AValue);
  SizeInBytes := (ACount * SizeOf(ansichar));
  // obtain size in bytes of characters
  // obtener tamaño en bytes de caracteres

  AllocMemo(Result, ACount);
  System.Move(SourceRec^.Items[0], DestRec^.Items[0], SizeInBytes);
  // transfer characters
  // transferir caracteres

  DestRec^.Header.Len := SourceRec^.Header.Len;
  DestRec^.Header.MaxLen := SourceRec^.Header.MaxLen;
  // update length
  // actualizar longuitud

  // Goal: Returns a copy of the given ansimemo.
  // Objetivo: Regresa una copia del ansimemo dado.
end;

procedure ReAllocMemo(var AValue: ansimemo; const ACount: Word);
var DestRec: PansimemoRec absolute AValue;
    Backup: pointer;
    OldCount, SizeInBytes: Word;
begin
  OldCount := Length(DestRec);
  // check if there's enough space
  // checar si hay suficiente espacio

  SizeInBytes := (OldCount * SizeOf(ansichar));
  // obtain size in bytes of characters
  // obtener tamaño en bytes de caracteres

  System.GetMem(Backup, OldCount);
  System.Move(DestRec^.Items[0], Backup^, SizeInBytes);
  // backup ansimemo's data
  // respaldar los datos del ansimemo

  FreeMemo(AValue);
  AllocMemo(AValue, ACount);
  // get a new ansimemo
  // obtener un nuevo ansimemo

  System.Move(Backup^, DestRec^.Items[0], ACount);
  DestRec^.Header.Len := ACount;
  // copy characters
  // copiar caracteres

  FreeMem(Backup, ACount);
  // release backup
  // liberar respaldo

  // Goal: Change a ansimemos* max length.
  // Objetivo: Cambiar la maxima longuitud de un ansimemo.
end;

end.
