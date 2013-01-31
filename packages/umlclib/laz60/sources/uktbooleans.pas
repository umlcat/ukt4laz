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

unit uktbooleans;

(* Espanol *)

  // Objetivo: Provee constantes para manejo de valores booleanos.

(* English *)

  // Goal: Provides constants for boolean values management.

interface
uses
  SysUtils, Math,
  uktresBooleans,
  ukttextconsts,
  uktansicharsets,
  uktansicharsetconsts,
  dummy;

const
  resBoolFalse = 'FALSE';
  resBoolTrue  = 'TRUE';

type
  TBoolean = Boolean;
  //PBoolean = ^TBoolean;

  TWordBool = WordBool;
  //PWordBool = ^TWordBool;

  function BoolToInt(const Value: Boolean): Integer;
  function IntToBool(const Value: Integer): Boolean;

  function BoolToChar(const Value: Boolean): char;
  function CharToBool(const Value: char): Boolean;

  function BoolToStr(const Value: Boolean): string;
  function BoolToShortText(const Value: Boolean): string;
  function BoolToLongText(const Value: Boolean): string;

  function StrToBool(const Value: string): Boolean;
  function ShortTextToBool(const Value: string): Boolean;
  function LongTextToBool(const Value: string): Boolean;

implementation

const
   BoolToStrArray: array[Boolean] of string =
     (resBoolFalse, resBoolTrue);
   BoolToShortTextArray: array[Boolean] of string =
     (resshortFalse, resshortTrue);
   BoolToLongTextArray: array[Boolean] of string =
     (reslongFalse, reslongTrue);

function BoolToInt(const Value: Boolean): Integer;
begin
  if Value
    then Result := 1
    else Result := 0;
//    then Result := -1
  // Goal: To cast a "boolean" value to a "integer" value.
  // Objetivo: Convertir un valor "boolean" a un valor "integer".
end;

function IntToBool(const Value: Integer): Boolean;
begin
  Result := (Value <> 0);
  // Goal: To cast a "integer" value to a "boolean" value.
  // Objetivo: Convertir un valor "integer" a un valor "boolean".
end;

function BoolToChar(const Value: Boolean): char;
begin
  if Value
    then Result := 'T'
    else Result := 'F';
  // Goal: To cast a "Boolean" value to a "char" value.
  // Objetivo: Convertir un valor "Boolean" a un valor "char".
end;

function CharToBool(const Value: char): Boolean;
begin
  Result := not IsMember(Value, BoolSet);
  // Goal: To cast a "char" value to a "Boolean" value.
  // Objetivo: Convertir un valor "char" a un valor "Boolean".
end;

function BoolToStr(const Value: Boolean): string;
begin
  if (Value)
    then Result := 'TRUE'
    else Result := 'FALSE';

(*
  Result := BoolToStrArray[Value];
  if (Value)
    then Result := BoolToStrArray[TRUE]
    else Result := BoolToStrArray[FALSE];
*)
  // Goal: To cast a "boolean" value to a "string" value.
  // Objetivo: Convertir un valor "Boolean" a un valor "string".
end;

function BoolToShortText(const Value: Boolean): string;
begin
  Result := BoolToShortTextArray[Value];
  // Goal: To cast a "boolean" value to a "string" value
  // (short description).

  // Objetivo: Convertir un valor "Boolean" a un valor "string"
  // (descripcion corta).
end;

function BoolToLongText(const Value: Boolean): string;
begin
  Result := BoolToLongTextArray[Value];
  // Goal: To cast a "boolean" value to a "string" value
  // (short description).

  // Objetivo: Convertir un valor "Boolean" a un valor "string"
  // (descripcion corta).
end;

function StrToBool(const Value: string): Boolean;
var Source: string;
begin
  Source := UpperCase(Value);
  // avoid case incompatibility
  // evitar incompatibilidad por caso

  Result := (Source = BoolToStrArray[TRUE]);
  // Goal: To cast a "string" value to a "Boolean" value.
  // Objetivo: Convertir un valor "string" a un valor "Boolean".
end;

function ShortTextToBool(const Value: string): Boolean;
var Source: string;
begin
  Source := UpperCase(Value);
  // avoid case incompatibility
  // evitar incompatibilidad por caso

  Result := (Source = BoolToShortTextArray[TRUE]);
  // Goal: To cast a "string" value to a "Boolean" value (short description).

  // Objetivo: Convertir un valor "string" a un valor "Boolean" }
  // (descripcion corta).
end;

function LongTextToBool(const Value: string): Boolean;
var Source: string;
begin
  Source := UpperCase(Value);
  // avoid case incompatibility
  // evitar incompatibilidad por caso

  Result := (Source = BoolToLongTextArray[TRUE]);
  // Goal: To cast a "string" value to a "Boolean" value (short description).

  // Objetivo: Convertir un valor "string" a un valor "Boolean"
  // (descripcion corta).
end;

end.
