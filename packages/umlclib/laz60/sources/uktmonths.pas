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

unit uktmonths;

(* Espanol *)

  // Objetivo: Provee constantes para manejo de meses.

(* English *)

  // Goal: Provides constants for months management.

interface
uses
  SysUtils, uktresmonths,
  dummy;

const
  strmoNone      = 'moNone';
  strmoJanuary   = 'moJanuary';
  strmoFebruary  = 'moFebruary';
  strmoMarch     = 'moMarch';
  strmoApril     = 'moApril';
  strmoMay       = 'moMay';
  strmoJune      = 'moJune';
  strmoJuly      = 'moJuly';
  strmoAugust    = 'moAugust';
  strmoSeptember = 'moSeptember';
  strmoOctober   = 'moOctober';
  strmoNovember  = 'moNovember';
  strmoDecember  = 'moDecember';

type

  TMonth =
  ( moNone,
    moJan, moFeb, moMar, moApr, moMay, moJun,
    moJul, moAug, moSep, moOct, moNov, moDec);

  function ValidateMonth(Value: Integer): Integer;
  function SafeMonth(const Value: TMonth): TMonth;

  function MonthToStr(const Value: TMonth): string;
  function MonthToShortText(const Value: TMonth): string;
  function MonthToLongText(const Value: TMonth): string;

  function StrToMonth(const Value: string): TMonth;
  function ShortTextToMonth(const Value: string): TMonth;
  function LongTextToMonth(const Value: string): TMonth;

implementation

type
  TMonthNames = array[TMonth] of string;
  PMonthNames = ^TMonthNames;

const
   MonthToStrArray: TMonthNames =
   (strmoNone,
    strmoJanuary, strmoFebruary, strmoMarch,
    strmoApril, strmoMay, strmoJune,
    strmoJuly, strmoAugust, strmoSeptember,
    strmoOctober, strmoNovember, strmoDecember);

   MonthToShortTextArray: TMonthNames =
   (shortmoNone,
    shortmoJanuary, shortmoFebruary, shortmoMarch,
    shortmoApril, shortmoMay, shortmoJune,
    shortmoJuly, shortmoAugust, shortmoSeptember,
    shortmoOctober, shortmoNovember, shortmoDecember);

   MonthToLongTextArray: TMonthNames =
   (longmoNone,
    longmoJanuary, longmoFebruary, longmoMarch,
    longmoApril, longmoMay, longmoJune,
    longmoJuly, longmoAugust, longmoSeptember,
    longmoOctober, longmoNovember, longmoDecember);

function ValidateMonth(Value: Integer): Integer;
begin
  if (Value < 1)
    then Result := 1
  else if (Value > 12)
    then Result := 12
  else Result := Value
  // Goal: Checks that a month number is between 1 and 12.
  // Objetivo: Revisa que el numero de mes este entre 1 y 12.
end;

function SafeMonth(const Value: TMonth): TMonth;
begin
  Result := Value;
  if (Result < Low(TMonth))
    then Result := Low(TMonth);
  if (Result > High(TMonth))
    then Result := High(TMonth);
  // Goal: Checks that a month number is between 1 and 12.
  // Objetivo: Revisa que el numero de mes este entre 1 y 12.
end;

function MonthToStr(const Value: TMonth): string;
begin
  Result := MonthToStrArray[Value];
  // Goal: To cast a "Month" value to a "string" value.
  // Objetivo: Convertir un valor "Month" a un valor "string".
end;

function MonthToShortText(const Value: TMonth): string;
begin
  Result := MonthToShortTextArray[Value];
  // Goal: To cast a "Month" value to a "string" value
  // (short description).

  // Objetivo: Convertir un valor "Month" a un valor "string"
  // (descripcion corta).
end;

function MonthToLongText(const Value: TMonth): string;
begin
  Result := MonthToLongTextArray[Value];
  // Goal: To cast a "Month" value to a "string" value
  // (Long description).

  // Objetivo: Convertir un valor "Month" a un valor "string"
  // (descripcion larga).
end;

function MatchMonth
  (const Value: string; const MonthNames: TMonthNames): TMonth;
var i: TMonth; Found: Boolean;
begin
  i := Low(TMonth); Found := FALSE;
  while ((i <= High(TMonth)) and (not Found)) do
  begin
    Found := SameText(MonthNames[i], Value);
    Inc(i);
  end;

  if (Found)
    then Result := Pred(i)
    else Result := Low(TMonth);
  // Goal: Locates a month by its name in a given array.
end;

function StrToMonth(const Value: string): TMonth;
begin
  Result := MatchMonth(Value, MonthToStrArray);
  // Goal: To cast a "string" value to a "Month" value.
  // Objetivo: Convertir un valor "string" a un valor "Month".
end;

function ShortTextToMonth(const Value: string): TMonth;
begin
  Result := MatchMonth(Value, MonthToShortTextArray);
  // Goal: To cast a "string" value to a "Month" value (short description).

  // Objetivo: Convertir un valor "string" a un valor "Month"
  // (descripcion corta).
end;

function LongTextToMonth(const Value: string): TMonth;
begin
  Result := MatchMonth(Value, MonthToLongTextArray);
  // Goal: To cast a "string" value to a "Month" value (Long description).

  // Objetivo: Convertir un valor "string" a un valor "Month"
  // (descripcion corta).
end;

end.
