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

unit uktdays;

{ Espanol }

  // Objetivo: Provee constantes para manejo de dias.

{ English }

  // Goal: Provides constants for days management.

interface
uses uktresdays;

const
  strdyNone      = 'dyNone';
  strdySunday    = 'dySunday';
  strdyMonday    = 'dyMonday';
  strdyTuesday   = 'dyTuesday';
  strdyWednesday = 'dyWednesday';
  strdyThursday  = 'dyThursday';
  strdyFriday    = 'dyFriday';
  strdySaturday  = 'dySaturday';

type

  TDay =
  ( dyNone,
    dySun, dyMon, dyTue, dyWed, dyThu, dyFri, dySat);

  function ValidateDay(Value: Integer): Integer;
  function SafeDay(const Value: TDay): TDay;

  function DayToStr(const Value: TDay): string;
  function DayToShortText(const Value: TDay): string;
  function DayToLongText(const Value: TDay): string;

  function StrToDay(const Value: string): TDay;
  function ShortTextToDay(const Value: string): TDay;
  function LongTextToDay(const Value: string): TDay;

implementation
uses SysUtils;

type
  TDayNames = array[TDay] of string;
  PDayNames = ^TDayNames;

const
  DayToStrArray: TDayNames =
  (strdyNone,
   strdySunday, strdyMonday, strdyTuesday, strdyWednesday,
   strdyThursday, strdyFriday, strdySaturday );

  DayToShortTextArray: TDayNames =
  (shortdyNone,
   shortdySunday, shortdyMonday, shortdyTuesday, shortdyWednesday,
   shortdyThursday, shortdyFriday, shortdySaturday );

  DayToLongTextArray: TDayNames =
  (longdyNone,
   longdySunday, longdyMonday, longdyTuesday, longdyWednesday,
   longdyThursday, longdyFriday, longdySaturday );

function ValidateDay(Value: Integer): Integer;
begin
  if (Value < 1)
    then Result := 1
  else if (Value > 31)
    then Result := 31
  else Result := Value
  // Goal: Checks that a day number is between 1 and 31.
  // Objetivo: Revisa que el numero de dia este entre 1 y 31.
end;

function SafeDay(const Value: TDay): TDay;
begin
  Result := Value;
  if (Result < Low(TDay))
    then Result := Low(TDay);
  if (Result > High(TDay))
    then Result := High(TDay);
  // Goal: Checks that a day number is between 0 and 7.
  // Objetivo: Revisa que el numero de dia este entre 0 y 7.
end;

function DayToStr(const Value: TDay): string;
begin
  Result := DayToStrArray[Value];
  // Goal: To cast a "Day" value to a "string" value.
  // Objetivo: Convertir un valor "Day" a un valor "string".
end;

function DayToShortText(const Value: TDay): string;
begin
  Result := DayToShortTextArray[Value];
  // Goal: To cast a "Day" value to a "string" value
  // (short description).

  // Objetivo: Convertir un valor "Day" a un valor "string"
  // (descripcion corta).
end;

function DayToLongText(const Value: TDay): string;
begin
  Result := DayToLongTextArray[Value];
  // Goal: To cast a "Day" value to a "string" value
  // (Long description).

  // Objetivo: Convertir un valor "Day" a un valor "string"
  // (descripcion larga).
end;

function MatchDay
  (const Value: string; const DayNames: TDayNames): TDay;
var i: TDay; Found: Boolean;
begin
  i := Low(TDay); Found := FALSE;
  while (i <= High(TDay)) and (not Found) do
  begin
    Found := SameText(DayNames[i], Value);
    Inc(i);
  end;

  if (Found)
    then Result := Pred(i)
    else Result := Low(TDay);
  // Goal: Locates a day by its name in a given array.
end;

function StrToDay(const Value: string): TDay;
begin
  Result := MatchDay(Value, DayToStrArray);
  // Goal: To cast a "string" value to a "Day" value.
  // Objetivo: Convertir un valor "string" a un valor "Day".
end;

function ShortTextToDay(const Value: string): TDay;
begin
  Result := MatchDay(Value, DayToShortTextArray);
  // Goal: To cast a "string" value to a "Day" value (short description).

  // Objetivo: Convertir un valor "string" a un valor "Day"
  // (descripcion corta).
end;

function LongTextToDay(const Value: string): TDay;
begin
  Result := MatchDay(Value, DayToLongTextArray);
  // Goal: To cast a "string" value to a "Day" value (Long description).

  // Objetivo: Convertir un valor "string" a un valor "Day"
  // (descripcion larga).
end;

end.
