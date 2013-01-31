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

unit uktdates;

{ Espanol }

  // Objetivo: Provee constantes para manejo de valores tipo fecha.

{ English }

  // Goal: Provides constants for date type values management.

interface
uses
{$IFDEF MSWINDOWS}
  Windows, Messages,
{$ENDIF}
{$IFDEF LINUX}
  Types, Libc,
{$ENDIF}
  SysUtils,
  uktcomparisons,
  dummy;

{ global types }

type
  TDate = type Integer;
  PDate = ^TDate;

  TSDDate = uktdates.TDate;
  PSDDate = uktdates.PDate;

{ global constants }

const
  NoDate = 0;

{ global properties }

  function getSystemDate: TDate;
  procedure setSystemDate(const Value: TDate);

{ global functions }

  function IsLeapYear(Year: Word): Boolean;

  function DateToDateTime(const Value: TDate): TDateTime;
  function DateTimeToDate(const Value: TDateTime): TDate;

  function CanEncodeDate(var Value: TDate; Year, Month, Day: Word): Boolean;
  function EncodeDate(Year, Month, Day: Word): TDate;
  procedure DecodeDate(Value: TDate; var Year, Month, Day: Word);

  function VarToDate(const Value: Variant): TDate;
  function DateToVar(const Value: TDate): Variant;

  function FormatDateToStr(const Format: string; Value: TDate): string;
  function FormatStrToDate(const Format: string; Value: string): TDate;

  function StrToDateDef(const S: string; Value: TDate): TDate;
  function StrToDate(const Value: string): TDate;
  function DateToStr(const Value: TDate): string;

  function PredWeek(const Value: TDate; Count: Integer): TDate;
  function PredMonth(const Value: TDate; Count: Integer): TDate;
  function PredYear(const Value: TDate; Count: Integer): TDate;
  function PredCentury(const Value: TDate; Count: Integer): TDate;

  function SuccWeek(const Value: TDate; Count: Integer): TDate;
  function SuccMonth(const Value: TDate; Count: Integer): TDate;
  function SuccYear(const Value: TDate; Count: Integer): TDate;
  function SuccCentury(const Value: TDate; Count: Integer): TDate;

  { = } function Equal(const A, B: TDate): Boolean;
  function Compare(const A, B: TDate): TComparison;

type
  PDayTable = ^TDayTable;
  TDayTable = array[1..12] of Word;

{ The MonthDays array can be used to quickly find the number of
  days in a month:  MonthDays[IsLeapYear(Y), M]      }

const
  MonthDays: array [Boolean] of TDayTable =
    ((31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31),
     (31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31));

implementation

{ global properties }

function getSystemDate: TDate;
var SystemTime: TSystemTime;
begin
  Windows.GetLocalTime(SystemTime);
  with SystemTime do
    Result := uktdates.EncodeDate(wYear, wMonth, wDay);
end;

procedure setSystemDate(const Value: TDate);
var SystemTime: TSystemTime;
begin
  with SystemTime do
    uktDates.DecodeDate(Value, wYear, wMonth, wDay);
  Windows.SetLocalTime(SystemTime);
end;

{ global functions }

function IsLeapYear(Year: Word): Boolean;
begin
  Result := (Year mod 4 = 0) and ((Year mod 100 <> 0) or (Year mod 400 = 0));
  // Goal: "IsLeapYear" determines whether the given year is a leap year.
  // Objetivo: "IsLeapYear" determina cuando el año dado es año bisiesto.
end;

function DateToDateTime(const Value: TDate): TDateTime;
var Temp: SysUtils.TTimeStamp;
begin
  Temp.Date := Value;
  Temp.Time := 0;
  Result := SysUtils.TimeStampToDateTime(Temp);
  // Goal: Used internally to return a datetime value from a time value.
  // Objetivo: Usado internamente para regresar un valor fechatiempo de un valor tiempo.
end;

function DateTimeToDate(const Value: TDateTime): TDate;
begin
  Result := SysUtils.DateTimeToTimeStamp(Value).Date;
  // Goal: Used internally to return a date value from a datetime value.
  // Objetivo: Usado internamente para regresar un valor fecha de un valor fechatiempo.
end;

function CanEncodeDate(var Value: TDate; Year, Month, Day: Word): Boolean;
begin
  Result := FALSE;
  // Goal: Groups years, months & days into a complete time value.
  // Objetivo: Agrupa años, meses y dias en un valor tiempo completo.
end;

function EncodeDate(Year, Month, Day: Word): TDate;
begin
  Result := uktdates.DateTimeToDate(SysUtils.EncodeDate(Year, Month, Day));
  // Objetivo: La funcion "EncodeDate" regresa un valor "TDate"
  // a partir de los valores especificados como "Year", "Month" y "Day".

  // Goal: The "EncodeDate" function returns a "TDate" value
  // from the values specified as "Year", "Month", and "Day".
end;

procedure DecodeDate(Value: TDate; var Year, Month, Day: Word);
begin
  if (Value = NoDate) then
  begin
    Year  := 0;
    Month := 0;
    Day   := 0;
  end else
  SysUtils.DecodeDate(DateToDateTime(Value), Year, Month, Day);
  // Objetivo: El procedimiento "DecodeDate" separa el valor especificado }
  { como "Value" en los valores "Year", "Month" y "Day".

  // Goal: The "DecodeDate" procedure breaks the value specified as }
  { "Value" into "Year", "Month", and "Day" values.}
end;

function VarToDate(const Value: Variant): TDate;
begin
  Result := TDate(Integer(Value));
  // Goal: Returns a date value stored in a variant.
  // Objetivo: Regresa un valor fecha almacenado en un variante.
end;

function DateToVar(const Value: TDate): Variant;
begin
  Result := Variant(Integer(Value));
  // Goal: Stores a date value into a variant.
  // Objetivo: Almacena un valor fecha dentro de un variante.
end;

function FormatDateToStr(const Format: string; Value: TDate): string;
begin
  Result := SysUtils.FormatDateTime(Format, DateToDateTime(Value));
  // Goal: Returns a string representation of the given date value.
  // Objetivo: Regresa una representacion alfanumerica del valor fecha dado.
end;

function FormatStrToDate(const Format: string; Value: string): TDate;
begin
  Result := uktdates.StrToDate(Value);
  // Goal: Returns a date value from the given string.
  // Objetivo: Regresa un valor fecha a partir de la cadena dada.
end;

function StrToDateDef(const S: string; Value: TDate): TDate;
begin
  try
    Result := StrToDate(S);
  except
    Result := Value;
  end;
  // Goal: Returns a date value from a string.
  // Objetivo: Regresa un valor fecha de una cadena.
end;

function StrToDate(const Value: string): TDate;
begin
  Result := uktdates.DateTimeToDate(SysUtils.StrToDate(Value));
  // Goal: Returns a date value from a string.
  // Objetivo: Regresa un valor fecha a partir de una cadena.
end;

function DateToStr(const Value: TDate): string;
begin
  Result := SysUtils.DateToStr(uktdates.DateToDateTime(Value));
  // Goal: Returns a string from a time value.
  // Objetivo: Regresa una cadena a partir de un valor tiempo.
end;

function PredWeek(const Value: TDate; Count: Integer): TDate;
begin
  Result := (Value - 7);
  // Goal: Decrements the given value by "count" weeks.
  // Objetivo: Decrementa el valor dado en "count" semanas.
end;

function PredMonth(const Value: TDate; Count: Integer): TDate;
begin
  Result := DateTimeToDate(SysUtils.IncMonth(DateToDateTime(Value), - Count));
  // Goal: Decrements the given value by "count" months.
  // Objetivo: Decrementa el valor dado en "count" meses.
end;

function PredYear(const Value: TDate; Count: Integer): TDate;
var Year, Month, Day: Word;
begin
  DecodeDate(Value, Year, Month, Day);
  Result := EncodeDate(Succ(Year), Month, Day);
  // Goal: Decrements the given value by "count" years.
  // Objetivo: Decrementa el valor dado en "count" años.
end;

function PredCentury(const Value: TDate; Count: Integer): TDate;
var Year, Month, Day: Word;
begin
  DecodeDate(Value, Year, Month, Day);
  Result := EncodeDate(Year - 100, Month, Day);
  // Goal: Decrements the given value by "count" centuries.
  // Objetivo: Decrementa el valor dado en "count" siglos.
end;

function SuccWeek(const Value: TDate; Count: Integer): TDate;
begin
  Result := (Value + 7);
  // Goal: Increments the given value by "count" weeks.
  // Objetivo: Incrementa el valor dado en "count" semanas.
end;

function SuccMonth(const Value: TDate; Count: Integer): TDate;
begin
  Result := DateTimeToDate(SysUtils.IncMonth(DateToDateTime(Value), + Count));
  // Goal: Increments the given value by "count" months.
  // Objetivo: Incrementa el valor dado en "count" meses.
end;

function SuccYear(const Value: TDate; Count: Integer): TDate;
var Year, Month, Day: Word;
begin
  DecodeDate(Value, Year, Month, Day);
  Result := EncodeDate(Succ(Year), Month, Day);
  // Goal: Increments the given value by "count" years.
  // Objetivo: Incrementa el valor dado en "count" años.
end;

function SuccCentury(const Value: TDate; Count: Integer): TDate;
var Year, Month, Day: Word;
begin
  DecodeDate(Value, Year, Month, Day);
  Result := EncodeDate(Year + 100, Month, Day);
  // Goal: Increments the given value by "count" centuries.
  // Objetivo: Incrementa el valor dado en "count" siglos.
end;

function Equal(const A, B: TDate): Boolean;
begin
  Result := (A = B);
  // Goal: Returns if 2 date values are equal.
  // Objetivo: Regresa si 2 valores fecha son iguales.
end;

function Compare(const A, B: TDate): TComparison;
begin
  if (A = B)
    then Result := cmpEqual
  else if (A < B)
    then Result := cmpLower
  else Result := cmpHigher;
  // Goal: Returns if 2 date values are equal.
  // Objetivo: Regresa si 2 valores fecha son iguales.
end;

end.
