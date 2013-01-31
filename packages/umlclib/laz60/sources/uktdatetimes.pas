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

unit uktdatetimes;

interface
uses
  SysUtils, 
  uktComparisons,
  dummy;

const
  NoDateTime = 0;

type
  TDateTime = System.TDateTime;

  function PredMonth(const Value: TDateTime): TDateTime;
  function SuccMonth(const Value: TDateTime): TDateTime;

  function EncodeDateTime(const Date, Time: TDateTime): TDateTime;
  procedure DecodeDateTime(const Value: TDateTime; var Date, Time: TDateTime);

  function StrToDateTime(const Value: string): TDateTime;
  function DateTimeToStr(const Value: TDateTime): string;

  function FormatDateTimeToStr(const Format: string; Value: TDateTime): string;
  function FormatStrToDateTime(const Format: string; Value: string): TDateTime;

  // operator =(const A, B: TDateTime): Boolean;
  function Equal(const A, B: TDateTime): Boolean;
  function Compare(const A, B: TDateTime): TComparison;

implementation

function PredMonth(const Value: TDateTime): TDateTime;
var Year, Month, Day: Word;
begin
  SysUtils.DecodeDate(Value, Year, Month, Day);
  Dec(Month);
  Result := SysUtils.EncodeDate(Year, Month, Day);
end;

function SuccMonth(const Value: TDateTime): TDateTime;
var Year, Month, Day: Word;
begin
  SysUtils.DecodeDate(Value, Year, Month, Day);
  Inc(Month);
  Result := SysUtils.EncodeDate(Year, Month, Day);
end;

function EncodeDateTime(const Date, Time: TDateTime): TDateTime;
begin
  Result := Date + Time;
  // Goal: Join a date & time value.
  // Objetivo: Juntar un valor fecha con un valor hora.
end;

procedure DecodeDateTime(const Value: TDateTime; var Date, Time: TDateTime);
var Year, Month, Day: Word; Hour, Min, Sec, MSec: Word;
begin
  SysUtils.DecodeDate(Value, Year, Month, Day);
  // obtain year, month & day from datetime value
  // obtener año, mes y dia de valor fechahora

  Date := SysUtils.EncodeDate(Year, Month, Day);
  // obtain the date without the time
  // obtener la fecha sin la hora

  SysUtils.DecodeTime(Value, Hour, Min, Sec, MSec);
  // obtain hour, minutes, seconds, & milliseconds from datetime value
  // obtener hora, minutos, segundos y milisegundos de valor fechahora

  Time := SysUtils.EncodeTime(Hour, Min, Sec, MSec);
  // obtain the time without the date
  // obtener la hora sin la fecha

  // Goal: Splits a date & time value.
  // Objetivo: Separa un valor fecha con un valor hora.
end;

function StrToDateTime(const Value: string): TDateTime;
begin
  Result := SysUtils.StrToDateTime(Value);
  // Goal: Returns a datetime value of the given string description.
  // Objetivo: Regresa un valor fechatiempo de la descripcion alfanumerica dada.
end;

function DateTimeToStr(const Value: TDateTime): string;
begin
  Result := SysUtils.DateTimeToStr(Value);
  // Goal: Returns a string description of the given datetime value.
  // Objetivo: Regresa una descripcion alfanumerica del valor fechatiempo dado.
end;

function FormatDateTimeToStr(const Format: string; Value: TDateTime): string;
begin
  Result := SysUtils.FormatDateTime(Format, Value);
  // Goal: Returns a string representation of the given datetime value.
  // Objetivo: Regresa una representacion alfanumerica del valor fechahora dado.
end;

function FormatStrToDateTime(const Format: string; Value: string): TDateTime;
begin
  Result := SysUtils.StrToDateTime(Value);
  // Goal: Returns a datetime value from the given string.
  // Objetivo: Regresa un valor fechahora a partir de la cadena dada.
end;

function Equal(const A, B: TDateTime): Boolean;
begin
  Result := (A = B);
end;

function Compare(const A, B: TDateTime): TComparison;
begin
  if (A = B)
    then Result := cmpEqual
  else if (A < B)
    then Result := cmpLower
  else Result := cmpHigher;
end;

end.
