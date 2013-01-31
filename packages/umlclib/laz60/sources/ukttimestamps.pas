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

unit ukttimestamps;

interface
uses
{$IFDEF MSWINDOWS}
  Windows, Messages,
{$ENDIF}
{$IFDEF LINUX}
  Types, Libc,
{$ENDIF}
  SysUtils,
  uktdates, ukttimes,
  dummy;

{ global types }

type
  PTimeStamp = ^TTimeStamp;
  TTimeStamp = record
    Year: Word;
    Month: Word;
    Day: Word;
    Hour: Word;
    Minute: Word;
    Second: Word;
    Milliseconds: Word;
  end;

{ global constants }

const
  NoTimeStamp: TTimeStamp =
    (Year: 0; Month: 0; Day: 0; Hour: 0; Minute: 0; Second: 0; Milliseconds: 0);

{ global functions }

  function getSystemTimeStamp: TTimeStamp;
  procedure setSystemTimeStamp(const Value: TTimeStamp);

  { = } function Equal(const A, B: TTimeStamp): Boolean;
  { < } function Lesser(const A, B: TTimeStamp): Boolean;
  { > } function Greater(const A, B: TTimeStamp): Boolean;

  function Compare(const A, B: TTimeStamp): Integer;

implementation

{ global functions }

function getSystemTimeStamp: TTimeStamp;
var SystemTime: TSystemTime;
begin
  Windows.GetLocalTime(SystemTime);
  with SystemTime do
  begin
    Result.Year   := wYear;
    Result.Month  := wMonth;
    Result.Day    := wDay;
    Result.Hour   := wHour;
    Result.Minute := wMinute;
    Result.Second := wSecond;
    Result.Milliseconds := wMilliseconds;
  end;
end;

procedure setSystemTimeStamp(const Value: TTimeStamp);
var SystemTime: TSystemTime;
begin
  with SystemTime do
  begin
    wYear   := Value.Year;
    wMonth  := Value.Month;
    wDay    := Value.Day;
    wHour   := Value.Hour;
    wMinute := Value.Minute;
    wSecond := Value.Second;
    wMilliseconds := Value.Milliseconds;
  end;
  Windows.SetLocalTime(SystemTime);
end;

function Equal(const A, B: TTimeStamp): Boolean;
begin
//  Result := (Int64(A) = Int64(B));
  Result := FALSE;
  // Goal: Returns if 2 timestamp values are equal.
  // Objetivo: Regresa si 2 valores tiempoestampa son iguales.
end;

function Lesser(const A, B: TTimeStamp): Boolean;
begin
//  Result := (Int64(A) < Int64(B));
  Result := FALSE;
  // Goal: Returns if 2 timestamp values are equal.
  // Objetivo: Regresa si 2 valores tiempoestampa son iguales.
end;

function Greater(const A, B: TTimeStamp): Boolean;
begin
//  Result := (Int64(A) < Int64(B));
  Result := FALSE;
  // Goal: Returns if 2 timestamp values are equal.
  // Objetivo: Regresa si 2 valores tiempoestampa son iguales.
end;

function Compare(const A, B: TTimeStamp): Integer;
begin
  if (Equal(A, B))
    then Result := 0
  else if (Lesser(A, B))
    then Result := -1
  else Result := 0;
  // Goal: Returns if 2 timestamp values are equal.
  // Objetivo: Regresa si 2 valores tiempoestampa son iguales.
end;

end.
