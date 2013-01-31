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

unit ukttimes;

interface
uses
{$IFDEF MSWINDOWS}
  Windows, Messages,
{$ENDIF}
{$IFDEF LINUX}
  Types, Libc,
{$ENDIF}
  Math,
  SysUtils,
  uktcomparisons, uktansicharsets, uktansinullstrs,
  dummy;

(* English *)

  // TTime. A time value stores the number of milliseconds
  // from midnight. The number of days is ignored.

(* Espanol *)

  // TTime. Un valor tiempo almacena el numero de milisegundos
  // desde medianoche. Se ignora el numero de dias.

const
  DefaultFormat = 'h:mm am/pm';

{ global types }

type
  TTime = type Integer;
  PTime = ^TTime;

  TSDTime = TTime;
  PSDTime = ^TSDTime;

{ global constants }

const
  NoTime: TTime = 0;

var
  HourFormat, MinFormat, SecFormat, MSecFormat: ansicharset;

{ global properties }

  function getSystemTime: TTime;
  function getTimeSeparator: char;

  function getAMString: string;
  function getPMString: string;

  function getShortTimeFormat: string;
  function getLongTimeFormat: string;

  procedure setSystemTime(const Value: TTime);
  procedure setTimeSeparator(const Value: char);

  procedure setAMString(const Value: string);
  procedure setPMString(const Value: string);

  procedure setShortTimeFormat(const Value: string);
  procedure setLongTimeFormat(const Value: string);

{ global functions }

  function TimeToDateTime(const Value: TTime): TDateTime;
  function DateTimeToTime(const Value: TDateTime): TTime;

  function CanEncodeTime(var Value: TTime; Hour, Min, Sec, MSec: Word): Boolean;
  function EncodeTime(Hour, Min, Sec, MSec: Word): TTime;

  procedure DecodeTime(Value: TTime; var Hour, Min, Sec, MSec: Word);
  procedure DecodeTimeFormat
    (const Value: string; var HourFmt, MinFmt, SecFmt, MSecFmt: string);

  function VarToTime(const Value: Variant): TTime;
  function TimeToVar(const Value: TTime): Variant;

  function FormatTimeToStr(const Format: string; Value: TTime): string;
  function FormatStrToTime(const Format: string; Value: string): TTime;

  function StrToTimeDef(const S: string; Value: TTime): TTime;
  function StrToTime(const Value: string): TTime;
  function TimeToStr(const Value: TTime): string;

  function CanDecMin(var Value: TTime; Count: Integer): Boolean;
  function CanIncMin(var Value: TTime; Count: Integer): Boolean;

  function PredMSec(const Value: TTime; Count: Integer): TTime;
  function PredSec(const Value: TTime; Count: Integer): TTime;
  function PredMin(const Value: TTime; Count: Integer): TTime;

  function SuccMSec(const Value: TTime; Count: Integer): TTime;
  function SuccSec(const Value: TTime; Count: Integer): TTime;
  function SuccMin(const Value: TTime; Count: Integer): TTime;

  function Compare(const A, B: TTime): TComparison;

{ global operators }

  { = }  function Equal(const A, B: TTime): Boolean; overload;
  { <> } function Different(const A, B: TTime): Boolean; overload;
  { < }  function Lesser(const A, B: TTime): Boolean; overload;
  { > }  function Greater(const A, B: TTime): Boolean; overload;
  { <= } function LesserEqual(const A, B: TTime): Boolean; overload;
  { >= } function GreaterEqual(const A, B: TTime): Boolean; overload;

  { + }  function Add(const A, B: TTime): TTime; overload;
  { - }  function Subs(const A, B: TTime): TTime; overload;

  { ++ } function Incr(var A: TTime; const B: TTime): TTime; overload;
  { -- } function Decr(var A: TTime; const B: TTime): TTime; overload;

  { := } procedure Assign(var A: TTime; const B: TTime); overload;

implementation

{ local functions }

procedure DivMod(Dividend: Integer; Divisor: Word;
  var Result, Remainder: Word);
begin
  Math.DivMod(Dividend, Divisor, Result, Remainder);
end;

(*
asm
        PUSH    EBX
        MOV     EBX,EDX
        MOV     EDX,EAX
        SHR     EDX,16
        DIV     BX
        MOV     EBX,Remainder
        MOV     [ECX],AX
        MOV     [EBX],DX
        POP     EBX
end;
*)

{ global properties }

function getSystemTime: TTime;
var SystemTime: TSystemTime;
begin
  Windows.GetLocalTime(SystemTime);
  with SystemTime do
    Result := EncodeTime(wHour, wMinute, wSecond, wMilliseconds);
  // Goal: Returns the current system time.
  // Objetivo: Regresa la hora del sistema.
end;

function getTimeSeparator: char;
begin
  Result := SysUtils.TimeSeparator;
  // Goal: Returns the current time separator string.
  // Objetivo: Regresa la cadena separadora de tiempo.
end;

function getAMString: string;
begin
  Result := SysUtils.TimeAMString;
  // Goal: Returns the current AM time string.
  // Objetivo: Regresa la cadena para tiempo AM.
end;

function getPMString: string;
begin
  Result := SysUtils.TimePMString;
  // Goal: Returns the current PM time string.
  // Objetivo: Regresa la cadena para tiempo PM.
end;

function getShortTimeFormat: string;
begin
  Result := SysUtils.ShortTimeFormat;
  // Goal: Returns the current short time format.
  // Objetivo: Regresa el formato corto para tiempo.
end;

function getLongTimeFormat: string;
begin
  Result := SysUtils.LongTimeFormat;
  // Goal: Returns the current long time format.
  // Objetivo: Regresa el formato largo para tiempo.
end;

procedure setSystemTime(const Value: TTime);
var SystemTime: TSystemTime;
begin
  with SystemTime do
   DecodeTime(Value, wHour, wMinute, wSecond, wMilliseconds);
  Windows.SetLocalTime(SystemTime);
  // Goal: Replaces the current system time.
  // Objetivo: Reemplaza la hora del sistema.
end;

procedure setTimeSeparator(const Value: char);
begin
  SysUtils.TimeSeparator := Value;
  // Goal: Replaces the current time separator string.
  // Objetivo: Reemplaza la cadena separadora de tiempo.
end;

procedure setAMString(const Value: string);
begin
  SysUtils.TimeAMString := Value;
  // Goal: Returns the current AM time string.
  // Objetivo: Regresa la cadena para tiempo AM.
end;

procedure setPMString(const Value: string);
begin
  SysUtils.TimePMString := Value;
  // Goal: Returns the current PM time string.
  // Objetivo: Regresa la cadena para tiempo PM.
end;

procedure setShortTimeFormat(const Value: string);
begin
  SysUtils.ShortTimeFormat := Value;
  // Goal: Replaces the current short time format.
  // Objetivo: Reemplaza el formato corto para tiempo.
end;

procedure setLongTimeFormat(const Value: string);
begin
  SysUtils.LongTimeFormat := Value;
  // Goal: Replaces the current long time format.
  // Objetivo: Reemplaza el formato largo para tiempo.
end;

{ global functions }

function TimeToDateTime(const Value: TTime): TDateTime;
var Temp: SysUtils.TTimeStamp;
begin
  Temp.Date := DateDelta;
  Temp.Time := Value;
  Result := TimeStampToDateTime(Temp);
  // Goal: Used internally to return a datetime value from a time value.
  // Objetivo: Usado internamente para regresar un valor fechatiempo de un valor tiempo.
end;

function DateTimeToTime(const Value: TDateTime): TTime;
begin
  Result := SysUtils.DateTimeToTimeStamp(Value).Time;
  // Goal: Used internally to return a time value from a datetime value.
  // Objetivo: Usado internamente para regresar un valor tiempo de un valor fechatiempo.
end;

function CanEncodeTime(var Value: TTime; Hour, Min, Sec, MSec: Word): Boolean;
var Temp: System.TDateTime;
begin
  Result := TRUE;
  Temp := Integer(NoTime);
  if ((Hour < 24) and (Min < 60) and (Sec < 60) and (MSec < 1000)) then
  begin
    Temp := (Hour * 3600000 + Min * 60000 + Sec * 1000 + MSec) / MSecsPerDay;
  end;
  Value := DateTimeToTime(Temp);
  // Goal: Groups hours, minutes, seconds & milliseconds into
  // a complete time value.

  // Objetivo: Agrupa horas, minutos, segundos y milisegindos en
  // un valor tiempo completo.
end;

function EncodeTime(Hour, Min, Sec, MSec: Word): TTime;
begin
  CanEncodeTime(Result, Hour, Min, Sec, MSec);
  // Goal: Groups hours, minutes, seconds & milliseconds into
  // a complete time value.

  // Objetivo: Agrupa horas, minutos, segundos y milisegindos en
  // un valor tiempo completo.
end;

procedure DecodeTime(Value: TTime; var Hour, Min, Sec, MSec: Word);
var MinCount, MSecCount: Word;
begin
  DivMod(Value, 60000, MinCount, MSecCount);
  DivMod(MinCount, 60, Hour, Min);
  DivMod(MSecCount, 1000, Sec, MSec);
  // Goal: Ungroups a time value into individual hours, minutes, seconds &
  // milliseconds.

  // Objetivo: Desagrupa un valor tiempo en horas, minutos, segundos y
  // milisegindos en por separado.
end;

procedure DecodeTimeFormat
  (const Value: string; var HourFmt, MinFmt, SecFmt, MSecFmt: string);
var S: ansinullstring;
begin
  S := StrToNullStr(Value);
    HourFmt := uktansinullstrs.ParseWhile(S, HourFormat);
    // obtain hour*s format
    // obtener formato para hora

    MinFmt := uktansinullstrs.ParseWhile(S, MinFormat);
    // obtain minutes*s format
    // obtener formato para horas

    SecFmt := uktansinullstrs.ParseWhile(S, SecFormat);
    // obtain seconds* format
    // obtener formato para segundos

    MSecFmt := uktansinullstrs.ParseWhile(S, MSecFormat);
    // obtain seconds* format
    // obtener formato para segundos
  FreeNullStr(S);
  // Goal: Splits a time format into standalone formats.
  // Objetivo: Dividir un formato tiempo en formatos separados.
end;

function VarToTime(const Value: Variant): TTime;
begin
  Result := TTime(Integer(Value));
  // Goal: Returns a time value stored in a variant.
  // Objetivo: Regresa un valor tiempo almacenado en un variante.
end;

function TimeToVar(const Value: TTime): Variant;
begin
  Result := Variant(Integer(Value));
  // Goal: Stores a time value into a variant.
  // Objetivo: Almacena un valor tiempo dentro de un variante.
end;

function FormatTimeToStr(const Format: string; Value: TTime): string;
begin
  Result := SysUtils.FormatDateTime(Format, TimeToDateTime(Value));
  // Goal: Returns a string representation of the given time value.
  // Objetivo: Regresa una representacion alfanumerica del valor tiempo dado.
end;

function FormatStrToTime(const Format: string; Value: string): TTime;
//var FmtStr, FmtPtr: ansinullstring; Token: string;
//    Hour, Min, Sec, MSec: Word;
begin
  Result := ukttimes.StrToTime(Value);
(*
  FmtStr := StrToNullStr(Format); FmtPtr := FmtStr;
    case FmtPtr^ of
      'h', 'H':
      begin
        Token := uktansinullstrs.ExtractWhile(FmtPtr, HourFormat);
      end;
      'm', 'M':
      begin
        Token := uktansinullstrs.ExtractWhile(FmtPtr, MinFormat);
      end;
      's', 'S':
      begin
        Token := uktansinullstrs.ExtractWhile(FmtPtr, SecFormat);
      end;
      't', 'T':
      begin
        Token := uktansinullstrs.ExtractWhile(FmtPtr, MSecFormat);
      end;
    end;
  Result := NoTime;
  FreeNullStr(FmtStr);
*)
  // Goal: Returns a time value from the given string.
  // Objetivo: Regresa un valor tiempo a partir de la cadena dada.
end;

function StrToTimeDef(const S: string; Value: TTime): TTime;
begin
  try
    Result := StrToTime(S);
  except
    Result := Value;
  end;
  // Goal: Returns a time value from a string.
  // Objetivo: Regresa un valor tiempo de una cadena.
end;

function StrToTime(const Value: string): TTime;
begin
  if (Value = '')
    then Result := NoTime
    else Result := DateTimeToTime(SysUtils.StrToTime(Value));
  // Goal: Returns a time value from a string.
  // Objetivo: Regresa un valor tiempo a partir de una cadena.
end;

function TimeToStr(const Value: TTime): string;
begin
  if (Equal(Value, NoTime))
    then Result := '00:00:00'
    else Result := SysUtils.TimeToStr(TimeToDateTime(Value));
  // Goal: Returns a string from a time value.
  // Objetivo: Regresa una cadena a partir de un valor tiempo.
end;

function CanDecMin(var Value: TTime; Count: Integer): Boolean;
var Hour, Min, Sec, MSec: Word;
begin
  DecodeTime(Value, Hour, Min, Sec, MSec);
  if ((Min + Count) > 59) then
  begin
    if (Hour > 24)
      then Hour := 0
      else
        begin
          System.Inc(Hour);
          Min := (Count - Min);
          if (Min > 59)
            then Min := 0;
       end;
  end else Min := Min + Count;
  Result := CanEncodeTime(Value, Hour, Min, Sec, MSec);
end;

function CanIncMin(var Value: TTime; Count: Integer): Boolean;
var Hour, Min, Sec, MSec: Word;
begin
  DecodeTime(Value, Hour, Min, Sec, MSec);
  if (Min + Count) > 59 then
  begin
    if (Hour > 24)
      then Hour := 0
      else
        begin
          System.Inc(Hour);
          Min := (Count - Min);
          if Min > 59
            then Min := 0;
       end;
  end else Min := Min + Count;
  Result := CanEncodeTime(Value, Hour, Min, Sec, MSec);
end;

function PredMSec(const Value: TTime; Count: Integer): TTime;
begin
  Result := Value;
  // Goal: Decrements the given value by "count" seconds.
  // Objetivo: Decrementa el valor dado en "count" segundos.
end;

function PredSec(const Value: TTime; Count: Integer): TTime;
var Hour, Min, Sec, MSec: Word;
begin
  SysUtils.DecodeTime(TimeToDateTime(Value), Hour, Min, Sec, MSec);
  if (Count > 59) then
  begin
    System.Dec(Min);
    Count := 0;
  end;
  Sec := Sec - Count;
  Result := DateTimeToTime(SysUtils.EncodeTime(Hour, Min, Sec, MSec));
  // Goal: Decrements the given value by "count" seconds.
  // Objetivo: Decrementa el valor dado en "count" segundos.
end;

function PredMin(const Value: TTime; Count: Integer): TTime;
begin
  Result := Value;
  CanDecMin(Result, Count);
  // Goal: Decrements the given value by "count" minutes.
  // Objetivo: Decrementa el valor dado en "count" minutos.
end;

function SuccMSec(const Value: TTime; Count: Integer): TTime;
begin
  Result := Value;
  // Goal: Increments the given value by "count" seconds.
  // Objetivo: Incrementa el valor dado en "count" segundos.
end;

function SuccSec(const Value: TTime; Count: Integer): TTime;
var Hour, Min, Sec, MSec: Word;
begin
  SysUtils.DecodeTime(TimeToDateTime(Value), Hour, Min, Sec, MSec);
  if (Count > 59) then
  begin
    System.Inc(Min);
    Count := 0;
  end;
  Sec := Sec + Count;
  Result := DateTimeToTime(SysUtils.EncodeTime(Hour, Min, Sec, MSec));
  // Goal: Increments the given value by "count" seconds.
  // Objetivo: Incrementa el valor dado en "count" segundos.
end;

function SuccMin(const Value: TTime; Count: Integer): TTime;
begin
  Result := Value;
  CanIncMin(Result, Count);
  // Goal: Increments the given value by "count" minutes.
  // Objetivo: Incrementa el valor dado en "count" minutos.
end;

function Compare(const A, B: TTime): TComparison;
begin
  if (A = B)
    then Result := cmpEqual
  else if (A < B)
    then Result := cmpLower
  else Result := cmpHigher;
  // Goal: Returns if 2 time values are equal.
  // Objetivo: Regresa si 2 valores tiempo son iguales.
end;

{ global operators }

function Equal(const A, B: TTime): Boolean;
begin
  Result := (A = B);
  // Goal: Returns if 2 time values are equal.
  // Objetivo: Regresa si 2 valores tiempo son iguales.
end;

function Different(const A, B: TTime): Boolean;
begin
  Result := (A <> B);
  // Goal: Returns if 2 time values are not equal.
  // Objetivo: Regresa si 2 valores tiempo no son iguales.
end;

function Lesser(const A, B: TTime): Boolean;
begin
  Result := (A < B);
  // Goal: Returns if time "A" occurs before time "B".
  // Objetivo: Regresa si el valor "A" ocurre antes que el valor "B".
end;

function Greater(const A, B: TTime): Boolean;
begin
  Result := (A > B);
  // Goal: Returns if time "A" occurs after time "B".
  // Objetivo: Regresa si el valor "A" ocurre despues que el valor "B".
end;

function LesserEqual(const A, B: TTime): Boolean;
begin
  Result := (A < B);
  // Goal: Returns if time "A" occurs before or is equal to time "B".
  // Objetivo: Regresa si el valor "A" ocurre antes o es igual al valor "B".
end;

function GreaterEqual(const A, B: TTime): Boolean;
begin
  Result := (A > B);
  // Goal: Returns if time "A" occurs after or is equal to time "B".
  // Objetivo: Regresa si el valor "A" ocurre despues o es igual al valor "B".
end;

function Add(const A, B: TTime): TTime;
begin
  Result := (A + B);
  // Goal: Returns the addition of the given values.
  // Objetivo: Regresa la suma de los valores dados.
end;

function Subs(const A, B: TTime): TTime;
begin
  Result := (A - B);
  // Goal: Returns the "A" value minus the milliseconds of "B" value.
  // Objetivo: Regresa el valor "A" menos los milisegundos del valor "B".
end;

function Incr(var A: TTime; const B: TTime): TTime;
begin
  A := A + B;
  Result  := A;
  // Goal: Adds "B" milliseconds to "A" value.
  // Objetivo: Agrega "B" milisegundos a "A".
end;

function Decr(var A: TTime; const B: TTime): TTime;
begin
  A := A - B;
  Result  := A;
  // Goal: Substracts "B" milliseconds to "A" value.
  // Objetivo: Resta "B" milisegundos a "A".
end;

procedure Assign(var A: TTime; const B: TTime);
begin
  A := B;
  // Goal: Assigns a time value.
  // Objetivo: Asigna un valor tiempo.
end;

procedure UnitConstructor;
begin
  HourFormat := 'hH';
  // HourFormat := ['h', 'H'];

  MinFormat := 'mM';
  // MinFormat  := ['m', 'M'];

  SecFormat := 'sS';
  // SecFormat  := ['s', 'S'];

  MSecFormat := 'tT';
  // MSecFormat := ['t', 'T'];
end;

procedure UnitDestructor;
begin
  MSecFormat := '';
  SecFormat  := '';
  MinFormat  := '';
  HourFormat := '';
end;

initialization
  UnitConstructor;
finalization
  UnitDestructor;
end.

