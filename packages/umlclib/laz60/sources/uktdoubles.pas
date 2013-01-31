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

unit uktdoubles;

interface
uses
  SysUtils,
  uktComparisons,
  dummy;

type
  TDouble = Double;
  PDouble = ^TDouble;

  function DoubleToStr(const Value: Double): string;
  function StrToDouble(const Value: string): Double;

  function FormatDoubleToStr(const Format: string; Value: Double): string;
  function FormatStrToDouble(const Format: string; Value: string): Double;

  // operator =(const A, B: Double): Boolean;
  function Equal(const A, B: Double): Boolean;
  function Compare(const A, B: Double): TComparison;

implementation

function DoubleToStr(const Value: Double): string;
begin
  Result := FloatToStr(Value);
end;

function StrToDouble(const Value: string): Double;
begin
  Result := StrToFloat(Value);
end;

function FormatDoubleToStr(const Format: string; Value: Double): string;
begin
  Result := '';
  // Goal: Returns a string representation of the given double value.
  // Objetivo: Regresa una representacion alfanumerica del valor doble dado.
end;

function FormatStrToDouble(const Format: string; Value: string): Double;
begin
  Result := 0;
  // Goal: Returns a double value from the given string.
  // Objetivo: Regresa un valor doble a partir de la cadena dada.
end;

function Equal(const A, B: Double): Boolean;
begin
  Result := (A = B);
end;

function Compare(const A, B: Double): TComparison;
begin
  if (A = B)
    then Result := cmpEqual
  else if (A < B)
    then Result := cmpLower
  else Result := cmpHigher;
end;

end.


