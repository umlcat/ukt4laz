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

unit uktfloats;

interface
uses
  SysUtils,
  uktcomparisons, 
  uktdoubles,
  uktANSIArrays, 
  uktANSIMemos, 
  uktTextConsts,
  uktANSICharSets, 
  uktANSICharsetConsts,
  dummy;

  // Las funciones "Float" utilizan "Double",
  // porque el tipo "Float" no existe !!!
  // "TFloatField" y "TCurrencyField" usan el tipo "Double" !!!

  // "Float" functions use "Double",
  // because "Float" type doesn't exist !!!
  // "TFloatField" & "TCurrencyField" use the "Double" type !!!

type
  TDBFloat = Double;
  PDBFloat = ^TDouble;

  TDBCurrency = Double;
  PDBCurrency = ^TDBCurrency;

  function StrToCurrDef
    (const Value: string; const DefValue: Currency): Currency;
  function StrToFloatDef
    (const Value: string; const DefValue: TDBFloat): TDBFloat;

  function ansimemoToCurrDef
    (const Value: ansimemo; const DefValue: Currency): Currency; overload;
  function ansimemoToFloatDef
    (const Value: ansimemo; const DefValue: TDBFloat): TDBFloat; overload;

  // operator =(const A, B: TDBFloat): Boolean;
  function Equal(const A, B: TDBFloat): Boolean;
  function Compare(const A, B: TDBFloat): TComparison;

implementation

function StrToCurrDef
  (const Value: string; const DefValue: Currency): Currency;
var i, l: byte; AnyError: Boolean;
begin
  I := 1;
  L := System.Length(Value);
  AnyError := L < 1;
  // Revisar longuitud primero

  if (L > 1) then
  while ((i<=L) and not (AnyError)) do
  begin
    AnyError := not IsMember(Value[i], CurrSet);
    Inc(i);
  end;
  // Under construction

  if (AnyError)
    then Result := DefValue
    else Result := StrToCurr(Value);
  // Goal: Returns the currency value of "Value" with type protection.
  // Objetivo: Regresa el valor moneda de "Value" con proteccion a errores.
end;

function StrToFloatDef(const Value: string; const DefValue: TDBFloat): TDBFloat;
var i, l: byte; AnyError: Boolean;
begin
  I := 1;
  L := System.Length(Value);
  AnyError := L < 1;
  // Revisar longuitud primero

  if (L > 1)
    then
  while ((i<=L) and not (AnyError)) do
  begin
    AnyError := not IsMember(Value[i], FloatSet);
    Inc(i);
  end;
  // Under construction

  if (AnyError)
    then Result := DefValue
    else Result := StrToFloat(Value);
  // Goal: Returns the float value of "Value" with error protection.
  // Objetivo: Regresa el valor flotante de "Value" con proteccion a errores.
end;

function ansimemoToCurrDef
  (const Value: ansimemo; const DefValue: Currency): Currency;
begin
  Result := StrToFloatDef(uktansimemos.MemoToStr(Value), DefValue);
  // Goal: Returns the currency value of "Value" with type protection.
  // Objetivo: Regresa el valor moneda de "Value" con proteccion a errores.
end;

function ansimemoToFloatDef(const Value: ansimemo; const DefValue: TDBFloat): TDBFloat;
begin
  Result := StrToCurrDef(uktansimemos.MemoToStr(Value), DefValue);
  // Goal: Returns the float value of "Value" with error protection.
  // Objetivo: Regresa el valor flotante de "Value" con proteccion a errores.
end;

function Equal(const A, B: TDBFloat): Boolean;
begin
  Result := (A = B);
end;

function Compare(const A, B: TDBFloat): TComparison;
begin
  if (A = B)
    then Result := cmpEqual
  else if (A < B)
    then Result := cmpLower
  else Result := cmpHigher;
end;

end.
