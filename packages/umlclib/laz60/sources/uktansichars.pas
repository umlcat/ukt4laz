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

unit uktansichars;

interface
uses
{$IFDEF MSWINDOWS}
  Windows, //Messages,
{$ENDIF}
{$IFDEF LINUX}
  Types, Libc,
{$ENDIF}
  SysUtils, //Math,
  uktComparisons,
  dummy;

{ global constants }

const
  ansinullchar = #0;

{ global functions }

  function IsNull(const Value: ansichar): Boolean; overload;
  function IsAlpha(const Value: ansichar): Boolean; overload;
  function IsDigit(const Value: ansichar): Boolean; overload;
  function IsSpace(const Value: ansichar): Boolean; overload;
  function IsBlank(const Value: ansichar): Boolean; overload;

  function IsDecimal(const Value: ansichar): Boolean; overload;
  function IsHexa(const Value: ansichar): Boolean; overload;
  function IsOctal(const Value: ansichar): Boolean; overload;
  function IsBinary(const Value: ansichar): Boolean; overload;

  function IsUppercase(const Value: ansichar): Boolean; overload;
  function IsLowercase(const Value: ansichar): Boolean; overload;

  function UppercaseCopy(const Value: ansichar): ansichar; overload;
  function LowercaseCopy(const Value: ansichar): ansichar; overload;
  function TogglecaseCopy(const Value: ansichar): ansichar; overload;

  function SameText(const A, B: ansichar): Boolean;

  function IfChar(Predicate: Boolean; A, B: ansichar): ansichar;

  function ReplaceChar
    (const Value: ansichar; A, B: ansichar): ansichar; overload;

  procedure UppercaseReplace(var Value: ansichar); overload;
  procedure LowercaseReplace(var Value: ansichar); overload;
  procedure TogglecaseReplace(var Value: ansichar); overload;

{ global operators }

  function Equal(const A, B: ansichar): Boolean;   // operator =;
  function Greater(const A, B: ansichar): Boolean; // operator >;
  function Lesser(const A, B: ansichar): Boolean;  // operator <;
  function Different(const A, B: ansichar): Boolean;  // operator <>;
  function GreaterEqual(const A, B: ansichar): Boolean; // operator >=;
  function LesserEqual(const A, B: ansichar): Boolean;  // operator <=;

implementation

function IsNull(const Value: ansichar): Boolean;
begin
  Result := (Value = #0);
end;

function IsAlpha(const Value: ansichar): Boolean;
begin
  Result := Windows.IsCharAlphaA(Value);
end;

function IsDigit(const Value: ansichar): Boolean;
begin
  Result := (Value >= '0') and (Value <= '9');
end;

function IsSpace(const Value: ansichar): Boolean;
begin
  Result := (Ord(Value) = 32);
end;

function IsBlank(const Value: ansichar): Boolean;
begin
  Result :=
    (Ord(Value) = 32) or // space
    (Ord(Value) = 13) or // D.O.S. carriage-return
    (Ord(Value) = 10) or // D.O.S. line-feed
    (Ord(Value) = 12) or // page break
    (Ord(Value) = 08);   // tabulator
end;

function IsDecimal(const Value: ansichar): Boolean;
begin
  Result := (Value >= '0') and (Value <= '9');
end;

function IsHexa(const Value: ansichar): Boolean;
begin
  Result :=
    (Value >= '0') and (Value <= '9') or
    (Value >= 'A') and (Value <= 'F') or
    (Value >= 'a') and (Value <= 'f');
end;

function IsOctal(const Value: ansichar): Boolean;
begin
  Result := (Value >= '0') and (Value <= '7');
end;

function IsBinary(const Value: ansichar): Boolean;
begin
  Result := (Value = '0') or (Value = '1');
end;

function IsUppercase(const Value: ansichar): Boolean;
begin
  Result := Windows.IsCharUpperA(Value);
end;

function IsLowercase(const Value: ansichar): Boolean;
begin
  Result := Windows.IsCharLowerA(Value);
end;

function UppercaseCopy(const Value: ansichar): ansichar;
begin
  Result := Value;
  Windows.CharUpperBuffA(@Result, 1);
  // Goal: Returns a uppercase copy of the given character.
  // Objetivo: Regresa una copia en mayusculas del caracter dado.
end;

function LowercaseCopy(const Value: ansichar): ansichar;
begin
  Result := Value;
  Windows.CharLowerBuffA(@Result, 1);
  // Goal: Returns a lowercase copy of the given character.
  // Objetivo: Regresa una copia en minusculas del caracter dado.
end;

function TogglecaseCopy(const Value: ansichar): ansichar;
begin
  Result := Value;

  (*
  if (Windows.CharLowerA(@Result))
    then Windows.CharUpperBuff(@Result, 1)
    else Windows.CharLowerBuff(@Result, 1);
  *)
  // Goal: Returns a lowercase copy of the given character.
  // Objetivo: Regresa una copia en minusculas del caracter dado.
end;

function SameText(const A, B: ansichar): Boolean;
begin
  Result := SysUtils.SameText(A, B);
  // Goal: Returns if 2 characters are equal, ignores sensitive case.
  // Objetivo: Regresa si 2 caracteres son iguales, ignorar caso sensitivo.
end;

function IfChar(Predicate: Boolean; A, B: ansichar): ansichar;
begin
  if (Predicate)
    then Result := A
    else Result := B;
  // Objetivo: Segun la condicion, regresar el caracter seleccionado.
  // Goal: Upon condition, return the select character.
end;

function ReplaceChar(const Value: ansichar; A, B: ansichar): ansichar;
begin
  if (Value = A)
    then Result := B
    else Result := Value;
  // Objetivo: Reemplazar un caracter en especifico.
  // Goal: Upon condition, return the select character.  
end;

procedure UppercaseReplace(var Value: ansichar);
begin
  Windows.CharUpperBuffA(@Value, 1);
  // Goal: Changes the given character into uppercase.
  // Objetivo: Cambia el caracter dado a mayusculas.
end;

procedure LowercaseReplace(var Value: ansichar);
begin
  Windows.CharLowerBuffA(@Value, 1);
  // Goal: Changes the given character into lowercase.
  // Objetivo: Cambia el caracter dado a minusculas.
end;

procedure TogglecaseReplace(var Value: ansichar);
begin
  if (Windows.IsCharLowerA(Value))
    then Windows.CharUpperBuff(@Value, 1)
    else Windows.CharLowerBuff(@Value, 1);
  // Goal: Changes the given character into lowercase.
  // Objetivo: Cambia el caracter dado a minusculas.
end;

{ global operators }

function Equal(const A, B: ansichar): Boolean;
begin
  Result := (A = B);
  // Goal: Returns if 2 characters are equal.
  // Objetivo: Regresa si 2 caracteres son iguales.
end;

function Greater(const A, B: ansichar): Boolean;
begin
  Result := (A > B);
  // Goal: Returns if "A > B".
  // Objetivo: Regresa si "A > B".
end;

function Lesser(const A, B: ansichar): Boolean;
begin
  Result := (A < B);
  // Goal: Returns if "A < B".
  // Objetivo: Regresa si "A < B".
end;

function Different(const A, B: ansichar): Boolean;
begin
  Result := (A <> B);
  // Goal: Returns if "A <> B".
  // Objetivo: Regresa si "A <> B".
end;

function GreaterEqual(const A, B: ansichar): Boolean;
begin
  Result := (A >= B);
  // Goal: Returns if "A >= B".
  // Objetivo: Regresa si "A >= B".
end;

function LesserEqual(const A, B: ansichar): Boolean;
begin
  Result := (A <= B);
  // Goal: Returns if "A <= B".
  // Objetivo: Regresa si "A <= B".
end;


end.
