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

unit uktwin16colors;

(* Espanol *)

  // Objetivo: Provee constantes para manejo de colores.

(* English *)

  // Goal: Provides constants for colors management.

interface
uses
{$IFDEF MSWINDOWS}
  Graphics,
{$ENDIF}
{$IFDEF LINUX}
  QGraphics,
{$ENDIF}
  SysUtils,
  uktreswin16colors,
  dummy;

const
  strwin16clBlack   = 'clBlack';
  strwin16clMaroon  = 'clMaroon';
  strwin16clGreen   = 'clGreen';
  strwin16clOlive   = 'clOlive';
  strwin16clNavy    = 'clNavy';
  strwin16clPurple  = 'clPurple';
  strwin16clTeal    = 'clTeal';
  strwin16clGray    = 'clGray';
  strwin16clSilver  = 'clSilver';
  strwin16clRed     = 'clRed';
  strwin16clLime    = 'clLime';
  strwin16clYellow  = 'clYellow';
  strwin16clBlue    = 'clBlue';
  strwin16clFuchsia = 'clFucsia';
  strwin16clAqua    = 'clAqua';
  strwin16clWhite   = 'clWhite';
  
type
  win16color =
  (
  win16clBlack,
  win16clMaroon,
  win16clGreen,
  win16clOlive,
  win16clNavy,
  win16clPurple,
  win16clTeal,
  win16clGray,
  win16clSilver,
  win16clRed,
  win16clLime,
  win16clYellow,
  win16clBlue,
  win16clFuchsia,
  win16clAqua,
  win16clWhite
  );
  twin16color = win16color;
  pwin16color = ^twin16color;

  win16colorset = set of win16color;

const
  win16clLow  = win16clBlack;
  win16clHigh = win16clWhite;

  function SafeWin16Color(const Value: win16color): win16color;

  function Win16colorToStr(const Value: win16color): string;
  function Win16colorToText(const Value: win16color): string;

  function StrToWin16Color(const Value: string): win16color;
  function TextToWin16Color(const Value: string): win16color;

  function ColorToWin16Color(const Value: TColor): win16color;
  function Win16colorToColor(const Value: win16color): TColor;

  function ColorToStr(const Value: TColor): string;
  function StrToColor(const Value: string): TColor;

implementation

type
  Twin16colorNames = array[win16color] of string;

const
  win16colorToStrArray: Twin16colorNames =
   ( strwin16clBlack, strwin16clMaroon,  strwin16clGreen,
     strwin16clOlive, strwin16clNavy,    strwin16clPurple,
     strwin16clTeal,  strwin16clGray,    strwin16clSilver,
     strwin16clRed,   strwin16clLime,    strwin16clYellow,
     strwin16clBlue,  strwin16clFuchsia, strwin16clAqua,
     strwin16clWhite );

  win16colorToTextArray: Twin16colorNames =
   ( textwin16clBlack, textwin16clMaroon,  textwin16clGreen,
     textwin16clOlive, textwin16clNavy,    textwin16clPurple,
     textwin16clTeal,  textwin16clGray,    textwin16clSilver,
     textwin16clRed,   textwin16clLime,    textwin16clYellow,
     textwin16clBlue,  textwin16clFuchsia, textwin16clAqua,
     textwin16clWhite );

  win16colorToColorArray: array[win16color] of TColor =
   ( clBlack,  clMaroon,  clGreen, clOlive,
     clNavy,   clPurple,  clTeal,  clGray,
     clSilver, clRed,     clLime,  clYellow,
     clBlue,   clFuchsia, clAqua,  clWhite );

function Safewin16color(const Value: win16color): win16color;
begin
  Result := Value;
  if (Result < Low(win16color))
    then Result := Low(win16color);
  if (Result > High(win16color))
    then Result := High(win16color);
  // Goal: Checks that a color number is valid.
  // Objetivo: Revisa que el numero de color es valido.
end;

function Win16colorToStr(const Value: win16color): string;
begin
  Result := win16colorToStrArray[Value];
  // Goal: To cast a "win16color" value to a string.
  // Objetivo: Convertir un valor "win16color" a un valor "string".
end;

function Win16colorToText(const Value: win16color): string;
begin
  Result := win16colorToTextArray[Value];
  // Goal: To cast a "win16color" value to a readable text.
  // Objetivo: Convertir un valor "win16color" a un texto legible.
end;

function MatchWin16Color
  (const Value: string; const win16colorNames: Twin16colorNames): Twin16color;
var i: win16color; Found: Boolean;
begin
  i := Low(win16color); Found := FALSE;
  while ((i <= High(Twin16color)) and (not Found)) do
  begin
    Found := SameText(win16colorNames[i], Value);
    Inc(i);
  end;

  if Found
    then Result := Pred(i)
    else Result := Low(win16color);
  // Goal: Locates a win16color by its name in a given array.
end;

function StrToWin16Color(const Value: string): win16color;
begin
  Result := MatchWin16Color(Value, Win16ColorToStrArray);
  // Goal: To cast a "string" value to a "win16color" value.
  // Objetivo: Convertir un valor "string" a un valor "win16color".
end;

function TextToWin16Color(const Value: string): win16color;
begin
  Result := MatchWin16Color(Value, Win16ColorToTextArray);
  // Goal: To cast a readable text to a "win16color" value.
  // Objetivo: Convertir un texto legible a un valor "win16color".
end;

function ColorToWin16Color(const Value: TColor): win16color;
var i: win16color; Found: Boolean;
begin
  i := Low(win16color); Found := FALSE;
  while ((i <= High(win16color)) and not Found) do
  begin
    Found := (Value = Win16ColorToColorArray[i]);
    Inc(I);
  end;

  if (Found)
    then Result := Pred(i)
    else Result := Low(win16color);
  // Goal: To cast a "TColor" value to a "win16color" value.
  // Objetivo: Convertir un valor "TColor" a un valor "win16color".
end;

function Win16colorToColor(const Value: win16color): TColor;
begin
  Result := Win16ColorToColorArray[Value];
  // Goal: To cast a "win16color" value to a "TColor" value.
  // Objetivo: Convertir un valor "win16color" a un valor "TColor".
end;

function ColorToStr(const Value: TColor): string;
begin
  Result := Win16ColorToStr( ColorToWin16Color(Value) );
  // Goal: To cast a "TColor" value to a string.
  // Objetivo: Convertir un valor "TColor" a un valor "string".
end;

function StrToColor(const Value: string): TColor;
begin
  Result := Win16ColorToColor( StrToWin16Color(Value) );
  // Goal: To cast a "string" value to a "TColor" value.
  // Objetivo: Convertir un valor "string" a un valor "TColor".
end;

end.

