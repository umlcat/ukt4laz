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

unit uktdoscolors;

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
  SysUtils, Math,
  uktresdoscolors,
  dummy;

const
  strdosclBlack         = 'dosclBlack';
  strdosclBlue          = 'dosclBlue';
  strdosclGreen         = 'dosclGreen';
  strdosclCyan          = 'dosclCyan';
  strdosclRed           = 'dosclRed';
  strdosclMagenta       = 'dosclMagenta';
  strdosclBrown         = 'dosclBrown';
  strdosclLightGray     = 'dosclLightGray';
  strdosclDarkGray      = 'dosclDarkGray';
  strdosclLightBlue     = 'dosclLightBlue';
  strdosclLightGreen    = 'dosclLightGreen';
  strdosclLightCyan     = 'dosclLightCyan';
  strdosclLightRed      = 'dosclLightRed';
  strdosclLightMagenta  = 'dosclLightMagenta';
  strdosclYellow        = 'dosclYellow';
  strdosclWhite         = 'dosclWhite';

type
  doscolor =
  (
  dosclBlack,
  dosclBlue,
  dosclGreen,
  dosclCyan,
  dosclRed,
  dosclMagenta,
  dosclBrown,
  dosclLightGray,
  dosclDarkGray,
  dosclLightBlue,
  dosclLightGreen,
  dosclLightCyan,
  dosclLightRed,
  dosclLightMagenta,
  dosclYellow,
  dosclWhite
  );
  tdoscolor = doscolor;
  pdoscolor = ^tdoscolor;

  doscolorset = set of doscolor;

const
  dosclLow  = dosclBlack;
  dosclHigh = dosclWhite;

  function SafeDOSColor(const Value: doscolor): doscolor;
  function RandomDOSColor(): doscolor;
  function RandomColor(): TColor;

  function HighDOSColor(const Value: doscolor): doscolor;
  function LowDOSColor(const Value: doscolor): doscolor;

  function DOSColorToStr(const Value: doscolor): string;
  function DOSColorToText(const Value: doscolor): string;

  function StrToDOSColor(const Value: string): doscolor;
  function TextToDOSColor(const Value: string): doscolor;

  function ColorToDOSColor(const Value: TColor): doscolor;
  function DOSColorToColor(const Value: doscolor): TColor;

  function ColorToStr(const Value: TColor): string;
  function StrToColor(const Value: string): TColor;

implementation

type
  TDOSColorNames = array[doscolor] of string;

const
  DOSColorToStrArray: TDOSColorNames =
   ( strdosclBlack,    strdosclBlue,         strdosclGreen,
     strdosclCyan,     strdosclRed,          strdosclMagenta,
     strdosclBrown,    strdosclLightGray,    strdosclDarkGray,
     strdosclBlue,     strdosclLightGreen,   strdosclLightCyan,
     strdosclLightRed, strdosclLightMagenta, strdosclYellow,
     strdosclWhite );

  DOSColorToTextArray: TDOSColorNames =
   ( textdosclBlack,    textdosclBlue,         textdosclGreen,
     textdosclCyan,     textdosclRed,          textdosclMagenta,
     textdosclBrown,    textdosclLightGray,    textdosclDarkGray,
     textdosclBlue,     textdosclLightGreen,   textdosclLightCyan,
     textdosclLightRed, textdosclLightMagenta, textdosclYellow,
     textdosclWhite );

  DOSColorToColorArray: array[doscolor] of TColor =
   ( clBlack, clBlue,    clGreen,
     clTeal,  clMaroon,  clPurple,
     clOlive, clSilver,  clGray,
     clBlue,  clLime,    clAqua,
     clRed,   clFuchsia, clYellow,
     clWhite );

  {clOlive = clBrown}

function SafeDOSColor(const Value: doscolor): doscolor;
begin
  Result := Value;
  if (Result < Low(doscolor))
    then Result := Low(doscolor);
  if (Result > High(doscolor))
    then Result := High(doscolor);
  // Goal: Checks that a color number is valid.
  // Objetivo: Revisa que el numero de color es valido.
end;

function RandomDOSColor(): doscolor;
begin
  Randomize();
  Result := doscolor(Random(Ord(dosclHigh)));
end;

function RandomColor(): TColor;
begin
  Randomize();
  Result := DOSColorToColor(doscolor(Random(Ord(dosclHigh))));
end;

function HighDOSColor(const Value: doscolor): doscolor;
var I: Integer;
begin
  I := Ord(Value);
  I := Math.Max(I, I + 8);
  Result := doscolor(I);
end;

function LowDOSColor(const Value: doscolor): doscolor;
var I: Integer;
begin
  I := Ord(Value);
  I := Math.Min(I, I - 8);
  I := Math.Max(0, I);
  Result := doscolor(I);
end;

function DOSColorToStr(const Value: doscolor): string;
begin
  Result := DOSColorToStrArray[Value];
  // Goal: To cast a "doscolor" value to a string.
  // Objetivo: Convertir un valor "doscolor" a un valor "string".
end;

function DOSColorToText(const Value: doscolor): string;
begin
  Result := DOSColorToTextArray[Value];
  // Goal: To cast a "doscolor" value to a readable text.
  // Objetivo: Convertir un valor "doscolor" a un texto legible.
end;

function MatchDOSColor
  (const Value: string; const DOSColorNames: TDOSColorNames): Tdoscolor;
var i: doscolor; Found: Boolean;
begin
  i := Low(doscolor); Found := FALSE;
  while (i <= High(Tdoscolor)) and (not Found) do
  begin
    Found := SameText(DOSColorNames[i], Value);
    Inc(i);
  end;

  if (Found)
    then Result := Pred(i)
    else Result := Low(doscolor);
  // Goal: Locates a doscolor by its name in a given array.
end;

function StrToDOSColor(const Value: string): doscolor;
begin
  Result := MatchDOSColor(Value, DOSColorToStrArray);
  // Goal: To cast a "string" value to a "doscolor" value.
  // Objetivo: Convertir un valor "string" a un valor "doscolor".
end;

function TextToDOSColor(const Value: string): doscolor;
begin
  Result := MatchDOSColor(Value, DOSColorToTextArray);
  // Goal: To cast a readable text to a "doscolor" value.
  // Objetivo: Convertir un texto legible a un valor "doscolor".
end;

function ColorToDOSColor(const Value: TColor): doscolor;
var i: doscolor; Found: Boolean;
begin
  i := Low(doscolor); Found := FALSE;
  while ((i <= High(doscolor)) and (not Found)) do
  begin
    Found := (Value = DOSColorToColorArray[i]);
    Inc(I);
  end;

  if (Found)
    then Result := Pred(i)
    else Result := Low(doscolor);
  // Goal: To cast a "TColor" value to a "doscolor" value.
  // Objetivo: Convertir un valor "TColor" a un valor "doscolor".
end;

function DOSColorToColor(const Value: doscolor): TColor;
begin
  Result := DOSColorToColorArray[Value];
  // Goal: To cast a "doscolor" value to a "TColor" value.
  // Objetivo: Convertir un valor "doscolor" a un valor "TColor".
end;

function ColorToStr(const Value: TColor): string;
begin
  Result := DOSColorToStr( ColorToDOSColor(Value) );
  // Goal: To cast a "TColor" value to a string.
  // Objetivo: Convertir un valor "TColor" a un valor "string".
end;

function StrToColor(const Value: string): TColor;
begin
  Result := DOSColorToColor( StrToDOSColor(Value) );
  // Goal: To cast a "string" value to a "TColor" value.
  // Objetivo: Convertir un valor "string" a un valor "TColor".
end;

end.

