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

unit uktrgbs;

interface
uses
  Graphics,
  dummy;

type

  /// <summary>
  /// Clase para manejo de funciones para colores,
  /// en el formato Alpha (transparencia) + RGB.
  /// </summary>
  TSDVARGBRecord = record
    Alpha: Byte;
    Red:   Byte;
    Green: Byte;
    Blue:  Byte;
  end;

  TSDVRGBRecord = record
    Red:   Byte;
    Green: Byte;
    Blue:  Byte;
  end;

  TSDVRGB = LongInt;

  procedure EncodeARGBRec
    (var ADest: TSDVARGBRecord; const A, R, G, B: Byte);

  procedure DecodeARGBRec
    (const ADest: TSDVARGBRecord; var A, R, G, B: Byte);

  procedure EncodeRGBRec
    (var ADest: TSDVRGBRecord; const R, G, B: Byte);

  procedure DecodeRGBRec
    (const ADest: TSDVRGBRecord; var R, G, B: Byte);

  function ColorToRGB(const AColor: Graphics.TColor): TSDVRGB;

implementation

procedure EncodeARGBRec
  (var ADest: TSDVARGBRecord; const A, R, G, B: Byte);
begin
  ADest.Alpha := A;
  ADest.Red   := R;
  ADest.Green := G;
  ADest.Blue  := B;
end;

procedure DecodeARGBRec
  (const ADest: TSDVARGBRecord; var A, R, G, B: Byte);
begin
  A := ADest.Alpha;
  R := ADest.Red;
  G := ADest.Green;
  B := ADest.Blue;
end;

procedure EncodeRGBRec
  (var ADest: TSDVRGBRecord; const R, G, B: Byte);
begin
  ADest.Red   := R;
  ADest.Green := G;
  ADest.Blue  := B;
end;

procedure DecodeRGBRec
  (const ADest: TSDVRGBRecord; var R, G, B: Byte);
begin
  R := ADest.Red;
  G := ADest.Green;
  B := ADest.Blue;
end;

function ColorToRGB(const AColor: Graphics.TColor): TSDVRGB;
begin
  Result := Graphics.ColorToRGB(AColor);
end;

function RGBToColor(const ARGB: TSDVRGB): TColor;
begin
  //@ to-do:
  Result := ARGB;
end;

function EncodeRGB(const R, G, B: Byte): TSDVRGB;
begin

end;

//function RGBToColor(R, G, B: Byte): TColor;

end.
