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

unit uktrgbstrs;

interface
uses
  uktrgbs,
  dummy;

  procedure RGBRecToHex
    (const ASource: TSDVRGBRecord; var ADest: string);

  procedure HexToRGBRec
    (const ASource: string; var ADest: TSDVRGBRecord);

  procedure ARGBRecToHex
    (const ASource: TSDVARGBRecord; var ADest: string);

  procedure HexToARGBRec
    (const ASource: string; var ADest: TSDVARGBRecord);

implementation

procedure RGBRecToHex
  (const ASource: TSDVRGBRecord; var ADest: string);
begin
  // ...
end;

procedure HexToRGBRec
  (const ASource: string; var ADest: TSDVRGBRecord);
begin
  // ...
end;

procedure ARGBRecToHex
  (const ASource: TSDVARGBRecord; var ADest: string);
begin
  // ...
end;

procedure HexToARGBRec
  (const ASource: string; var ADest: TSDVARGBRecord);
begin
  // ...
end;



end.
