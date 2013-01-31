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

unit uktansibreaks;

interface
uses
  dummy;

const
  brkCR   = #13;
  brkLF   = #10;

  brkDOS  = #13#10;
  brkUnix = #13;
  brkClassic = #10;

  brkCRLF  = brkDOS;
  brkMacClassic = brkClassic;
  brkMacX  = brkUnix;

  brkPC    = brkUnix;
  brkWin   = brkUnix;
  brkMac   = brkUnix;
  brkLinux = brkUnix;
  brkBSD   = brkUnix;
  // nota: "Un*x" ? no pregunten porque ";-)"
  // note: "Un*x" ? don*t ask why ";-)"

type

{ TBreakStyle }

  TBreakStyle = (bksUnknown, bksCRLF, bksLF, bksCR);

const
  bksDOS     = bksCRLF;
  bksUnix    = bksCR;
  bksClassic = bksLF;

  bksWin   = bksLF;
  bksMac   = bksLF;
  bksLinux = bksLF;
  bksBSD   = bksLF;  

  function BreakToStyle(const Value: ansistring): TBreakStyle;
  function StyleToBreak(const Value: TBreakStyle): ansistring;

  function ReplaceNonDOSBreaksCopy
   (const Value: ansistring; const Source, Dest: TBreakStyle): ansistring;
  function ReplaceDOSBreaksCopy
   (const Value: ansistring; const BreakStyle: TBreakStyle): ansistring;

  procedure ReplaceNonDOSBreaks
   (var Value: ansistring; const Source, Dest: TBreakStyle);
  procedure ReplaceDOSBreaks
   (var Value: ansistring; const BreakStyle: TBreakStyle);

implementation

function BreakToStyle(const Value: ansistring): TBreakStyle;
var L: Integer;
begin
  L := System.Length(Value);
  if (L > 0) then
  begin
    if (L = 1) then
    begin
      case Value[1] of
        brkUnix:
          Result := bksLF;
        brkClassic:
          Result := bksCR;
        else
          Result := bksUnknown;
      end;
    end else
    begin
      if (Value = brkDOS)
        then Result := bksCRLF
        else Result := bksUnknown;
    end;
  end else Result := bksUnknown;
end;

function StyleToBreak(const Value: TBreakStyle): ansistring;
begin
  case Value of
    bksCRLF:
      Result := brkDOS;
    bksLF:
      Result := brkUnix;
    bksCR:
      Result := brkClassic;
    else
      Result := brkDOS;
  end;
end;

function ReplaceNonDOSBreaksCopy
 (const Value: ansistring; const Source, Dest: TBreakStyle): ansistring;
begin
  Result := Value;
  ReplaceNonDOSBreaks(Result, Source, Dest);
  // Objetivo: Cambiar los saltos de linea cuando no son DOS.
  // Goal: Change the line breaks when they*re not DOS.}
end;

function ReplaceDOSBreaksCopy
 (const Value: ansistring; const BreakStyle: TBreakStyle): ansistring;
begin
  Result := Value;
  ReplaceDOSBreaksCopy(Result, BreakStyle);
  // Objetivo: Cambiar los saltos de linea DOS por otro tipo.
  // Goal: Change the DOS line breaks for another style.
end;

procedure ReplaceNonDOSBreaks
 (var Value: ansistring; const Source, Dest: TBreakStyle);
var SourceChar, DestChar: ansichar;
begin
  if (Source = bksLF)
    then SourceChar := brkUnix
    else SourceChar := brkClassic;

  if (Dest = bksCR)
    then DestChar := brkClassic
    else DestChar := brkUnix;

  //sdvStrings.ReplaceChar(Value, SourceChar, DestChar);
  // Objetivo: Cambiar los saltos de linea cuando no son DOS.
  // Goal: Change the line breaks when they*re not DOS.}
end;

procedure ReplaceDOSBreaks
 (var Value: ansistring; const BreakStyle: TBreakStyle);
begin
  //sdvStrings.RemoveChar(Value, brkLF);

  //if (BreakStyle = bksCR)
    //then sdvStrings.ReplaceChar(Value, brkCR, brkLF);
  // Objetivo: Cambiar los saltos de linea DOS por otro tipo.
  // Goal: Change the DOS line breaks for another style.
end;

function ReplaceBreaks
 (const Value: ansistring; const Source, Dest: TBreakStyle): ansistring;
begin
  Result := '';
end;

end.
