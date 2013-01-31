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

unit uktmaskarrays;

{$mode objfpc}{$H+}

interface
uses
  Classes,
  SysUtils,
  dummy;

(**
 ** Description:
 ** This unit declares an dynamic array of boolean values,
 ** its intended to be used in parallel, with an array of characters,
 ** or string variable, to validate a pattern.
 ** Each character in the string array, has a matching boolean,
 ** mask in the mask array. Only those values with "true",
 ** should be validated, agains a pattern.
 **)

type
  TMaskArray = array[0 .. 65535] of Boolean;
  PMaskArray = ^TMaskArray;

  function NewMaskArray
    (const AMaxSize: Integer): PMaskArray;
  function NewClearMaskArray
    (const AMaxSize: Integer): PMaskArray;

  procedure ClearMaskArray
    (var AValue: PMaskArray; const AMaxSize: Integer);
  procedure FillMaskArray
    (var AValue: PMaskArray; const AMaxSize: Integer);

  function MaskArrayGetAt
    (var AArray: PMaskArray;
      const AMaxSize: Integer; const AIndex: Integer): Boolean;
  procedure MaskArraySetAt
    (var AArray: PMaskArray;
      const AMaxSize: Integer; const AIndex: Integer; const AValue: Boolean);

implementation

function NewMaskArray(const AMaxSize: Integer): PMaskArray;
begin
  Result := nil;
  System.Getmem(Result, AMaxSize);
end;

function NewClearMaskArray(const AMaxSize: Integer): PMaskArray;
begin
  Result := NewMaskArray(AMaxSize);
  ClearMaskArray(Result, AMaxSize);
end;

procedure ClearMaskArray(var AValue: PMaskArray; const AMaxSize: Integer);
begin
  System.FillByte(AValue^, AMaxSize, 0);
end;

procedure FillMaskArray(var AValue: PMaskArray; const AMaxSize: Integer);
begin
  System.FillByte(AValue^, AMaxSize, Byte(true));
end;

function MaskArrayGetAt
  (var AArray: PMaskArray; const AMaxSize: Integer;
   const AIndex: Integer): Boolean;
begin
  Result := false;
  if (AArray <> nil) then
  begin
    if (AIndex < AMaxSize) then
    begin
      Result := AArray^[AIndex];
    end;
  end;
end;

procedure MaskArraySetAt
  (var AArray: PMaskArray;
   const AMaxSize: Integer; const AIndex: Integer; const AValue: Boolean);
begin
  if (AArray <> nil) then
  begin
    if (AIndex < AMaxSize) then
    begin
      AArray^[AIndex] := AValue;
    end;
  end;
end;

end.

