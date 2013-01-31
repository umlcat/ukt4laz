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

unit uktansicharsets;

interface
uses
  uktcomparisons, 
  uktansichars,
  dummy;

{ global constants }

const
  MemberNotFound = -1;

{ global types }

type
  ansicharset = type ansistring;

  function IsMember(AItem: ansichar; const ASet: ansicharset): Boolean;
  function IsEmpty(const Value: ansicharset): Boolean;

  function Range({in} Lo, Hi: ansichar): ansicharset;

  procedure Empty(var Value: ansicharset);

  procedure Include(var ASet: ansicharset; AItem: ansichar);
  procedure Exclude(var ASet: ansicharset; AItem: ansichar);

  procedure IncludeRange(var ASet: ansicharset; Lo, Hi: ansichar);
  procedure ExcludeRange(var ASet: ansicharset; Lo, Hi: ansichar);

  function ReplaceChars
    (const Value: ansichar; Source: ansicharset; Dest: ansichar): ansichar; overload;

implementation
uses
  SysUtils, Math;

function IndexOf(AItem: ansichar; ASet: ansicharset): Integer;
begin
  Result := MemberNotFound;
end;

function IsMember(AItem: ansichar; const ASet: ansicharset): Boolean;
var Index, Count: Integer; Found: Boolean;
begin
  Index := 1; Count := System.Length(ASet); Found := FALSE;
  while (Index <= Count) and (not Found) do
  begin
    Found := (ASet[Index] = AItem);
    Inc(Index);
  end;
  Result := Found;
end;

function IsEmpty(const Value: ansicharset): Boolean;
begin
  Result := (Value = '');
end;

function Range({in} Lo, Hi: ansichar): ansicharset;
var AItem: ansichar;
begin
  Result := '';

  Lo := IfChar((Lo > Hi), Hi, Lo);
  Hi := IfChar((Lo > Hi), Lo, Hi);

  for AItem := Lo to Hi do
    Result := Result + AItem;
end;

procedure Empty(var Value: ansicharset);
begin
  Value := '';
end;

procedure Include(var ASet: ansicharset; AItem: ansichar);
begin
//  if (not IsMember(AItem, ASet))
//    then ASet := ASet + AItem;

  ASet := ASet + AItem;
end;

procedure Exclude(var ASet: ansicharset; AItem: ansichar);
var I, Index, Count: Integer; Temp: ansicharset;
begin
  Index := IndexOf(AItem, ASet);
  if (Index <> MemberNotFound) then
  begin
    Temp := '';

    Count := Pred(Index);
    for I := 1 to Count do
    begin
      Temp := Temp + ASet[I];
    end;
    // add items before selected item

    Count := Pred(System.Length(ASet));
    for I := Index to Count do
    begin
      ASet[I] := ASet[Succ(I)];
    end;
    // add items after selected item

    ASet := '';
    ASet := Temp;
  end;
end;

procedure IncludeRange(var ASet: ansicharset; Lo, Hi: ansichar);
var AItem, L, H: ansichar;
begin
  L := IfChar((Lo > Hi), Hi, Lo);
  H := IfChar((Lo > Hi), Lo, Hi);

  for AItem := L to H do
    Include(ASet, AItem);
end;

procedure ExcludeRange(var ASet: ansicharset; Lo, Hi: ansichar);
var AItem: ansichar;
begin
  Lo := IfChar((Lo > Hi), Hi, Lo);
  Hi := IfChar((Lo > Hi), Lo, Hi);

  for AItem := Lo to Hi do
    Exclude(ASet, AItem);
end;

function ReplaceChars
 (const Value: ansichar; Source: ansicharset; Dest: ansichar): ansichar;
begin
  { to-do...}
  if (IsMember(Value, Source))
    then Result := Dest
    else Result := Value;
  // Goal: Replace a specific character set.
  // Objetivo: Reemplazar un conjunto caracter en especifico.
end;

end.
