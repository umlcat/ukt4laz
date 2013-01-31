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

unit uktguids;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  Classes,
  dummy;

(**
 ** Description:
 ** This unit supports function for Global Unique Identifiers.
 **
 ** This unit, DOES NOT have strings conversion functions.
 **)

 (* Global Types *)

 type
  TUUID = array[0 .. 15] of byte;
  PUUID = ^TUUID;

(* Global Functions *)

  function EqualGUID(const A, B: TGUID): Boolean;

  procedure EmptyGUID(var AGUID: TGUID);
  procedure CreateGUID(var AGUID: TGUID);

  procedure ClearGUID(var AGUID: TGUID);

implementation

function EqualGUID(const A, B: TGUID): Boolean;
begin
  Result :=
    (System.CompareByte(A, B, SizeOf(TGUID)) = 0);
end;

procedure EmptyGUID(var AGUID: TGUID);
begin
  System.FillByte((* var *) AGUID, SizeOf(TGUID), 0);
end;

procedure CreateGUID(var AGUID: TGUID);
begin
  if (SysUtils.CreateGUID((* out *) AGUID) <> 0) then
  begin
     EmptyGUID((* out *) AGUID);
  end;
end;

procedure ClearGUID(var AGUID: TGUID);
begin
  System.FillByte(AGUID, sizeof(TGUID), 0);
end;

end.

