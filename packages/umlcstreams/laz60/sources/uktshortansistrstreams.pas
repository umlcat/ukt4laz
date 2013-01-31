(**
 **************************************************************************
 *                                                                        *
 *  This file is part of the UMLCat's Component Library.                  *
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

unit uktshortansistrstreams;

interface

uses
  SysUtils, Classes,
  uktShortAnsiStrs,
  uktStreams, uktRecStreams, uktRecRefStreams,
  dummy;

type

(* TCustomSDVShortANSIStringStream *)

  TCustomSDVShortANSIStringStream = class(TCustomSDVRecordReferenceStream)
  private
    (* private declarations *)
  protected
    (* protected declarations *)

    procedure setReference(const Value: TCustomSDVStream); override;
  public
    (* public declarations *)

    function Put(var Buffer: shortansistring): Boolean; dynamic;
    function Get(const Buffer: shortansistring): Boolean; dynamic;

    function Read(var Buffer: shortansistring): Boolean; dynamic;
    function Write(const Buffer: shortansistring): Boolean; dynamic;
  end;

(* TSDVShortANSIStringStream *)

  TSDVShortANSIStringStream = class(TCustomSDVShortANSIStringStream)
  published
    (* published declarations *)

    (* TCustomSDFileStream: *)

    property Reference;
  end;

implementation

(* TCustomSDVShortANSIStringStream *)

procedure TCustomSDVShortANSIStringStream.setReference
  (const Value: TCustomSDVStream);
begin
  if (Value <> FReference) then
  begin
    FReference := Value;
    if (Assigned(FReference))
      then RecordStream.AssignRecordSize(SizeOf(shortansistring));
  end;
end;

function TCustomSDVShortANSIStringStream.Put(var Buffer: shortansistring): Boolean;
begin
  Result := Assigned(Reference);
  if (Result) then
  begin
    Result := RecordStream.PutRecord((@Buffer));
  end;
end;

function TCustomSDVShortANSIStringStream.Get(const Buffer: shortansistring): Boolean;
begin
  Result := Assigned(Reference);
  if (Result) then
  begin
    Result := RecordStream.GetRecord((@Buffer));
  end;
end;

function TCustomSDVShortANSIStringStream.Read(var Buffer: shortansistring): Boolean;
begin
  Result := Assigned(Reference);
  if (Result) then
  begin
    Result := RecordStream.ReadRecord((@Buffer));
  end;
end;

function TCustomSDVShortANSIStringStream.Write(const Buffer: shortansistring): Boolean;
begin
  Result := Assigned(Reference);
  if (Result) then
  begin
    Result := RecordStream.WriteRecord((@Buffer));
  end;
end;

end.
