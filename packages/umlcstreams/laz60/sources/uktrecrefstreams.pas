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

unit uktrecrefstreams;

interface

uses
  SysUtils, Classes,
  uktStreams, uktRecStreams, uktRefStreams,
  dummy;

type

(* TCustomSDVRecordReferenceStream *)

  TCustomSDVRecordReferenceStream = class(TCustomSDVReferenceStream)
  private
    (* private declarations *)
  protected
    (* protected declarations *)

    function RecordStream: TCustomSDVRecordStream;

    function GetRecord(const Buffer: pointer): Boolean; virtual;
    function PutRecord(const Buffer: pointer): Boolean; virtual;

    function ReadRecord(const Buffer: pointer): Boolean; virtual;
    function WriteRecord(const Buffer: pointer): Boolean; virtual;

    procedure PrevRecord;
    procedure NextRecord;
  public
    (* public declarations *)

    function IsEoF: Boolean; virtual;
  end;

implementation

(* TCustomSDVRecordReferenceStream *)

function TCustomSDVRecordReferenceStream.RecordStream: TCustomSDVRecordStream;
begin
  Result := (FReference as TCustomSDVRecordStream);
end;

function TCustomSDVRecordReferenceStream.GetRecord(const Buffer: pointer): Boolean;
begin
  Result := (FReference <> nil);
  if (Result)
    then Result := RecordStream.GetRecord(Buffer);
end;

function TCustomSDVRecordReferenceStream.PutRecord(const Buffer: pointer): Boolean;
begin
  Result := (FReference <> nil);
  if (Result)
    then Result := RecordStream.PutRecord(Buffer);
end;

function TCustomSDVRecordReferenceStream.ReadRecord(const Buffer: pointer): Boolean;
begin
  Result := (FReference <> nil);
  if (Result)
    then Result := RecordStream.ReadRecord(Buffer);
end;

function TCustomSDVRecordReferenceStream.WriteRecord(const Buffer: pointer): Boolean;
begin
  Result := Assigned(FReference);
  if (Result)
    then Result := RecordStream.WriteRecord(Buffer);
end;

procedure TCustomSDVRecordReferenceStream.PrevRecord;
begin
  if (FReference <> nil)
    then RecordStream.PrevRecord;
end;

procedure TCustomSDVRecordReferenceStream.NextRecord;
begin
  if (FReference <> nil)
    then RecordStream.NextRecord;
end;

function TCustomSDVRecordReferenceStream.IsEoF: Boolean;
begin
  Result := (FReference <> nil);
  if (Result)
    then Result := RecordStream.IsEoF;
end;

end.
