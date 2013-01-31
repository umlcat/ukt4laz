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

unit uktrecstreams;

interface
uses
  SysUtils, Classes,
  uktStreams,
  dummy;

const
  sterRecordSizeNotAssigned = 01;

type

(* TCustomSDVRecordStream *)

  TCustomSDVRecordStream = class(TCustomSDVStream)
  private
    (* private declarations *)
  protected
    (* protected declarations *)

    FRecordIndex: Integer;
    FRecordSize:  Integer;
    FRecordCount: Integer;
  public
    (* public declarations *)

    constructor Create(AOwner: TComponent); override;

    function IsEoF: Boolean; dynamic; abstract;

    function RecordIndex: Integer; dynamic;
    function RecordSize: Integer; dynamic;
    function RecordCount: Integer; dynamic;

    function GetRecord(const Buffer: pointer): Boolean; dynamic; abstract;
    function PutRecord(const Buffer: pointer): Boolean; dynamic; abstract;

    function ReadRecord(const Buffer: pointer): Boolean; dynamic;
    function WriteRecord(const Buffer: pointer): Boolean; dynamic;

    procedure ClearRecord(const Buffer: pointer);

    procedure PrevRecord;
    procedure NextRecord;

    // friend protected
    procedure AssignRecordSize(const Value: Integer); dynamic;
    procedure AssignRecordCount(const Value: Integer); dynamic;
  end;

implementation

(* TCustomSDVRecordStream *)

constructor TCustomSDVRecordStream.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FRecordIndex := 0;
  FRecordSize  := 0;
  FRecordCount := 0;
end;

function TCustomSDVRecordStream.RecordIndex: LongInt;
begin
  Result := FRecordIndex;
  (* Goal: Returns "index of current record" .*)
end;

function TCustomSDVRecordStream.RecordSize: LongInt;
begin
  Result := FRecordSize;
  (* Goal: Returns "Size in bytes of each record" .*)
end;

function TCustomSDVRecordStream.RecordCount: LongInt;
begin
  Result := FRecordCount;
  (* Goal: Returns "How many records the file has" .*)
end;

function TCustomSDVRecordStream.ReadRecord(const Buffer: pointer): Boolean;
begin
  Result := GetRecord(Buffer);
  if (Result)
    then NextRecord;
end;

function TCustomSDVRecordStream.WriteRecord(const Buffer: pointer): Boolean;
begin
  Result := PutRecord(Buffer);
  if (Result)
    then NextRecord;
end;

procedure TCustomSDVRecordStream.ClearRecord(const Buffer: pointer);
begin
  System.FillChar(Buffer^, FRecordSize, 0);
end;

procedure TCustomSDVRecordStream.PrevRecord;
begin
  System.Dec(FRecordIndex);
end;

procedure TCustomSDVRecordStream.NextRecord;
begin
  System.Inc(FRecordIndex);
end;

procedure TCustomSDVRecordStream.AssignRecordSize(const Value: Integer);
begin
  if (not IsConnected)
    then FRecordSize := Value;
  (* Goal: Changes "Size in bytes of each record" .*)
end;

procedure TCustomSDVRecordStream.AssignRecordCount(const Value: Integer);
begin
  if (not IsConnected)
    then FRecordCount := Value;
  (* Goal: Changes "How many records" .*)
end;

end.
