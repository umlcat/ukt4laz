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

unit uktstrfilestreams;

interface

uses
  SysUtils, Classes,
  uktStreams, uktRecStreams,
  dummy;

type

(* TCustomSDVStrFileStream *)

  TCustomSDVStrFileStream = class(TCustomSDVRecordStream)
  private
    (* private declarations *)
  protected
    (* protected declarations *)

    FConnected: Boolean;
    FText: string;

    function getText: string; virtual;

    procedure setText(const Value: string); virtual;
  public
    (* public declarations *)

    function IsInput: Boolean; override;
    function IsOutput: Boolean; override;

    function IsConnected: Boolean; override;

    function IsEoF: Boolean; override;

    function GetRecord(const Buffer: pointer): Boolean; override;
    function PutRecord(const Buffer: pointer): Boolean; override;

    function Connect: Boolean; override;
    function Disconnect: Boolean; override;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Text: string
      read getText write setText;
  end;

(* TSDVStrFilestream *)

  TSDVStrFilestream = class(TCustomSDVStrFileStream)
  published
    (* published declarations *)

    (* TCustomSDFileStream: *)

    property Text;
  end;

implementation

(* TCustomSDVStrFileStream *)

function TCustomSDVStrFileStream.getText: string;
begin
  Result := FText;
end;

procedure TCustomSDVStrFileStream.setText(const Value: string);
begin
  if (Value <> FText) then
  begin
    FText := Value;
    FRecordCount := System.Length(FText);
  end;
end;

function TCustomSDVStrFileStream.GetRecord(const Buffer: pointer): Boolean;
begin
  ClearRecord(Buffer);
  pansichar(Buffer)^ := FText[RecordIndex];
// (Buffer : pansichar)^ := FText[RecordIndex];
  Result := TRUE;
end;

function TCustomSDVStrFileStream.PutRecord(const Buffer: pointer): Boolean;
begin
  FText[RecordIndex] := pansichar(Buffer)^;
//  FText[RecordIndex] := (Buffer : pansichar)^;
  Result := TRUE;
end;

function TCustomSDVStrFileStream.IsInput: Boolean;
begin
  Result := TRUE;
end;

function TCustomSDVStrFileStream.IsOutput: Boolean;
begin
  Result := FALSE;
end;

function TCustomSDVStrFileStream.IsConnected: Boolean;
begin
  Result := FConnected;
end;

function TCustomSDVStrFileStream.IsEoF: Boolean;
begin
  Result := (FRecordIndex > FRecordCount);
end;

function TCustomSDVStrFileStream.Connect: Boolean;
begin
  FConnected := TRUE;
  FRecordIndex := 1;
  Result := TRUE;
end;

function TCustomSDVStrFileStream.Disconnect: Boolean;
begin
  FConnected := FALSE;
  FRecordIndex := -1;
  Result := TRUE;
end;

constructor TCustomSDVStrFileStream.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AssignRecordSize(SizeOf(ansichar));
  FText := '';
  FConnected := FALSE;
end;

destructor TCustomSDVStrFileStream.Destroy;
begin
  FConnected := FALSE;
  FText := '';
  inherited Destroy;
end;

end.
