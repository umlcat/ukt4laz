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

unit uktstrliststreams;

interface

uses
  SysUtils, Classes,
  uktStreams, uktRecStreams, uktStateStreams,
  dummy;

type

(* TCustomSDVStringListStream *)

  TCustomSDVStringListStream = class(TCustomSDVStateStream)
  private
    (* private declarations *)
  protected
    (* protected declarations *)

    FIsConnected: Boolean;
    FLines: TStringList;

    function getLines(): TStringList; virtual;

    procedure setLines(const Value: TStringList); virtual;

    procedure LinesOnChange(Sender: TObject);
  public
    (* public declarations *)

    function IsConnected(): Boolean; override;

    function IsEoF(): Boolean; override;

    function Connect(): Boolean; override;
    function Disconnect(): Boolean; override;

    function GetRecord(const Buffer: pointer): Boolean; override;
    function PutRecord(const Buffer: pointer): Boolean; override;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;

    property Lines: TStringList
      read getLines write setLines;
  end;

(* tdstringlistStream *)

  TSDVStringListStream = class(TCustomSDVStringListStream)
  published
    (* published declarations *)

    (* TCustomSDFileStream: *)

    property Lines;
  end;

implementation

(* TCustomSDVStringListStream *)

function TCustomSDVStringListStream.getLines(): TStringList;
begin
  Result := FLines;
end;

procedure TCustomSDVStringListStream.setLines(const Value: TStringList);
begin
  if (Value <> FLines) then
  begin
    FLines.Assign(Value);
  end;
end;

procedure TCustomSDVStringListStream.LinesOnChange(Sender: TObject);
begin
  AssignRecordCount(System.Length(FLines.Text));
end;

function TCustomSDVStringListStream.IsConnected(): Boolean;
begin
  Result := FIsConnected;
end;

function TCustomSDVStringListStream.IsEoF(): Boolean;
begin
  Result := (FRecordIndex > FRecordCount);
end;

function TCustomSDVStringListStream.Connect(): Boolean;
begin
  FIsConnected := TRUE;
  FRecordIndex := 1;
  Result := TRUE;
end;

function TCustomSDVStringListStream.Disconnect(): Boolean;
begin
  FIsConnected := FALSE;
  FRecordIndex := -1;
  Result := TRUE;
end;

function TCustomSDVStringListStream.GetRecord(const Buffer: pointer): Boolean;
begin
  ClearRecord(Buffer);
  Result := not IsEoF;
  if Result
    then pansichar(Buffer)^ := FLines.Text[RecordIndex];
// (Buffer : pansichar)^ := FLines[RecordIndex];
end;

function TCustomSDVStringListStream.PutRecord(const Buffer: pointer): Boolean;
var S: string;
begin
  S := FLines.Text;
  S[RecordIndex] := pansichar(Buffer)^;
  FLines.Text := S;
//  FLines[RecordIndex] := (Buffer : pansichar)^;
  Result := TRUE;
end;

constructor TCustomSDVStringListStream.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AssignRecordSize(SizeOf(ansichar));

  FIsConnected := FALSE;
  FLines := TStringList.Create;

  //FLines.OnChange := (*@*)LinesOnChange;

  FLines.OnChange := @LinesOnChange;
end;

destructor TCustomSDVStringListStream.Destroy();
begin
  FLines.Free();
  FIsConnected := FALSE;
  inherited Destroy();
end;

end.
