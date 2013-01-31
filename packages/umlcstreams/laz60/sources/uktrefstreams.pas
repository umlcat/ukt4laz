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

unit uktrefstreams;

interface

uses
  //Windows,
  SysUtils, Classes,
  uktStreams,
  dummy;

type

(* TCustomSDVReferenceStream *)

  TCustomSDVReferenceStream = class(TCustomSDVStream)
  private
    (* private declarations *)
  protected
    (* protected declarations *)

    FReference: TCustomSDVStream;

    function getReference: TCustomSDVStream; virtual;

    procedure setReference(const Value: TCustomSDVStream); virtual;

    procedure Notification
      (AComponent: TComponent; Operation: TOperation); override;
  public
    (* public declarations *)

    function IsInput: Boolean; override;
    function IsOutput: Boolean; override;

    function IsConnected: Boolean; override;

    function Connect: Boolean; override;
    function Disconnect: Boolean; override;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Reference: TCustomSDVStream
      read getReference write setReference;
  end;

implementation

(* TCustomSDVReferenceStream *)

function TCustomSDVReferenceStream.getReference: TCustomSDVStream;
begin
  Result := FReference;
end;

procedure TCustomSDVReferenceStream.setReference(const Value: TCustomSDVStream);
begin
  if (Value <> FReference) then
  begin
     FReference := Value;
  end;
end;

function TCustomSDVReferenceStream.IsInput(): Boolean;
begin
  Result := (FReference <> nil);
  if (Result) then
  begin
    Result := FReference.IsInput();
  end;
end;

function TCustomSDVReferenceStream.IsOutput(): Boolean;
begin
  Result := (FReference <> nil);
  if (Result) then
  begin
    Result := FReference.IsOutput();
  end;
end;

function TCustomSDVReferenceStream.IsConnected(): Boolean;
begin
  Result := (FReference <> nil);
  if (Result) then
  begin
    Result := FReference.IsConnected();
  end;
end;

function TCustomSDVReferenceStream.Connect(): Boolean;
begin
  Result := (FReference <> nil);
  if (Result) then
  begin
    Result := FReference.Connect();
  end;
end;

function TCustomSDVReferenceStream.Disconnect(): Boolean;
begin
  Result := (FReference <> nil);
  if (Result) then
  begin
    Result := FReference.Disconnect();
  end;
end;

procedure TCustomSDVReferenceStream.Notification
  (AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) then
  begin
    if (FReference = AComponent)
      then FReference := nil;
  end;
end;

constructor TCustomSDVReferenceStream.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FReference := nil;
end;

destructor TCustomSDVReferenceStream.Destroy;
begin
  FReference := nil;
  inherited Destroy;
end;

end.
 
