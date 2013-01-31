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

unit uktstatestreams;

interface
uses
  SysUtils, Classes,
  uktStreams, uktRecStreams,
  dummy;

type

(* TStreamStates *)

  TStreamStates = (ssClosed, ssReset, ssRewrite, ssAppend);

(* TCustomSDVStateStream *)

  TCustomSDVStateStream = class(TCustomSDVRecordStream)
  private
    (* private declarations *)
  protected
    (* protected declarations *)

    FState: TStreamStates;
  public
    (* public declarations *)

    function IsInput: Boolean; override;
    function IsOutput: Boolean; override;
    function IsConnected: Boolean; override;

    function Connect: Boolean; override;
    function Disconnect: Boolean; override;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property State: TStreamStates
      read FState write FState;
  end;

implementation

(* TCustomSDVStateStream *)

function TCustomSDVStateStream.IsInput: Boolean;
begin
  Result := (FState <> ssClosed);
end;

function TCustomSDVStateStream.IsOutput: Boolean;
begin
  Result := (FState <> ssClosed);
end;

function TCustomSDVStateStream.IsConnected: Boolean;
begin
  Result := (FState <> ssClosed);
end;

function TCustomSDVStateStream.Connect: Boolean;
begin
  FState := ssReset;
  Result := TRUE;
end;

function TCustomSDVStateStream.Disconnect: Boolean;
begin
  FState := ssClosed;
  Result := TRUE;  
end;

constructor TCustomSDVStateStream.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FState := ssClosed;
end;

destructor TCustomSDVStateStream.Destroy;
begin
  FState := ssClosed;
  inherited Destroy;
end;

end.
 
