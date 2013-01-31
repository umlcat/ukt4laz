(*****************************************************************************
 *                                                                           *
 *  This file is part of the UMLCat Component Library.                       *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
 **)

unit uktxmltagansiscanners;

interface

uses
  SysUtils, Classes,
  uktstreams, uktansitextstreams,
  uktansistrings,
  uktscanners,
  uktscannerstates,
  uktxmltagtokens,
  uktxmltaggroups,
  uktxmltagansisymbols,
  dummy;

type

(* TCustomXMLTagANSIScanner *)

  TCustomXMLTagANSIScanner = class(TCustomSDVScanner)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)

    FCurrentSymbol: TXMLTagANSISymbol;
    FBackupSymbol:  TXMLTagANSISymbol;

    UsePrevToken: Boolean;
  protected
    (* Protected declarations *)

    function getCurrentSymbol: TXMLTagANSISymbol; virtual;
    function getStream: TCustomSDVANSITextBasedStream; virtual;

    procedure setCurrentSymbol(const Value: TXMLTagANSISymbol); virtual;
    procedure setStream(const Value: TCustomSDVANSITextBasedStream); virtual;
  protected
    (* Protected declarations *)

    function InternalNext: Boolean; virtual;
    function ScanNext: Boolean;
  public
    (* Public declarations *)

    function Start: Boolean; override;
    function Next: Boolean; override;
    function Finish: Boolean; override;
  public
    (* Public declarations *)

    property CurrentSymbol: TXMLTagANSISymbol
      read getCurrentSymbol write setCurrentSymbol;

    property Stream: TCustomSDVANSITextBasedStream
      read getStream write setStream;
  end;

(* TXMLTagANSIScanner *)

  TXMLTagANSIScanner = class(TCustomXMLTagANSIScanner)
  published
    (* published declarations *)

    (* TCustomSDVScanner: *)

//    property InternalOptions;
    property Stream;

    (* TCustomXMLTagANSIScanner: *)
  end;

implementation

{$include 'uktxmltagmatrix.incpas'}

(* TCustomXMLTagANSIScanner *)

function TCustomXMLTagANSIScanner.getCurrentSymbol: TXMLTagANSISymbol;
begin
  Result := FCurrentSymbol;
end;

function TCustomXMLTagANSIScanner.getStream: TCustomSDVANSITextBasedStream;
begin
  Result := (FStream as TCustomSDVANSITextBasedStream);
end;

procedure TCustomXMLTagANSIScanner.setCurrentSymbol
  (const Value: TXMLTagANSISymbol);
begin
  FCurrentSymbol := Value;
end;

procedure TCustomXMLTagANSIScanner.setStream(const Value: TCustomSDVANSITextBasedStream);
begin
  if (FStream <> Value) then
  begin
    FStream := Value;
  end;
end;

function TCustomXMLTagANSIScanner.InternalNext: Boolean;
var C: ANSIchar; G: TXMLTagGroup; CurrentState: TSDVState;
begin
  uktxmltagansisymbols.Clear(FCurrentSymbol);

  FCurrentSymbol.StartRow := FCurrentRow;
  FCurrentSymbol.StartCol := FCurrentCol;
  C := #0;

  CurrentState := stStart;
  repeat
    Stream.GetChar(C);
    G := CharToGroup(C);
    CurrentState := NextState(CurrentState, G);

    if (G = xmltaggrEoLn) then
    begin
      FCurrentCol := 1;
      System.Inc(FCurrentRow);
    end else
    begin
      System.Inc(FCurrentCol);
    end;
    // actualizar posicion de caracteres
    // update characters location

    if CanSave(CurrentState)
      then uktansistrings.Concat(FCurrentSymbol.Text, C);
    if CanMove(CurrentState)
      then Stream.NextChar;
  until IsTerminal(CurrentState);

  FCurrentSymbol.FinishRow := FCurrentRow;
  FCurrentSymbol.FinishCol := FCurrentCol;
  FCurrentSymbol.Token := StateToToken(CurrentState);

  Result := not (CurrentState < stStart);
end;

function TCustomXMLTagANSIScanner.ScanNext: Boolean;
var CanContinue: Boolean;
begin
  uktxmltagansisymbols.Clear(FBackupSymbol);
  repeat
    Result := InternalNext;
    // obtener "EoF", "EoPg", "EoLn", "espacio" como simbolos independientes
    // obtain "EoF", "EoPg", "EoLn", "space" as independent symbols

    CanContinue :=
      (FCurrentSymbol.Token = xmltagtkSpace) or
      (FCurrentSymbol.Token = xmltagtkEoLn) or
      (FCurrentSymbol.Token = xmltagtkEoPg);
  until ((not CanContinue) or (not Result))
end;

function TCustomXMLTagANSIScanner.Start: Boolean;
begin
  FCurrentRow := 1;
  FCurrentCol := 1;

  Result := FStream.Connect;
end;

function TCustomXMLTagANSIScanner.Next: Boolean;
begin
  if (UsePrevToken) then
  begin
    FCurrentSymbol := FBackupSymbol;
    UsePrevToken := FALSE;
    Result := TRUE;
  end else Result := ScanNext;
end;

function TCustomXMLTagANSIScanner.Finish: Boolean;
begin
  Result := FStream.Disconnect;
end;

end.
