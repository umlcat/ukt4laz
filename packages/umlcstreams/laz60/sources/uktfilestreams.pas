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

unit uktfilestreams;

interface

uses
  SysUtils, Classes,
  uktStreams, uktRecStreams, uktStateStreams,
  dummy;

(**
 ** Description:
 ** This unit implements a group of streams that support
 ** access to filesystem, reading or reading blocks of records.
 **)

type

  TFile = File;
  PFile = ^TFile;

(* TCustomSDVFileStream *)

  TCustomSDVFileStream = class(TCustomSDVStateStream)
  private
    (* private declarations *)
  protected
    (* protected declarations *)

    (* fields declarations *)

    FFileRec: PFile;
    FPath: string;
  protected
    (* protected declarations *)

    (* accesors declarations *)

    function getPath(): string; virtual;

    procedure setPath(const Value: string); virtual;
  public
    (* public declarations *)

    (* read-only properties declarations *)

    function IsEoF(): Boolean; override;
  public
    (* public declarations *)

    function Connect(): Boolean; override;
    function Disconnect(): Boolean; override;

    function GetRecord(const Buffer: pointer): Boolean; override;
    function PutRecord(const Buffer: pointer): Boolean; override;
  public
    (* public declarations *)

    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
  public
    (* public declarations *)

    (* properties declarations *)

    property Path: string
      read getPath write setPath;
  end;

(* TSDVFileStream *)

  TSDVFileStream = class(TCustomSDVFileStream)
  published
    (* published declarations *)

    (* TCustomSDVFileStream: *)

    property Path;
  end;

implementation

(* TCustomSDVFileStream *)

function TCustomSDVFileStream.getPath(): string;
begin
  Result := FPath;
end;

procedure TCustomSDVFileStream.setPath(const Value: string);
begin
  if (Value <> FPath) then
  begin
    FPath := Value;
  end;
end;

function TCustomSDVFileStream.IsEoF(): Boolean;
begin
  Result := System.EoF(FFileRec^);
end;

function TCustomSDVFileStream.Connect(): Boolean;
begin
  FFileRec := System.New(PFile);
  // create temporal O.S. file handler

  System.Assign(FFileRec^, FPath);
  // assign file to O.S.

  case (FState) of
    ssClosed:
      begin
        FRecordIndex := 0;
      end;
    ssReset:
      begin
        System.Reset(FFileRec^, FRecordSize);
        FRecordCount := System.FileSize(FFileRec^);
        FRecordIndex := 0;
      end;
    ssRewrite:
      begin
        System.Rewrite(FFileRec^, FRecordSize);
        FRecordIndex := 0;
      end;
    ssAppend:
      begin
        System.Reset(FFileRec^, FRecordSize);
        FRecordIndex := System.FileSize(FFileRec^);
        FRecordCount := System.FileSize(FFileRec^);
        System.Seek(FFileRec^, FRecordIndex);
      end;
    else (*None*);
  end;

  Result := TRUE;
end;

function TCustomSDVFileStream.Disconnect(): Boolean;
begin
  System.Close(FFileRec^);
  // close file

  System.Dispose(FFileRec); FFileRec := nil;
  // destroy temporal O.S. file handler

  Result := TRUE;
end;

function TCustomSDVFileStream.GetRecord(const Buffer: pointer): Boolean;
var AmtTransferred: Integer;
begin
  ClearRecord(Buffer);
  System.Seek(FFileRec^, FRecordIndex);
  System.BlockRead(FFileRec^, Buffer^, 1, AmtTransferred);
  Result := TRUE;
end;

function TCustomSDVFileStream.PutRecord(const Buffer: pointer): Boolean;
var AmtTransferred: Integer;
begin
  System.Seek(FFileRec^, FRecordIndex);
  System.BlockWrite(FFileRec^, Buffer^, 1, AmtTransferred);
  Result := TRUE;
end;

constructor TCustomSDVFileStream.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPath := '';
end;

destructor TCustomSDVFileStream.Destroy();
begin
  FPath := '';
  inherited Destroy();
end;

end.
