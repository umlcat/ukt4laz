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

unit uktansicharstreams;

interface

uses
  SysUtils, Classes,
  uktStreams, uktRecStreams, uktRecRefStreams,
  dummy;

(**
 ** Description:
 ** This unit implements a group of streams that support
 ** access to one character (one byte) A.N.S.I. data.
 **
 ** This classes do not access files by themselves,
 ** they need to connected to other streams,
 ** similar to Java-style libraries.
 **)

type

(* TCustomSDVANSICharStream *)

  TCustomSDVANSICharStream = class(TCustomSDVRecordReferenceStream)
  private
    (* private declarations *)
  protected
    (* protected declarations *)

    procedure setReference(const Value: TCustomSDVStream); override;
  public
    (* public declarations *)

    function GetChar(var Buffer: ansichar): Boolean; virtual;
    function PutChar(const Buffer: ansichar): Boolean; virtual;

    function ReadChar(var Buffer: ansichar): Boolean; virtual;
    function WriteChar(const Buffer: ansichar): Boolean; virtual;

    procedure PrevChar;
    procedure NextChar;

    function Get(var Buffer: ansichar): Boolean;
    function Put(const Buffer: ansichar): Boolean;

    function Read(var Buffer: ansichar): Boolean;
    function Write(const Buffer: ansichar): Boolean;
  end;

(* TSDVANSICharStream *)

  TSDVANSICharStream = class(TCustomSDVANSICharStream)
  published
    (* published declarations *)

    (* TCustomSDFileStream: *)

    property Reference;
  end;

implementation

(* TCustomSDVANSICharStream *)

function TCustomSDVANSICharStream.GetChar(var Buffer: ansichar): Boolean;
begin
  Result := Assigned(Reference);
  if (Result) then
  begin
    Result := RecordStream.GetRecord((@Buffer));
  end;
end;

function TCustomSDVANSICharStream.PutChar(const Buffer: ansichar): Boolean;
begin
  Result := Assigned(Reference);
  if (Result) then
  begin
    Result := RecordStream.PutRecord((@Buffer));
  end;
end;

function TCustomSDVANSICharStream.ReadChar(var Buffer: ansichar): Boolean;
begin
  Result := Assigned(Reference);
  if (Result) then
  begin
    Result := GetChar(Buffer);
    if (Result)
      then RecordStream.NextRecord;
  end;
end;

function TCustomSDVANSICharStream.WriteChar(const Buffer: ansichar): Boolean;
begin
  Result := Assigned(Reference);
  if (Result) then
  begin
    Result := PutChar(Buffer);
    if (Result)
      then RecordStream.NextRecord;
  end;
end;

procedure TCustomSDVANSICharStream.PrevChar;
begin
  PrevRecord;
end;

procedure TCustomSDVANSICharStream.NextChar;
begin
  NextRecord;
end;

procedure TCustomSDVANSICharStream.setReference(const Value: TCustomSDVStream);
begin
  if (Value <> FReference) then
  begin
    FReference := Value;
    if (FReference <> nil)
      then RecordStream.AssignRecordSize(SizeOf(ansichar));
  end;
end;

function TCustomSDVANSICharStream.Get(var Buffer: ansichar): Boolean;
begin
  Result := GetChar(Buffer);
end;

function TCustomSDVANSICharStream.Put(const Buffer: ansichar): Boolean;
begin
  Result := PutChar(Buffer);
end;

function TCustomSDVANSICharStream.Read(var Buffer: ansichar): Boolean;
begin
  Result := ReadChar(Buffer);
end;

function TCustomSDVANSICharStream.Write(const Buffer: ansichar): Boolean;
begin
  Result := WriteChar(Buffer);
end;

end.
