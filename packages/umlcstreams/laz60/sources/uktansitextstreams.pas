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

unit uktansitextstreams;

interface
uses
  SysUtils, Classes,
  uktAnsiBreaks, uktTextMarkers,
  uktStreams, uktRecStreams, uktRecRefStreams,
  uktShortAnsiStrs, uktAnsiCharStreams,
  dummy;

(**
 ** Description:
 ** This unit implements a group of streams that support
 ** access to lines composed by
 ** one character (one byte) A.N.S.I. data.
 **
 ** This classes do not access files by themselves,
 ** they need to connected to other streams,
 ** similar to Java-style libraries.
 **)

(*$OPENSTRINGS OFF*)

type

(* TCustomSDVANSITextBasedStream *)

  TCustomSDVANSITextBasedStream = class(TCustomSDVAnsiCharStream)
  private
    (* private declarations *)
  protected
    (* protected declarations *)

    // deprecated:
    function ReadStr(var Buffer: shortansistring): Boolean; virtual;
    // deprecated:
    function WriteStr(const Buffer: shortansistring): Boolean; virtual;

    function LineMarkerFound(): Boolean; virtual;
    function PageMarkerFound(): Boolean; virtual;
    function FileMarkerFound(): Boolean; virtual;

    function WriteLineMarker(): Boolean; virtual;
    function WritePageMarker(): Boolean; virtual;
    function WriteFileMarker(): Boolean; virtual;
  public
    (* public declarations *)

    function GetChar(var Buffer: ansichar): Boolean; override;
  end;

(* TCustomSDVAnsiSourceStream *)

  TCustomSDVAnsiSourceStream = class(TCustomSDVANSITextBasedStream)
  private
    (* private declarations *)
  protected
    (* protected declarations *)
  public
    (* public declarations *)

    function LineMarkerFound(): Boolean; override; // promoted;
    function PageMarkerFound(): Boolean; override; // promoted;
    function FileMarkerFound(): Boolean; override; // promoted;

    function WriteLineMarker(): Boolean; override; // promoted;
    function WritePageMarker(): Boolean; override; // promoted;
    function WriteFileMarker(): Boolean; override; // promoted;
  end;

(* TCustomSDVAnsiTextstream *)

  TCustomSDVAnsiTextStream = class(TCustomSDVANSITextBasedStream)
  private
    (* private declarations *)
  protected
    (* protected declarations *)
  public
    (* public declarations *)

    function IsEoLn(): Boolean;

    function ReadLn(var Buffer: shortansistring): Boolean;
    function WriteLn(const Buffer: shortansistring): Boolean;
  end;

(* TSDVAnsiTextStream *)

  TSDVAnsiTextStream = class(TCustomSDVAnsiTextStream)
  published
    (* published declarations *)

    (* TCustomSDVAnsiTextStream: *)

    property Reference;
  end;

(* TCustomSDVAnsiSourceStream *)

  TSDVAnsiSourceStream = class(TCustomSDVAnsiSourceStream)
  published
    (* published declarations *)

    (* TCustomSDVAnsiTextStream: *)

    property Reference;
  end;

implementation

(* TCustomSDVANSITextBasedStream *)

function TCustomSDVANSITextBasedStream.ReadStr(var Buffer: shortansistring): Boolean;
var CharBuffer: ansichar; L: Integer;
begin
  System.FillChar(Buffer, System.SizeOf(Buffer), 0);

  Result := Assigned(FReference);
  if (Result) then
  begin
    L := 0; CharBuffer := SpaceMarker;
    while ((not IsEoF) and (L < System.SizeOf(Buffer))) do
    begin
      ReadChar(CharBuffer);
      Buffer := Buffer + CharBuffer;
      Inc(L);
    end;
  end;
end;

function TCustomSDVANSITextBasedStream.WriteStr(const Buffer: shortansistring): Boolean;
var CharBuffer: ansichar; I, L: Integer;
begin
  Result := Assigned(Reference);
  if (Result) then
  begin
    L := System.Length(Buffer);
    for I := 1 to L do
    begin
      CharBuffer := Buffer[i];
      WriteChar(CharBuffer);
    end;
  end;
end;

function TCustomSDVANSITextBasedStream.LineMarkerFound(): Boolean;
var Buffer: ansichar;
begin
  Result := GetChar(Buffer) and (Buffer = LineMarker);
end;

function TCustomSDVANSITextBasedStream.PageMarkerFound(): Boolean;
var Buffer: ansichar;
begin
  Result := GetChar(Buffer) and (Buffer = PageMarker);
end;

function TCustomSDVANSITextBasedStream.FileMarkerFound(): Boolean;
var Buffer: ansichar;
begin
  Result := GetChar(Buffer) and (Buffer = FileMarker);
end;

function TCustomSDVANSITextBasedStream.WriteLineMarker(): Boolean;
begin
  Result := WriteChar(LineMarker);
end;

function TCustomSDVANSITextBasedStream.WritePageMarker(): Boolean;
begin
  Result := WriteChar(PageMarker);
end;

function TCustomSDVANSITextBasedStream.WriteFileMarker(): Boolean;
begin
  Result := WriteChar(FileMarker);
end;

function TCustomSDVANSITextBasedStream.GetChar
  (var Buffer: ansichar): Boolean;
begin
  Result := (Reference <> nil);
  if (Result) then
  begin
    if (not RecordStream.IsEoF()) then
    begin
      Result := RecordStream.GetRecord((@Buffer));
      //if (Buffer = brkLF) then
      if (Buffer = brkCR) then
      begin
        NextChar();
        Result := GetChar(Buffer);
      end;
      // ignore "Line Feed" character and read next char
      // ignorar caracter "Avance de Linea" y leer siguiente caracter
    end else Buffer := FileMarker;
  end;
end;

(* TCustomSDVAnsiSourceStream *)

function TCustomSDVAnsiSourceStream.LineMarkerFound(): Boolean;
var Buffer: ansichar;
begin
  Result := GetChar(Buffer) and (Buffer = LineMarker);
end;

function TCustomSDVAnsiSourceStream.PageMarkerFound(): Boolean;
begin
  Result := inherited PageMarkerFound();
end;

function TCustomSDVAnsiSourceStream.FileMarkerFound(): Boolean;
begin
  Result := inherited FileMarkerFound();
end;

function TCustomSDVAnsiSourceStream.WriteLineMarker(): Boolean;
begin
  Result := inherited WriteLineMarker();
end;

function TCustomSDVAnsiSourceStream.WritePageMarker(): Boolean;
begin
  Result := inherited WritePageMarker();
end;

function TCustomSDVAnsiSourceStream.WriteFileMarker(): Boolean;
begin
  Result := inherited WriteFileMarker();
end;

(* TCustomSDVAnsiTextStream *)

function TCustomSDVAnsiTextStream.IsEoLn(): Boolean;
begin
  Result := LineMarkerFound();
end;

function TCustomSDVAnsiTextStream.ReadLn(var Buffer: shortansistring): Boolean;
var CharBuffer: ansichar; L: Integer; BreakFound: Boolean;
begin
  System.FillChar(Buffer, System.SizeOf(Buffer), 0);

  Result := (FReference <> nil);
  if (Result) then
  begin
    L := 0; BreakFound := FALSE; CharBuffer := SpaceMarker;
    while ((not IsEoF) and (L < System.SizeOf(Buffer)) and (not BreakFound)) do
    begin
      ReadChar(CharBuffer);
      BreakFound := (CharBuffer in LineMarkers);

      if (not BreakFound) then
      begin
        Buffer := Buffer + CharBuffer;
      end;

      Inc(L);
    end;
  end;
end;

function TCustomSDVAnsiTextStream.WriteLn(const Buffer: shortansistring): Boolean;
begin
  Result := WriteStr(Buffer) and WriteLineMarker;
end;

end.
