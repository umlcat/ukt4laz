(**
 **************************************************************************
 *                                                                        *
 *  This file is part of the uktat Developer's Component Library.        *
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

unit uktpaths;

{$mode objfpc}{$H+}

interface

(**
 ** Description:
 ** Contains several utility functions,
 ** to change paths, without access to filesystem.
 **)

uses
  Classes, SysUtils,
  uktstrings,
  dummy;

(* types *)

type
  TParsePathRecord = record
    P: PChar;
  end;
type
  PParsePathRecord = ^TParsePathRecord;

(* global public functions *)

  function ChangeFileExt(const FileName, Extension: string): string;
  function ChangeFileDrive(const FileName, Drive: string): string;

  function RemoveFileExt(const FileName: string): string;

  function ExtractFileExt(const AValue: string): string;
  function ExtractFilename(const AValue: string): string;
  function ExtractFileDir(const AValue: string): string;
  function ExtractFileDrive(const AValue: string): string;

  function ExtractFileParentDir(const AValue: string): string;
  function ExtractFilePath(const AValue: string): string;
  function ExtractFileFullName(const AValue: string): string;

  function EncodePath(const aDir, aFile, aExt: string): string;
  procedure DecodePath(var Path: string; const aDir, aFile, aExt: string);

  function ExtractWildcard(var Value: PChar): string;

  procedure ParseStart(var Value: TParsePathRecord; const Path: string);
  function  ParseNext(var Value: TParsePathRecord; var S: string): Boolean;
  procedure ParseFinish(var Value: TParsePathRecord);

implementation

(* global public functions *)

function ChangeFileExt(const FileName, Extension: string): string;
begin
  Result := SysUtils.ChangeFileExt(Filename, Extension);
  // Goal: Returns a filename with extension replaced by 'Extension'.

  // Objetivo: Regresa un nombrearchivo con la extension
  // reemplazada por 'Extension'.
end;

function ChangeFileDrive(const FileName, Drive: string): string;
begin
  Result := '';
  // Goal: Returns a filename with drive replaced by 'Drive'.

  // Objetivo: Regresa un nombrearchivo con la unidad
  // reemplazada por 'Drive'.
end;

function RemoveFileExt(const FileName: string): string;
begin
  Result := SysUtils.ChangeFileExt(Filename, '');
  // Goal: Returns a filename without extension.}
  // Objetivo: Regresa un nombrearchivo sin extension.
end;

function ExtractFileExt(const AValue: string): string;
begin
  Result := SysUtils.ExtractFileExt(AValue);
  // Goal: Returns the extension of a path without drive,
  // directories or filename.

  // Objetivo: Regresa la extension de una ruta sin unidad,
  // directorios o nombrearchivo.
end;

function ExtractFilename(const AValue: string): string;
begin
  Result := SysUtils.ChangeFileExt(SysUtils.ExtractFileName(AValue), '');
  // Goal: Returns the filename of a path without drive,
  // directories or extension.

  // Objetivo: Regresa el nombrearchivo de una ruta sin unidad,
  // directorios o extension.
end;

function ExtractFileDir(const AValue: string): string;
begin
  Result := SysUtils.ExtractFileDir(AValue);
  // Goal: Returns the directories of a path without drive,
  // filename or extension.

  // Objetivo: Regresa los directorios de una ruta sin unidad,
  // nombrearchivo o extension.
end;

function ExtractFileDrive(const AValue: string): string;
begin
  Result := SysUtils.ExtractFileDrive(AValue);
  // Goal: Returns the drive of a path without directories,
  // filename or extension.

  // Objetivo: Regresa la unidad de una ruta sin directorios,
  // nombrearchivo o extension.
end;

function ExtractFileParentDir(const AValue: string): string;
var C: char; I, L: Integer; Match: Boolean;
begin
  Result := '';
  L := System.Length(AValue);
  I := L; Match := TRUE;
  while ((I > 0) and (Match)) do
  begin
    C := AValue[I];
    Match := not((C = ':') or (C = '\'));
    Dec(I);
  end;
  Inc(I, 2);
  Result := System.Copy(AValue, I, Succ(L-I));
  // Goal: Returns the parent directory of a path without drive,
  // path, filename or extension.

  // Objetivo: Regresa el directorio padre de una ruta sin unidad,
  // ruta, nombrearchivo o extension.
end;

function ExtractFilePath(const AValue: string): string;
begin
  Result := SysUtils.ExtractFilePath(AValue);
  // Goal: Returns the drive & directories of a path
  // without filename or extension.

  // Objetivo: Regresa la unidad y directorios de una ruta
  // sin nombrearchivo o extension.
end;

function ExtractFileFullName(const AValue: string): string;
begin
  Result := SysUtils.ExtractFileName(AValue);
  // Goal: Returns the filename & extension of a path
  // without drive or directories.

  // Objetivo: Regresa el nombrearchivo y extension de una ruta
  // sin unidad o directorios.
end;

function EncodePath(const aDir, aFile, aExt: string): string;
begin
  Result := aDir +  '\' + aFile + '.' + aExt;
end;

procedure DecodePath(var Path: string; const aDir, aFile, aExt: string);
begin
//
end;

function ExtractWildcard(var Value: PChar): string;
//const WildcardSet  = ['A'..'Z', 'a'..'z', '_', '0'..'9', '*', '?', '.'];
begin
  Result := '';
//  SkipBlanks(Value);
//  Result := ExtractWhile(Value, WildcardSet);
  // Objetivo: Regresa la 1era. subcadena de letras o digitos encontrado.
  // Goal: Returns the first letters or digits substrings found.
end;

procedure ParseStart(var Value: TParsePathRecord; const Path: string);
begin
  Value.P := PChar(Path);
  // Goal: Prepare resources for detection of objects in path.
  // Objetivo: Preparar recursos para reconocimiento de objetos en ruta.
end;

function ParseNext(var Value: TParsePathRecord; var S: string): Boolean;
begin
(*
  Result := FALSE;
  if (IsAssigned(OSRecord.P)) then
  begin
    SkipCharWhile(OSRecord.P, '.');
    // ignore dot separator
    // ignorar separador de punto

    S := ExtractWildcard(OSRecord.P);
    S := starStrings.ReplaceChar(S, ':', '.');
    Result := TRUE;
  end;
*)
  Result := TRUE;
  // Goal: Extracts an object id from a path.
  // Objetivo: Extrae un id de objeto de una ruta.
end;

procedure ParseFinish(var Value: TParsePathRecord);
begin
  Value.P := nil;
  // Goal: Unprepare resources for detection of objects in path.
  // Objetivo: Despreparar recursos para reconocimiento de objetos en ruta.
end;

end.

