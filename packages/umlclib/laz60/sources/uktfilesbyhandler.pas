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

unit uktfilesbyhandler;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uktstrings,
  uktossys,
  dummy;

(* FileRec *)

  function IsEoF(var F: File): Boolean;
  function IsAssigned(var F: File): Boolean;
  function IsClosed(var F: File): Boolean;

  function FileRecFound(var F: File): Boolean;
  function FileRecSize(var F: File): Int64;
  function FileRecMode(var F: File): Integer;
  function FileRecPath(var F: File): string;

  procedure FileRecAssign(out F: File; const FullPath: string);
  procedure FileRecReset(out F: File);
  procedure FileRecRewrite(out F: File);
  procedure FileRecClose(out F: File);

  procedure FileRecRename(var F: File; const NewFilename: string);
  procedure FileRecRemove(var F: File);

  function FileRecResetUserTemp
    (out F: File; out FileNameExt: string): boolean;

(* BlockFile *)

type
  BlockFile = File of Byte;

  function BlockFileIsEoF(var F: BlockFile): Boolean;
  function BlockFileIsAssigned(var F: BlockFile): Boolean;
  function BlockFileIsClosed(var F: BlockFile): Boolean;

  function BlockFileFound(var F: File): Boolean;
  function BlockFileSize(var F: File): Int64;
  function BlockFileMode(var F: BlockFile): Integer;
  function BlockFilePath(var F: File): string;

  procedure BlockFileAssign(out F: File; const FullPath: string);

  procedure BlockFileReset
    (var F: BlockFile; const RecordSize: Integer);
  procedure BlockFileRewrite
    (var F: BlockFile; const RecordSize: Integer);

  procedure BlockFileClose(var F: BlockFile);

  procedure BlockFileRead
    (var F: BlockFile; const Buffer: (* var *) pointer; Count: Int64;
     out RecordReaded: Int64);
  procedure BlockFileWrite
    (var F: BlockFile; const Buffer: (* const *) pointer; Count: Int64;
     out RecordWritten: Int64);

  procedure BlockFileRemove(var F: BlockFile);

implementation  

(* FileRec *)

function IsEoF(var F: File): Boolean;
begin
  (* out *) Result := false;

  Result := System.EoF(F);
end;

function IsAssigned(var F: File): Boolean;
begin
  (* out *) Result := false;

  Result := (FileRec(f).Name[1] <> #0);
end;

function IsClosed(var F: File): Boolean;
begin
  (* out *) Result := false;

  Result := (FileRec(F).mode = fmClosed);
end;

function FileRecMode(var F: File): Integer;
begin
  (* out *) Result := 0;

  Result := FileRec(F).mode;
end;

function FileRecPath(var F: File): string;
begin
  (* out *) Result := '';

  if (IsAssigned(F)) then
  begin


    // ...

    //Result := SysUtils.FileExists(FullPath);
  end;
end;

function FileRecFound(var F: File): Boolean;
var FullPath: string;
begin
  (* out *) Result := false;

  if (IsAssigned(F)) then
  begin
     FullPath := BlockFilePath(F);
     Result := (SysUtils.FileExists(FullPath));
  end;
end;

function FileRecSize(var F: File): Int64;
begin
  (* out *) Result := 0;

  Result := System.FileSize(F);
end;

procedure FileRecAssign(out F: File; const FullPath: string);
begin
  System.Assign((* var *) F, FullPath);
  FileRec(f).Name[0] := chr(Length(FullPath));
end;

procedure FileRecReset(out F: File);
begin
  System.Reset(F);
end;

procedure FileRecRewrite(out F: File);
begin
  System.Rewrite(F);
end;

procedure FileRecClose(out F: File);
begin
  System.Close(F);
end;

procedure FileRecRename(var F: File; const NewFilename: string);
begin
  System.Rename((* var *) F, NewFilename);
end;

procedure FileRecRemove(var F: File);
var FullPath: string; CanRemove: Boolean;
begin
   CanRemove :=
      (FileRecFound(F) and IsClosed(F));
   if (CanRemove) then
   begin
     FullPath := BlockFilePath(F);
     SysUtils.DeleteFile(FullPath);
   end;
end;

function FileRecResetUserTemp
  (out F: File; out FileNameExt: string): boolean;
var APath, AFullPath: string;
begin
  Result := true;

  APath := uktossys.getUserTempPath();
  FileNameExt := uktossys.RandomFileNameExt(APath);

  AFullPath := APath + uktossys.FolderSeparator() + FileNameExt;
  FileRecAssign(F, AFullPath);
  FileRecReset(F);
  // Goal: Obtains a temporal filename in the "UserTemp" folder,
  // and recreates a new file,
  // do not care about file extension.
  // do not care about filename size.
end;

(* BlockFile *)

function BlockFileIsEoF(var F: BlockFile): Boolean;
begin
  (* out *) Result := false;

  Result := System.EoF(F);
end;

function BlockFileIsAssigned(var F: BlockFile): Boolean;
begin
  (* out *) Result := false;

  Result := (FileRec(f).Name[1] <> #0);
end;

function BlockFileIsClosed(var F: BlockFile): Boolean;
begin
  (* out *) Result := false;

  Result := (FileRec(F).mode = fmClosed);
end;

function BlockFileFound(var F: File): Boolean;
var FullPath: string;
begin
  (* out *) Result := false;

  if (IsAssigned(F)) then
  begin
     FullPath := BlockFilePath(F);
     Result := (SysUtils.FileExists(FullPath));
  end;
end;

function BlockFileSize(var F: File): Int64;
begin
  (* out *) Result := 0;

  Result := System.FileSize(F);
end;

function BlockFileMode(var F: BlockFile): Integer;
begin
  (* out *) Result := 0;

  Result := FileRec(F).mode;
end;

function LengthSize(const Value: pchar; const MaxSize: Integer): Int64;
var P: pchar;
begin
  Result := 0;

  if (Value <> NIL) then
  begin
    P := Value;
    while ((P^ <> #0) and (Result < MaxSize)) do
    begin
      Inc(P);
      Inc(Result);
    end;
  end;
  // Objetivo: Regresa la cantidad de caracteres de una cadena.

  // Goal: Returns the character count of a string.
end;

procedure NullStrToStringSize
  (const Source: pchar; var Dest: string; const MaxSize: Int64);
var S: pchar; Count: Integer;
begin
  (* out *) Dest := '';
  if (Source <> nil) then
  begin
    S := Source;
    Count := 0;
    while ((S^ <> #0) and (Count < MaxSize)) do
    begin
      Dest := Dest + S^;

      Inc(S);
      Inc(Count);
    end;
  end;
  // Objetivo: Copia cierta cantidad de caracteres de un arreglo a una cadena,
  // siempre y cuando no haya un caracter nulo antes.
  // El arreglo puede no estar delimitado por el caracter nulo.

  // Goal: Copies some quantity of characters from an array to a string,
  // as long as there is not a null character before.
  // The array doesn't have to be delimited by a null character.
end;

function BlockFilePath(var F: File): string;
var ALen, AMaxLen: Int64; Source: pchar;
begin
  (* out *) Result := '';

  if (IsAssigned(F)) then
  begin
    Source  := pchar(@(FileRec(f).Name));
    AMaxLen := (SizeOf(FileRec(f).Name) - 1);
    ALen    := LengthSize(Source, AMaxLen);

    if (ALen > 0) then
    begin
      SetLength(Result, ALen);
      NullStrToStringSize(Source, Result, ALen);
    end;
  end;
end;

procedure BlockFileAssign(out F: File; const FullPath: string);
begin
  System.Assign(F, FullPath);
end;

procedure BlockFileReset
  (var F: BlockFile; const RecordSize: Integer);
begin
  System.Reset(F, RecordSize);
  // Goal: Opens an existing file for operations with blocks.

  // Objetivo: Reabre un archivo ya existente,
  // para operaciones por lote.
end;

procedure BlockFileRewrite
  (var F: BlockFile; const RecordSize: Integer);
begin
  System.Rewrite(F, RecordSize);
  // Goal: Recreates a file for operations with blocks.

  // Objetivo: Recrea un archivo,
  // para operaciones por lote.
end;

procedure BlockFileClose(var F: BlockFile);
begin
  System.Truncate(F);
  System.Close(F);
  // Goal: Closes a file for operations with blocks.
  // Objetivo: Cierra un archivo para operaciones por lote.
end;

procedure BlockFileRead
  (var F: BlockFile; const Buffer: (* var *) pointer; Count: Int64;
   out RecordReaded: Int64);
var j: Integer; P: pbyte;
begin
  (* out *) RecordReaded := 0;

  P := Buffer;

  j := 0;
  while ((not BlockFileIsEoF(F)) and (J < Count)) do
  begin
    System.Read(F, P^);

    Inc(J);
    Inc(RecordReaded);
    Inc(P);
  end;
end;

procedure BlockFileWrite
  (var F: BlockFile; const Buffer: (* const *) pointer; Count: Int64;
   out RecordWritten: Int64);
var j: Integer; P: pbyte;
begin
  (* out *) RecordWritten := 0;

  P := Buffer;
  for j := 1 to Count do
  begin
    System.Write(F, P^);

    Inc(RecordWritten);
    Inc(P);
  end;
end;

procedure BlockFileRemove(var F: BlockFile);
var FullPath: string; CanRemove: Boolean;
begin
   CanRemove :=
      (BlockFileFound(F) and BlockFileIsClosed(F));
   if (CanRemove) then
   begin
     FullPath := BlockFilePath(F);
     SysUtils.DeleteFile(FullPath);
   end;
end;



end.
