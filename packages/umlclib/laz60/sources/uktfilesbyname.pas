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

unit uktfilesbyname;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  windows;

  function FileNameFound(const FullPath: string): Boolean;
  function FileNameFoundSens(const FullPath: string): Boolean;

  function FileNameSize(const FullPath: string): Int64;

  procedure FileNameRename(const SourceFullPath, DestPlainFilename: string);
  procedure FileNameCopy(const SourceFullPath, DestFullPath: string);
  procedure FileNameMove(const SourceFullPath, DestFullPath: string);
  procedure FileNameRemove(const FullPath: string);

  function FileNameRandomByPath( const APath, AExt: string ): string;

implementation

function FileNameFound(const FullPath: string): Boolean;
// ## hack {
var Handle: THandle;
// ## hack }
begin
  (* out *) Result := false;

  (*
  // bug:
  Result := SysUtils.FileExists(FullPath);
  *)
  // ## hack {
  Handle := FileOpen(FullPath, fmOpenRead);
  if (Handle <> 0) then
  begin
    FileClose(Handle);
  end;
  // ## hack }
end;

function FileNameFoundSens(const FullPath: string): Boolean;
begin
  (* out *) Result := false;

  Result := SysUtils.FileExists(FullPath);
end;

function FileNameSize(const FullPath: string): Int64;
{$IFDEF DELPHI}
var F: File;
{$ENDIF}
{$IFDEF FPC}
var F: File of char; AChar: char;
{$ENDIF}
begin
  (* out *) Result := 0;

  System.Assign(F, FullPath);
  System.Reset(F);

  if (InOutRes = 0) then
  begin
    {$IFDEF DELPHI}
    Result := System.FileSize(F);
    {$ENDIF}
    {$IFDEF FPC}
    while (not EoF(F)) do
    begin
      System.Read(F, AChar);
      Inc(Result);
    end;
    {$ENDIF}
  end;

  System.Close(F);
end;

procedure FileNameRename(const SourceFullPath, DestPlainFilename: string);
//var F: File;
begin
  SysUtils.RenameFile(SourceFullPath, DestPlainFilename);

  (*
  System.Assign(F, SourceFullPath);
  System.Reset(F);

  // to-do: fix path in "DestPlainFilename"

  if (InOutRes = 0) then
  begin
    System.Rename( (* var *) F, DestPlainFilename);
  end;

  System.Close(F);
  *)
end;

procedure FileNameCopy(const SourceFullPath, DestFullPath: string);
var CanCopy: Boolean;
begin
  CanCopy :=
    (FileNameFound(SourceFullPath) and (not FileNameFound(DestFullPath)));
  if (CanCopy) then
  begin
    // ...

    //SysUtils.DeleteFile(Filename);
    //setIOResult(ioOK);
  end;
end;

procedure FileNameMove(const SourceFullPath, DestFullPath: string);
begin
  if (FileNameFound(SourceFullPath)) then
  begin


    //SysUtils.DeleteFile(Filename);
    //setIOResult(ioOK);
  end;
end;

procedure FileNameRemove(const FullPath: string);
begin
  if (FileNameFound(FullPath)) then
  begin
    SysUtils.DeleteFile(FullPath);
    //setIOResult(ioOK);
  end; // else setIOResult(ioFileNotFound);

  // Goal: The "FileNameRemove" function erases the file named by "Filename"
  // from the disk.

  // Objetivo: La funcion "DeleteFile" elimina el archivo llamado "Filename"
  // en el disco.
end;

function InternalRandomRange( AMin, AMax: Byte ): Byte;
begin
  Result := Random(AMax - AMin) + AMin;
  // Objetivo: Obtener un numero aleatorio en el rango indicado.
end;

function InternalRandomString( ACount: Byte ): string;
var i: byte;
begin
  Result := StringOfChar( #32, ACount );
  for i := 1 to ACount do
    Result[i] :=  Chr( InternalRandomRange( 65, 90 ) );
  // Objetivo: Obtener una cadena de caracteres aleatorios,
  // con la longuitud indicada.
end;

function FileNameRandomByPath( const APath, AExt: string ): string;
var AFileName, ASepExt: string;
begin
  if (AExt[1] <> '.') then
  begin
    ASepExt := '.' + AExt;
  end else
  begin
    ASepExt := AExt;
  end;

  repeat
    AFileName := '_' + InternalRandomString(7) + ASepExt;
  until not FileNameFound(APath + AFileName);
  Result := AFileName;
  // Objetivo: Obtener una cadena de caracteres aleatorios,
  // con la longuitud indicada.
end;

end.
