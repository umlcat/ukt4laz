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

unit uktfoldersbyname;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  dummy;

{ global properties }

    function getSystemPath: string;

    procedure setSystemPath(const Value: string);

{ global functions }

    function FolderFound(const Value: string): Boolean;
    function FolderTemp(): string;

implementation

var
  FSystemPath: string;

{ global properties }

function getSystemPath: string;
begin
  Result := SysUtils.GetCurrentDir;
  FSystemPath := Result;
end;

procedure setSystemPath(const Value: string);
begin
  FSystemPath := Value;
end;

{ global functions }

function FolderFound(const Value: string): Boolean;
var PrevDir: string;
begin
  PrevDir := SysUtils.getCurrentDir;
  Result  := SysUtils.setCurrentDir(Value);
  setCurrentDir(PrevDir);
end;

function FolderTemp(): string;
begin
  // to-do: change the value
  Result := 'c:\temp';
  // Goal: Return a temporal folder with permisions to create
  // temporal files.

  // Objetivo: Obtener un folder temporal con permisos para crear
  // archivos temporales.
end;

end.
