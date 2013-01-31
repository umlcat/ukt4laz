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

unit uktossys;

(* Espanol *)

  // Objetivo: Provee funciones para el manejo del Sistema Operativo.

(* English *)

  // Goal: Provides functions for the Operative System management.

interface
uses
  // @to-do: make fully multiplatform
  Windows,
  SysUtils,
  uktstrings,
  dummy;

// IOResult constants

const
  ioOK = 0;
  ioFileNotFound = 1;
  ioFileAlreadyExists = 2;

(* global properties *)

    function getSystemPath(): string;
    function getSystemTempPath(): string;
    function getUserTempPath(): string;
    function getIOError(): Boolean;
    function getIOResult(): Integer;
    function getCurrentDrive(): string;

    procedure setSystemPath(const AValue: string);
    procedure setSystemTempPath(const AValue: string);
    procedure setUserTempPath(const AValue: string);
    procedure setIOError(const AValue: Boolean);
    procedure setIOResult(const AValue: Integer);
    procedure setCurrentDrive(const AValue: string);

(* global functions *)

    function FolderSeparator(): string;
    function ExtensionSeparator(): string;

    function RandomRange(AMin, AMax: Byte): Byte;

    function RandomFileName
      (const APath, AExt: string): string;
    function RandomFileNameExt
      (const APath: string): string;

    function RandomFileNameSize
      (const APath, AExt: string; const ASize: Byte): string;
    function RandomFileNameExtSize
      (const APath: string; const ASize: Byte): string;

    function DriveToInt(ADrive: char): Integer;
    function IntToDrive(ANumber: Integer): char;

    function FileFound(const AValue: string): Boolean;
    function DirFound(const AValue: string): Boolean;

//    function DiskSize(Drive: Byte): Int64;
//    function DiskFree(Drive: Byte): Int64;

    function FileSize(const Filename: string): Integer;

    function DirSizeNonRec
      (const Dir: string): Integer;
    function DirSizeRec
      (const Dir: string; Recursive: Boolean): Integer;

    function DirSize
      (const Dir: string): Integer; overload;
    function DirSize
      (const Dir: string; Recursive: Boolean): Integer; overload;

    procedure FileDelete(const Filename: string);
    procedure FileRename(const Source, Dest: string);
    procedure FileCopy
      (const Source, Dest: string; const Overwrite: Boolean = FALSE);
    procedure FileMove(const Source, Dest: string);

    procedure DirDelete(const AValue: string);
    procedure DirRename(const Source, Dest: string);
    procedure DirCopy(const Source, Dest: string);
    procedure DirMove(const Source, Dest: string);

    function ExecPath(): string;

    function ParamCount(): Integer;
    function ParamStr(Index: Integer): string;

    procedure ExecProc(const Command: string; Args: pointer);
    procedure ExecCmd(const Command, Args: string);

    procedure Nothing();
    procedure Halt(const ErrorCode: Integer);

(* module *)

    procedure UnitConstructor; // constructor SysOS.Init;
    procedure UnitDestructor;  // destructor SysOS.Done;

implementation

var
  FSystemPath: string;
  FSystemTempPath: string;
  FUserTempPath: string;

  FIOResult: Integer;
  FIOError: Boolean;

(* global properties *)

function getSystemPath(): string;
begin
  Result := SysUtils.GetCurrentDir;
  FSystemPath := Result;
end;

function getSystemTempPath(): string;
begin
  Result := FSystemTempPath;
  // Goal: Return the path of the temp folder,
  // for the O.S. .
  // Sometimes, its the same for all users, sometimes, its not.
end;

function getUserTempPath(): string;
begin
  Result := FUserTempPath;
  // Goal: Return the path of the temp folder,
  // for the current user.
  // Sometimes, its the same for all users, sometimes, its not.
end;

function getIOError(): Boolean;
begin
  Result := FIOError;
end;

function getIOResult(): Integer;
begin
  Result := FIOResult;
end;

function getCurrentDrive(): string;
var APath: string;
begin
  APath := SysUtils.GetCurrentDir;
  // Obtener ruta actual

  if (Length(APath) > 3)
    then Result := System.Copy(APath, 1, 3)
    else Result := '';
  // Goal: To obtain current drive.
  // Objetivo: Obtener la unidad de disco en uso.
end;

procedure setSystemPath(const AValue: string);
begin
  FSystemPath := AValue;
end;

procedure setSystemTempPath(const AValue: string);
begin
  FSystemTempPath := AValue;
  // Goal: Assigns the path of the temp folder,
  // for the O.S. .
  // Sometimes, its the same for all users, sometimes, its not.
  // Warning: Change it, with care.
end;

procedure setUserTempPath(const AValue: string);
begin
  FUserTempPath := AValue;
  // Goal: Assigns the path of the temp folder,
  // for the current user .
  // Sometimes, its the same for all users, sometimes, its not.
  // Warning: Change it, with care.
end;

procedure setIOError(const AValue: Boolean);
begin
  FIOError := AValue;
end;

procedure setIOResult(const AValue: Integer);
begin
  FIOResult := AValue;
  if (getIOError and (AValue > ioOK)) then
  begin
    RunError(AValue);
  end;
end;

procedure setCurrentDrive(const AValue: string);
begin
//
end;

(* global functions *)

function FolderSeparator(): string;
begin
  // @to-do: multiplatform
  Result := '/';
end;

function ExtensionSeparator(): string;
begin
  // @to-do: multiplatform
  Result := '.';
end;

function RandomRange(AMin, AMax: Byte): Byte;
begin
  Result := Random(AMax-AMin)+AMin;
  // Objetivo: Obtener un numero aleatorio en el rango indicado.
end;

function RandomFileName(const APath, AExt: string): string;
var AFullPath, AFileName: string;
begin
  Result := '';

  repeat
    AFileName := '_' +
      uktstrings.RandomAlphaString(7) + ExtensionSeparator() + AExt;

    AFullPath := APath + FolderSeparator() + AFileName;
  until (not FileExists(AFullPath));

  Result := AFileName;
  // Goal: Obtains a random alphabetic filename,
  // in the given path, with the default file extension.

  // Objetivo: Obtener un nombrearchivo alfabetico aleatorio,
  // en la ruta indicada, con la extension de archivo por defacto.
end;

function RandomFileNameExt(const APath: string): string;
var AFullPath, AFileName: string;
begin
  Result := '';

  repeat
    AFileName :=
      '_' + uktstrings.RandomAlphaString(7) + ExtensionSeparator() +
            uktstrings.RandomAlphaString(3);

    AFullPath := APath + FolderSeparator() + AFileName;
  until (not FileExists(AFullPath));

  Result := AFileName;
  // Goal: Obtains a random alphabetic filename,
  // in the given path, with the given file extension.

  // Objetivo: Obtener un nombrearchivo alfabetico aleatorio,
  // en la ruta indicada, con la extension de archivo indicada.
end;

function RandomFileNameSize
  (const APath, AExt: string; const ASize: Byte): string;
var AFullPath, AFileName: string;
begin
  Result := '';

  repeat
    AFileName :=
      '_' + uktstrings.RandomAlphaString(7) + ExtensionSeparator() + AExt;

    AFullPath := APath + FolderSeparator() + AFileName;
  until (not FileExists(AFullPath));

  Result := AFileName;
  // Goal: Obtains a random alphabetic filename,
  // in the given path, with the default file extension.

  // Objetivo: Obtener un nombrearchivo alfabetico aleatorio,
  // en la ruta indicada, con la extension de archivo por defacto.
end;

function RandomFileNameExtSize
  (const APath: string; const ASize: Byte): string;
var AFullPath, AFileName, AExtSep: string;
    ANameSize, ASepSize, AExtSize: Byte;
begin
  Result := '';

  AExtSep   := ExtensionSeparator();
  ASepSize  := Length(AExtSep);

  AExtSize  := 3;
  ANameSize := (ASize - ASepSize - AExtSize - 1);

  repeat
    AFileName :=
      '_' + uktstrings.RandomAlphaString(ANameSize) + AExtSep +
            uktstrings.RandomAlphaString(AExtSize);

    AFullPath := APath + FolderSeparator() + AFileName;
  until (not FileExists(AFullPath));

  Result := AFileName;
  // Goal: Obtains a random alphabetic filename,
  // in the given path, with the given file extension.

  // Objetivo: Obtener un nombrearchivo alfabetico aleatorio,
  // en la ruta indicada, con la extension de archivo indicada.
end;

function DriveToInt(ADrive: char): Integer;
begin
  Result := Ord( upcase(ADrive) ) - 64;
  // Goal: Return the drive's number.
  // Objetivo: Obtener el no. de la unidad de disco.
end;

function IntToDrive(ANumber: Integer): char;
begin
  Result := Chr(ANumber + 64);
  // Goal: Return the drive's number.
  // Objetivo: Obtener el no. de la unidad de disco.
end;

function FileFound(const AValue: string): Boolean;
begin
  Result := SysUtils.FileExists(AValue);
end;

function DirFound(const AValue: string): Boolean;
var PrevDir: string;
begin
  PrevDir := getCurrentDir;
  Result  := setCurrentDir(AValue);
  setCurrentDir(PrevDir);
end;

function FileSize(const Filename: string): Integer;
var F: File;
begin
  AssignFile(F, Filename);
  Reset(F);
    Result := System.FileSize(F);
  Close(F);
  // Goal: To get the size of a file by its' name.
  // Note: The file doesn't have to be open.
  // Note: Don't confused with "System.FileSize" function.

  // Objetivo: Obtener el tamano de un archivo por su nombre.
  // Nota: El archivo no necesita estar abierto.
  // Nota: No confundir con la funcion "System.FileSize".
end;

function DirSizeNonRec(const Dir: string): Integer;
{$IFDEF VER140}
platform;
{$ELSE}
{$ENDIF}
var ACount, ErrorCode: Integer; APath: string;
    SearchRec: TSearchRec; Attr: Integer; LastChar: char;
begin
  LastChar := Dir[System.Length(Dir)];
  if (LastChar = '\')
    then APath := ''
    else APath := '\';
  // Detect '\' character in case of root directory
  // Detectar caracter '\' en caso de directorio raiz

  APath := APath + Dir + '*.*';
  // Locate files in the indicated path
  // Buscar archivos en la ruta indicada

  Attr := faArchive;
  // Locate only files
  // Buscar solo archivos

  ACount := 0;
  ErrorCode := FindFirst(APath, Attr, SearchRec);
  while (ErrorCode = 0) do
  begin
    Inc( ACount, SearchRec.Size );
    ErrorCode := FindNext(SearchRec);
  end;
  FindClose(SearchRec);
  Result := ACount;
  // Goal: To get the size of a directory file by its' name.
  // Objetivo: Obtener el tamano de un directorio por su nombre.
end;

function DirSizeRec(const Dir: string; Recursive: Boolean): Integer;
begin
  // @ to-do: make it recursive
  Result := DirSizeNonRec(Dir);
end;

function DirSize(const Dir: string): Integer;
begin
  Result := DirSizeNonRec(Dir);
  // Goal: To get the size of a directory file by its' name.
  // Objetivo: Obtener el tamano de un directorio por su nombre.
end;

function DirSize(const Dir: string; Recursive: Boolean): Integer;
begin
  Result := DirSizeRec(Dir, Recursive);
  // Goal: To get the size of a directory file by its' name.
  // Objetivo: Obtener el tamano de un directorio por su nombre.
end;

procedure FileDelete(const Filename: string);
begin
  if (FileFound(Filename)) then
  begin
    SysUtils.DeleteFile(Filename);
    setIOResult(ioOK);
  end else setIOResult(ioFileNotFound);
  // Goal: The "DeleteFile" function erases the file named by "Filename"
  // from the disk. If the file cannot be deleted or does not exist,
  // the function returns "FALSE".

  // Objetivo: La funcion "DeleteFile" elimina el archivo llamado "Filename"
  // en el disco. Si el archivo no puede ser eliminado o no existe,
  // la funcion regresa "FALSE".
end;

procedure FileRename(const Source, Dest: string);
begin
  if (FileFound(Source)) then
  begin
    if (not FileFound(Dest)) then
    begin
      SysUtils.RenameFile(Source, Dest);
      setIOResult(ioOK);
    end  else setIOResult(ioFileAlreadyExists);
  end else setIOResult(ioFileNotFound);
end;

procedure FileCopy
  (const Source, Dest: string; const Overwrite: Boolean = FALSE);
begin
  if (FileFound(Source)) then
  begin
    if (FileFound(Dest) and not Overwrite) then
    begin
      Windows.CopyFile(PChar(Source), PChar(Dest), TRUE);
      setIOResult(ioOK);
    end else setIOResult(ioFileAlreadyExists);
  end else setIOResult(ioFileNotFound);
end;

procedure FileMove(const Source, Dest: string);
begin
//
end;

procedure DirDelete(const AValue: string);
begin
//
end;

procedure DirRename(const Source, Dest: string);
begin
//
end;

procedure DirCopy(const Source, Dest: string);
begin
//
end;

procedure DirMove(const Source, Dest: string);
begin
//
end;

function ExecPath(): string;
begin
  Result := System.ParamStr(0);
  // Goal: Returns the path where the program was executed.
  // Objetivo: Regresa la ruta en donde el programa fue ejecutado.
end;

function ParamCount(): Integer;
begin
  Result := System.ParamCount;
  // Goal: Returns the number of parameters passed to the program
  // on the command line.

  // Objetivo: Regresa el numero de parametros pasados al programa
  // en la linea de comandos.
end;

function ParamStr(Index: Integer): string;
begin
  Result := System.ParamStr(Index);
  // Goal: Returns the parameter from the command line
  // that corresponds to "Index".

  // Objetivo: Regresa el parametro de la linea de comandos
  // que corresponde a "Index".
end;

procedure ExecProc(const Command: string; Args: pointer);
begin
//
end;

procedure ExecCmd(const Command, Args: string);
begin
//
end;

procedure Nothing();
begin
  // Goal: Null procedure, does nothing.
  // Objetivo: Procedimiento nulo, hace nada.
end;

procedure Halt(const ErrorCode: Integer);
begin
  System.Halt(ErrorCode);
  // Goal: Terminate program execution.
  // Objetivo: Terminar la ejecucion del programa.
end;

(* module *)

procedure UnitConstructor;
var APath: string;
begin
  setIOError(TRUE);

  APath := '';
  setSystemTempPath(APath);
  APath := '';
  setUserTempPath(APath);
end;

procedure UnitDestructor;
begin
  setIOError(TRUE);
end;

initialization
  UnitConstructor;
finalization
  UnitDestructor;
end.

