unit uktossearchfiles;

interface
uses
{$IFDEF MSWINDOWS}
  Windows, Messages,
{$ENDIF}
{$IFDEF LINUX}
  Types, Libc,
{$ENDIF}
  SysUtils, Variants, Classes,
  uktossys, uktpaths,
  dummy;

type

  TSearchState = (ssInit, ssNext, ssDone);

  PSearchFilesRecord = ^TSearchFilesRecord;
  TSearchFilesRecord = record
    Path:         string;
    Filename:     string;
    Extensions:   array of string;
    Index:        Integer;
    State:        TSearchState;
    Attr:         Integer;
    IsFirst:      Boolean;
    IsMultiple:   Boolean;
    DOSRec:       TSearchRec;
  end;

  PFileRecord = ^TFileRecord;
  TFileRecord = record
    FileName: string;
    FileExt:  string;

    IsParentFolder: Boolean;
    IsOwnFolder:    Boolean;
    IsFolder:       Boolean;
  end;

  procedure FileSearchSingleInit
    (var ASearchFilesRecord: TSearchFilesRecord; const WildcardPath: string; Attr: Integer);
  procedure FileSearchManyInit
    (var ASearchFilesRecord: TSearchFilesRecord;
     const WildcardPath: string; Ext: array of string; Attr: Integer);

  procedure FileSearchInit
    (var ASearchFilesRecord: TSearchFilesRecord; const WildcardPath: string; Attr: Integer); overload;
  procedure FileSearchInit
    (var ASearchFilesRecord: TSearchFilesRecord;
     const WildcardPath: string; Ext: array of string; Attr: Integer); overload;

  function FileSearchNext(var Value: TSearchFilesRecord; var F: TFileRecord): Boolean;

  procedure FileSearchDone(var Value: TSearchFilesRecord);

implementation

procedure FileSearchSingleInit
 (var ASearchFilesRecord: TSearchFilesRecord;
  const WildcardPath: string; Attr: Integer);
begin
  System.FillByte(ASearchFilesRecord, SizeOf(ASearchFilesRecord), 0);

  ASearchFilesRecord.Path       := WildcardPath;
  ASearchFilesRecord.Filename   := WildcardPath;
  ASearchFilesRecord.Index      := -1;
  ASearchFilesRecord.State      := ssInit;
  ASearchFilesRecord.Attr       := Attr;
  ASearchFilesRecord.IsMultiple := FALSE;
  ASearchFilesRecord.IsFirst    := TRUE;
  // Goal: Prepare resources for searching files.
  // Objetivo: Preparar recursos para buscar archivos. }
end;

procedure FileSearchManyInit
 (var ASearchFilesRecord: TSearchFilesRecord;
  const WildcardPath: string; Ext: array of string; Attr: Integer);
var I, Count: Integer;
begin
  System.FillByte(ASearchFilesRecord, SizeOf(ASearchFilesRecord), 0);

  ASearchFilesRecord.Path       := WildcardPath;
  ASearchFilesRecord.Filename   := '';
  ASearchFilesRecord.Index      := -1;
  ASearchFilesRecord.State      := ssInit;
  ASearchFilesRecord.Attr       := Attr;
  ASearchFilesRecord.IsMultiple := TRUE;

  Count := High(Ext);
  System.setLength(ASearchFilesRecord.Extensions, Succ(Count));
  for I := 0 to Count do
  begin
    ASearchFilesRecord.Extensions[i] := Ext[i];
  end;
  // Goal: Prepare resources for searching files.
  // Objetivo: Preparar recursos para buscar archivos. }
end;

procedure FileSearchInit
 (var ASearchFilesRecord: TSearchFilesRecord; const WildcardPath: string; Attr: Integer);
begin
  FileSearchSingleInit(ASearchFilesRecord, WildcardPath, Attr);
end;

procedure FileSearchInit
 (var ASearchFilesRecord: TSearchFilesRecord;
  const WildcardPath: string; Ext: array of string; Attr: Integer);
begin
  FileSearchManyInit(ASearchFilesRecord, WildcardPath, Ext, Attr);
end;

function SearchSingle(var ASearchFilesRecord: TSearchFilesRecord; var F: TFileRecord): Boolean;
var IsDir, IsFirstLoop: Boolean;
begin
  IsFirstLoop := ASearchFilesRecord.IsFirst;
  with ASearchFilesRecord do
  begin
    if (IsFirstLoop) then
    begin
      //Result  := (SysUtils.FindFirst(Filename, faAnyFile, DOSRec) = 0);

      Result  := (SysUtils.FindFirst(Filename, Attr, DOSRec) = 0);
      // get the info from the DOS interface
      // obtener la informacion de la interface DOS

      IsFirst := FALSE;
    end else
    begin
      Result := (SysUtils.FindNext(DOSRec) = 0);
      // get the info from the DOS interface
      // obtener la informacion de la interface DOS
    end;

    if (Result) then
    begin
      // detect if current entry is own folder link
      F.IsOwnFolder  := (DOSRec.Name = '.');

      // detect if current entry is parent folder link
      F.IsParentFolder := (DOSRec.Name = '..');

      // detect if current entry is a folder
      IsDir := ((DOSRec.Attr and faDirectory) <> 0);
      F.IsFolder := IsDir;

      // extract filename without path or extension
      if ((F.IsOwnFolder) or (F.IsParentFolder))
        then F.FileName := DOSRec.Name
        else F.FileName := uktpaths.ExtractFileName(DOSRec.Name);

      // extract extension without path or filename
      if (not IsDir) then
      begin
        F.FileExt := uktpaths.ExtractFileExt(DOSRec.Name);
      end;
    end;
  end;
  // Goal: Return the next file record.
  // Objetivo: Regresar el siguiente registro de archivo.
end;

function SearchMany(var ASearchFilesRecord: TSearchFilesRecord; var F: TFileRecord): Boolean;
begin
  with ASearchFilesRecord do
  case State of
    ssInit:
      begin
        Inc(Index);
        State    := ssNext;
        IsFirst  := TRUE;

        // @to-do: check folder sep.
        Filename := Path + '\' + Extensions[Index];

        Result := SearchSingle(ASearchFilesRecord, F);
        if (not Result) then
        begin
          State := ssDone;
          Result := SearchMany(ASearchFilesRecord, F);
        end;
      end;
    ssNext:
      begin
        Result := SearchSingle(ASearchFilesRecord, F);
        if (not Result) then
        begin
          State := ssDone;
          Result := SearchMany(ASearchFilesRecord, F);
        end;
      end;
    ssDone:
      begin
        if (Index = High(Extensions))
          then Result := FALSE
          else
            begin
              State := ssInit;
              Result := SearchMany(ASearchFilesRecord, F);
            end;
      end;
    else  Result := FALSE;
  end;
  // Goal: Return the next file record.
  // Objetivo: Regresar el siguiente registro de archivo.
end;

function FileSearchNext(var Value: TSearchFilesRecord; var F: TFileRecord): Boolean;
begin
  if (Value.IsMultiple)
    then Result := SearchMany(Value, F)
    else Result := SearchSingle(Value, F);
  // Goal: Return the next file record.
  // Objetivo: Regresar el siguiente registro de archivo.
end;

procedure FileSearchDone(var Value: TSearchFilesRecord);
begin
  System.FillByte(Value, SizeOf(Value), 0);
  // Goal: Unprepare resources for searching files.
  // Objetivo: Despreparar recursos para buscar archivos. }
end;

end.
