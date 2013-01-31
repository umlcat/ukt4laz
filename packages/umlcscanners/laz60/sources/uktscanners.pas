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

unit uktscanners;

interface
uses
  SysUtils, Classes,
  uktactivatedcontrols,
  uktcomponents,
  uktstreams,
  uktscannerstates,
  uktscanneroptions,
  dummy;

type

(* TSDVScannerOptions *)

  TSDVScannerOptions = class(TSDVNormalizedComponent)
  private
    (* private declarations *)
  protected
    (* protected declarations *)

    (* fields declarations *)

    FSpecials: TSDVScannerOption;
    FTabs:     TSDVScannerOption;
    FSpaces:   TSDVScannerOption;
    FEoLn:     TSDVScannerOption;
    FEoPg:     TSDVScannerOption;
    FEoF:      TSDVScannerOption;
  protected
    (* protected declarations *)

    (* accesors declarations *)

    function getSpecials(): TSDVScannerOption;
    function getTabs(): TSDVScannerOption;
    function getSpaces(): TSDVScannerOption;
    function getEoLn(): TSDVScannerOption;
    function getEoPg(): TSDVScannerOption;
    function getEoF(): TSDVScannerOption;

    procedure setSpecials(const Value: TSDVScannerOption);
    procedure setTabs(const Value: TSDVScannerOption);
    procedure setSpaces(const Value: TSDVScannerOption);
    procedure setEoLn(const Value: TSDVScannerOption);
    procedure setEoPg(const Value: TSDVScannerOption);
    procedure setEoF(const Value: TSDVScannerOption);
  public
    (* public declarations *)

    constructor Create(AOwner: TComponent); override;

    procedure Clear();
  public
    (* public declarations *)

    property Specials: TSDVScannerOption
      read getSpecials write setSpecials;
    property Tabs: TSDVScannerOption
      read getTabs write setTabs;
    property Spaces: TSDVScannerOption
      read getSpaces write setSpaces;
    property EoLn: TSDVScannerOption
      read getEoLn write setEoLn;
    property EoPg: TSDVScannerOption
      read getEoPg write setEoPg;
    property EoF: TSDVScannerOption
      read getEoF write setEoF;
  end;

(* TCustomSDVScanner *)

  TCustomSDVScanner = class(TComponent)
  private
    (* private declarations *)
  protected
    (* protected declarations *)

    (* fields declarations *)

    FInternalOptions: TSDVScannerOptions;

    FStream: TCustomSDVStream;

    FCurrentRow: Integer;
    FCurrentCol: Integer;
  protected
    (* protected declarations *)

    (* accesors declarations *)

    function getCurrentRow(): Integer; virtual;
    function getCurrentCol(): Integer; virtual;

    function getInternalOptions(): TSDVScannerOptions; virtual;

    procedure setCurrentRow(const Value: Integer); virtual;
    procedure setCurrentCol(const Value: Integer); virtual;

    procedure setInternalOptions(const Value: TSDVScannerOptions); virtual;
  protected
    (* protected declarations *)

    function CreateOptions(): TSDVScannerOptions; virtual;

    procedure Notification
      (AComponent: TComponent; Operation: TOperation); override;
  protected
    (* protected declarations *)

    (* properties declarations *)

    property InternalOptions: TSDVScannerOptions
      read getInternalOptions write setInternalOptions;
  public
    (* public declarations *)

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    (* public declarations *)

    function Start(): Boolean; virtual;
    function Next(): Boolean; virtual;
    function Finish(): Boolean; virtual;
  public
    (* public declarations *)

    (* read-only properties declarations *)

    function Options(): TSDVScannerOptions;
  public
    (* public declarations *)

    (* property declarations *)

    property CurrentRow: Integer
      read getCurrentRow write setCurrentRow;
    property CurrentCol: Integer
      read getCurrentCol write setCurrentCol;
  end;

implementation

(* TSDVScannerOptions *)

function TSDVScannerOptions.getTabs: TSDVScannerOption;
begin
  Result := FTabs;
end;

function TSDVScannerOptions.getSpecials(): TSDVScannerOption;
begin
  Result := FSpecials;
end;

function TSDVScannerOptions.getSpaces(): TSDVScannerOption;
begin
  Result := FSpaces;
end;

function TSDVScannerOptions.getEoLn(): TSDVScannerOption;
begin
  Result := FEoLn;
end;

function TSDVScannerOptions.getEoPg(): TSDVScannerOption;
begin
  Result := FEoPg;
end;

function TSDVScannerOptions.getEoF(): TSDVScannerOption;
begin
  Result := FEoF;
end;

procedure TSDVScannerOptions.setSpecials(const Value: TSDVScannerOption);
begin
  FSpecials := Value;
end;

procedure TSDVScannerOptions.setTabs(const Value: TSDVScannerOption);
begin

end;

procedure TSDVScannerOptions.setSpaces(const Value: TSDVScannerOption);
begin
  FSpaces := Value;
end;

procedure TSDVScannerOptions.setEoLn(const Value: TSDVScannerOption);
begin
  FEoLn := Value;
end;

procedure TSDVScannerOptions.setEoPg(const Value: TSDVScannerOption);
begin
  FEoPg := Value;
end;

procedure TSDVScannerOptions.setEoF(const Value: TSDVScannerOption);
begin
  FEoF := Value;
end;

constructor TSDVScannerOptions.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Clear;
end;

procedure TSDVScannerOptions.Clear();
begin
  FSpecials := scnopIgnoreTag;
  FTabs     := scnopIgnoreTag;
  FSpaces   := scnopIgnoreTag;
  FEoLn     := scnopIgnoreTag;
  FEoPg     := scnopIgnoreTag;
  FEoF      := scnopIgnoreTag;
end;

(* TCustomSDVScanner *)

function TCustomSDVScanner.getCurrentRow(): Integer;
begin
  Result := FCurrentRow;
end;

function TCustomSDVScanner.getCurrentCol(): Integer;
begin
  Result := FCurrentCol;
end;

function TCustomSDVScanner.getInternalOptions(): TSDVScannerOptions;
begin
  Result := FInternalOptions;
end;

procedure TCustomSDVScanner.setCurrentRow(const Value: Integer);
begin
  if (Value <> FCurrentRow) then
  begin
    FCurrentRow := Value;
  end;
end;

procedure TCustomSDVScanner.setCurrentCol(const Value: Integer);
begin
  if (Value <> FCurrentCol) then
  begin
    FCurrentCol := Value;
  end;
end;

procedure TCustomSDVScanner.setInternalOptions(const Value: TSDVScannerOptions);
begin
//  if (Value <> FInternalOptions) then
//  begin
    FInternalOptions := Value;
//  end;
end;

function TCustomSDVScanner.CreateOptions(): TSDVScannerOptions;
begin
  Result := TSDVScannerOptions.Create(Self);
end;

procedure TCustomSDVScanner.Notification
  (AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) then
  begin
    if (FStream = AComponent)
      then FStream := nil;
  end;
end;

constructor TCustomSDVScanner.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStream  := nil;
  FInternalOptions := CreateOptions;
end;

destructor TCustomSDVScanner.Destroy;
begin
//  FInternalOptions.Free;
  FStream := nil;
  inherited Destroy;
end;

function TCustomSDVScanner.Start(): Boolean;
begin
  Result := FALSE;
end;

function TCustomSDVScanner.Next(): Boolean;
begin
  Result := FALSE;
end;

function TCustomSDVScanner.Finish(): Boolean;
begin
  Result := FALSE;
end;

function TCustomSDVScanner.Options(): TSDVScannerOptions;
begin
  Result := Self.FInternalOptions;
end;

end.
