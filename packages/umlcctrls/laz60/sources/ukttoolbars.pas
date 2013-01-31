unit ukttoolbars;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  GraphType,
  Graphics, Controls,
  contnrs,
  uktpanels, uktspeedbtns,
  dummy;

type
  TButtonIconSize =
  (
    biTiny,      // 16
    biSmall,     // 24
    biMedium,    // 32
    biLarge,     // 64
    biExtraLarge // 128
  );

  // ...
  TCustomSDVToolBarGroup = class;
  // ...
  TSDVToolBarRow = class;
  // ...
  TSDVToolBar = class;

(* ISDVToolBarControl *)

  ISDVToolBarControl = interface
    ['{8494B82A-D12A-C248-8446-140A84A1C1BA}']

    function readToolBarRow(): TSDVToolBarRow;
    function readToolBarGroup(): TCustomSDVToolBarGroup;
  end;

(* TSDVToolButton *)

  TSDVToolButton = class(TCustomSDVSpeedButton, ISDVToolBarControl)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)

    FToolBarRow:   TSDVToolBarRow;
    FToolBarGroup: TCustomSDVToolBarGroup;
  protected
    (* Protected declarations *)

    function readToolBarRow(): TSDVToolBarRow;
    function readToolBarGroup(): TCustomSDVToolBarGroup;
  public
    (* Public declarations *)

    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
  public
    (* Public declarations *)

    procedure AssignToolBarRow(const AValue: TSDVToolBarRow);
    procedure AssignToolBarGroup(const AValue: TCustomSDVToolBarGroup);
  end;

(* TSDVToolBar *)

  TSDVToolBar = class(TCustomSDVPanel)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)

    FToolControls: TFPObjectList;
    FCounter:     Integer;
  public
    (* Public declarations *)

    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
  public
    (* Public declarations *)

    function InsertToolButton(): TSDVToolButton;

    function ControlCount(): Integer;
    function ControlAt(const AIndex: Integer): TSDVToolButton;
  end;

(* TSDVToolBarRow *)

  TSDVToolBarRow = class(TCustomSDVPanel)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)

    FToolBars: TFPObjectList;
    FCounter:  Integer;
  public
    (* Public declarations *)

    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
  public
    (* Public declarations *)

    function InsertToolBar(): TSDVToolBar;

    function ToolBarCount(): Integer;
    function ToolBarAt(const AIndex: Integer): TSDVToolBar;
  end;

(* TCustomSDVToolBarGroup *)

  TCustomSDVToolBarGroup = class(TCustomSDVPanel)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)

    FToolBarRows: TFPObjectList;

    FCounter: Integer;
  protected
    (* Protected declarations *)

    procedure ActivateFirst(); override;
  public
    (* Public declarations *)

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    (* Public declarations *)

    function InsertToolBarRow(): TSDVToolBarRow;

    function ToolBarRowCount(): Integer;
    function ToolBarRowAt(const AIndex: Integer): TSDVToolBarRow;
  end;

(* TSDVToolBarGroup *)

  TSDVToolBarGroup = class(TCustomSDVToolBarGroup)
  published
    (* Published declarations *)

    (* TCustomSDVPanel: *)

{$IFDEF MSWINDOWS}
    property DragCursor;
    property DragKind;
    {$IFDEF DELPHI}
    property Ctl3D;
    property ParentCtl3D;
    {$ENDIF}

    property OnEndDock;
    property OnStartDock;
{$ENDIF}

    (* TCustomPanel: *)

    property Align;
    property Anchors;
    property Constraints;
    property DragMode;
    property Enabled;
    property ParentShowHint;
    property PopupMenu;
    property TabOrder;
    property TabStop;
    property Visible;

    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnResize;
    property OnStartDrag;

    (* TCustomSDVToolBarGroup: *)

  end;

implementation

(* TSDVToolButton *)

function TSDVToolButton.readToolBarRow(): TSDVToolBarRow;
begin
  Result := FToolBarRow;
end;

function TSDVToolButton.readToolBarGroup(): TCustomSDVToolBarGroup;
begin
  Result := FToolBarGroup;
end;

constructor TSDVToolButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FToolBarRow   := nil;
  FToolBarGroup := nil;

  Self.Flat  := true;
  Self.Width := 100;
end;

destructor TSDVToolButton.Destroy();
begin
  inherited Destroy();
end;

procedure TSDVToolButton.AssignToolBarRow
  (const AValue: TSDVToolBarRow);
begin
  FToolBarRow := AValue;
end;

procedure TSDVToolButton.AssignToolBarGroup
  (const AValue: TCustomSDVToolBarGroup);
begin
  FToolBarGroup := AValue;
end;

(* TSDVToolBar *)

constructor TSDVToolBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Self.BorderWidth := 0;
  Self.BevelInner  := TGraphicsBevelCut.bvNone;
  Self.BevelOuter  := TGraphicsBevelCut.bvNone;
  Self.Height      := 21;
  Self.ParentColor := false;
  Self.Color := Graphics.clCream;

  FToolControls := TFPObjectList.Create(false);
  FCounter := 0;
end;

destructor TSDVToolBar.Destroy();
begin
  FToolControls.Free();
  inherited Destroy();
end;

function TSDVToolBar.InsertToolButton(): TSDVToolButton;
begin
  Result := nil;

  Result := TSDVToolButton.Create(Self);
  Result.Top   := 0;
  Result.Left  := 0;
  Result.Align := TAlign.alLeft;

  Result.Name  := 'ToolButton' + IntToStr(FCounter);
  Inc(FCounter);

  Self.InsertControl(Result);
  Result.Caption := '';

  FToolControls.Add(Result);
end;

function TSDVToolBar.ControlCount(): Integer;
begin
  Result := FToolControls.Count;
end;

function TSDVToolBar.ControlAt(const AIndex: Integer): TSDVToolButton;
var ACount: Integer;
begin
  Result := nil;
  ACount := FToolControls.Count;
  if ((AIndex >= 0) and (AIndex <= ACount)) then
  begin
    Result := TSDVToolButton(FToolControls[AIndex]);
  end;
end;

(* TSDVToolBarRow *)

constructor TSDVToolBarRow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Self.BorderWidth := 0;
  Self.BevelInner  := TGraphicsBevelCut.bvNone;
  Self.BevelOuter  := TGraphicsBevelCut.bvNone;
  Self.Height      := 21;
  Self.ParentColor := false;
  Self.Color := Graphics.clLtGray;

  FToolBars := TFPObjectList.Create(false);
  FCounter := 0;
end;

destructor TSDVToolBarRow.Destroy();
begin
  FToolBars.Free();
  inherited Destroy();
end;

function TSDVToolBarRow.InsertToolBar(): TSDVToolBar;
begin
  Result := nil;

  Result := TSDVToolBar.Create(Self);
  Result.Top   := 0;
  Result.Left  := 0;
  Result.Align := TAlign.alLeft;

  Result.Name  := 'ToolBar' + IntToStr(FCounter);
  Inc(FCounter);

  Self.InsertControl(Result);
  Result.Caption := '';

  FToolBars.Add(Result);
end;

function TSDVToolBarRow.ToolBarCount(): Integer;
begin
  Result := FToolBars.Count;
end;

function TSDVToolBarRow.ToolBarAt
  (const AIndex: Integer): TSDVToolBar;
var ACount: Integer;
begin
  Result := nil;
  ACount := FToolBars.Count;
  if ((AIndex >= 0) and (AIndex <= ACount)) then
  begin
    Result := TSDVToolBar(FToolBars[AIndex]);
  end;
end;

(* TCustomSDVToolBarGroup *)

procedure TCustomSDVToolBarGroup.ActivateFirst();
var AToolBar: TSDVToolBarRow;
begin
  //inherited ActivateFirst();
  AToolBar := InsertToolBarRow();
end;

constructor TCustomSDVToolBarGroup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Self.Color := Graphics.clDkGray;
  //Self.BorderWidth := 0;
  //Self.BevelInner  := TGraphicsBevelCut.bvNone;
  //Self.BevelOuter  := TGraphicsBevelCut.bvNone;

  FToolBarRows := TFPObjectList.Create(false);

  FCounter := 0;
end;

destructor TCustomSDVToolBarGroup.Destroy();
begin
  FToolBarRows.Free();
  inherited Destroy();
end;

function TCustomSDVToolBarGroup.InsertToolBarRow(): TSDVToolBarRow;
begin
  Result := nil;

  Result := TSDVToolBarRow.Create(Self);
  Result.Top   := 0;
  Result.Left  := 0;
  Result.Align := TAlign.alTop;

  Result.Name  := 'ToolBarRow' + IntToStr(FCounter);
  Inc(FCounter);

  Self.InsertControl(Result);
  Result.Caption := '';

  FToolBarRows.Add(Result);
end;

function TCustomSDVToolBarGroup.ToolBarRowCount(): Integer;
begin
  Result := FToolBarRows.Count;
end;

function TCustomSDVToolBarGroup.ToolBarRowAt
  (const AIndex: Integer): TSDVToolBarRow;
var ACount: Integer;
begin
  Result := nil;
  ACount := FToolBarRows.Count;
  if ((AIndex >= 0) and (AIndex <= ACount)) then
  begin
    Result := TSDVToolBarRow(FToolBarRows[AIndex]);
  end;
end;

end.

