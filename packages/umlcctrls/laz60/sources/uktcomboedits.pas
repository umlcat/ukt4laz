unit uktcomboedits;

interface
uses
{.IFDEF MSWINDOWS}
  Windows,
  Messages,
  Graphics,
  Controls, Forms,
  ExtCtrls, Buttons,
{.ENDIF}
  SysUtils, Classes, Types,
  ActnList,
  uktpanels, uktedits, uktcomboctrls,
  dummy;

const
  EditWidth  = 99;
  EditHeight = 21;

  ButtonWidth  = 22;
  ButtonHeight = 21;

type

(* TCustomSDVComboEditControl *)

  TCustomSDVComboEditControl = class(TCustomSDVComboControl)
  private
    { Private declarations }
  protected
    { Protected declarations }

    FEditor: TCustomSDVEdit;
  protected
    { Protected declarations }

    procedure AssignComboControlTop();
    procedure AssignComboControlBottom();
    procedure AssignComboControlLeft();
    procedure AssignComboControlRight();

    procedure AssignButtons();
  public
    { Friend Protected declarations }

    procedure CreateControls(); override;
    procedure DestroyControls(); override;
  public
    { Public declarations }

    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
  end;

{ TCustomSDVComboEdit }

  TCustomSDVComboEdit = class(TCustomSDVComboEditControl)
  private
    { Private declarations }
  protected
    { Protected declarations }

    FGlyph: TBitmap;

    FOnClick: TNotifyEvent;
  protected
    { Protected declarations }

    function getGlyph(): TBitmap;

    procedure setGlyph(AValue: TBitmap);
  protected
    { Protected declarations }

    procedure DelegateOnClick();

    procedure RefreshGlyph(); virtual;
    procedure DoComboButtonOnClick(); override;
    procedure Loaded(); override;
  public
    { Public declarations }

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    { Public declarations }

    property Glyph: TBitmap
       read getGlyph write setGlyph;

    property OnClick: TNotifyEvent
       read FOnClick write FOnClick;
  end;

{ TSDVComboEdit }

  TSDVComboEdit = class(TCustomSDVComboEdit)
  published
    { Published declarations }

    { TControl: }

{.IFDEF MSWINDOWS}
    property Alignment;
    property BiDiMode;
    {$IFDEF DELPHI}
    property Ctl3D;
    {$ENDIF}
    property DragCursor;
    property DragKind;
    property ParentBiDiMode;
    {$IFDEF DELPHI}
    property ParentCtl3D;
    {$ENDIF}
    property OnEndDock;
    property OnStartDock;
{.ENDIF}

    property Anchors;
//    property AutoSelect;
    property AutoSize;

    {$IFDEF DELPHI}
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    {$ENDIF}

//    property BiDiMode;
    property BorderStyle;
//    property CharCase;
    property Color;
    property Constraints;
//    property Ctl3D;
    property DragMode;
    property Enabled;
    property Font;
//    property HideSelection;
    {$IFDEF DELPHI}
    property ImeMode;
    property ImeName;
    {$ENDIF}
//    property MaxLength;
//    property OEMConvert;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
//    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;

    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;

    (* TCustomSDVComboEditControl: *)

    property Glyph;
  end;

implementation

(* TCustomSDVComboEditControl *)

procedure TCustomSDVComboEditControl.AssignComboControlTop();
begin
  //
end;

procedure TCustomSDVComboEditControl.AssignComboControlBottom();
begin
  //
end;

procedure TCustomSDVComboEditControl.AssignComboControlLeft();
begin
  Width  := EditWidth + ButtonWidth + TestArea;
  Height := EditHeight + TestArea;

  FComboButton.SetBounds
    (0, 0, ButtonWidth, ButtonHeight);
  FEditor.SetBounds
    (ButtonWidth, 0, EditWidth, EditHeight);

  FEditor.Align := alLeft;
  FComboButton.Align := alClient;
end;

procedure TCustomSDVComboEditControl.AssignComboControlRight();
begin
  Width  := EditWidth + ButtonWidth + TestArea;
  Height := EditHeight + TestArea;

  FEditor.SetBounds
    (0, 0, EditWidth, EditHeight);
  FComboButton.SetBounds
    (EditWidth, 0, ButtonWidth, ButtonHeight);

  FComboButton.Align := alRight;
  FEditor.Align := alClient;
end;

procedure TCustomSDVComboEditControl.AssignButtons();
begin
  case (FPosition) of
    dpBottom: AssignComboControlBottom();
    dpLeft:   AssignComboControlLeft();
    dpRight:  AssignComboControlRight();
//    dpTop:
    else      AssignComboControlTop();
  end;
end;

procedure TCustomSDVComboEditControl.CreateControls();
begin
  FEditor := TCustomSDVEdit.Create(Self);
  FComboButton := TSDVComboControlSpeedButton.Create(Self);
  FComboButton.Flat := false;

//  FEditor.ComboControl := {$IFNDEF DELPHI}@{$ENDIF}Self;
  FComboButton.ComboControl := Self;

  FComboButton.OnClick := {$IFNDEF DELPHI}@{$ENDIF}ComboButtonDelegateOnClick;

  AssignOrientation();
  AssignButtons();

  Self.InsertControl(FEditor);
//  FEditor.InsertControl(FComboButton);

  Self.InsertControl(FComboButton);
end;

procedure TCustomSDVComboEditControl.DestroyControls();
begin
  FComboButton.Free; FComboButton := nil;
  FEditor.Free; FEditor := nil;
end;

constructor TCustomSDVComboEditControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
//  BevelOuter := bvLowered;
//  BevelInner := bvNone;

  CreateControls();
  { Goal: To prepare the control .}
end;

destructor TCustomSDVComboEditControl.Destroy();
begin
  DestroyControls();
  inherited Destroy();
  { Goal: To unprepare the control .}
end;

{ TCustomSDVComboEdit }

function TCustomSDVComboEdit.getGlyph(): TBitmap;
begin
  Result := FGlyph;
  { Goal: "Glyph" property get method .}
  { Objetivo: Metodo lectura para propiedad "Glyph" .}
end;

procedure TCustomSDVComboEdit.setGlyph(AValue: TBitmap);
begin
  FGlyph.Assign(AValue);
  RefreshGlyph();
  { Goal: "Glyph" property set method .}
  { Objetivo: Metodo escritura para propiedad "Glyph" .}
end;

procedure TCustomSDVComboEdit.DelegateOnClick();
begin
  if (FOnClick <> nil) then
  begin
    FOnClick(Self);
  end;
end;

procedure TCustomSDVComboEdit.RefreshGlyph();
begin
  if (FComboButton <> nil) then
  begin
    FComboButton.Glyph.Assign(FGlyph);
  end;
end;

procedure TCustomSDVComboEdit.DoComboButtonOnClick;
begin
  DelegateOnClick();
end;

procedure TCustomSDVComboEdit.Loaded();
begin
  inherited Loaded();
  RefreshGlyph();
end;

constructor TCustomSDVComboEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FGlyph := TBitmap.Create();
  { Goal: To prepare the control .}
end;

destructor TCustomSDVComboEdit.Destroy;
begin
  FGlyph.Free();
  inherited Destroy();
  { Goal: To unprepare the control .}
end;

end.
