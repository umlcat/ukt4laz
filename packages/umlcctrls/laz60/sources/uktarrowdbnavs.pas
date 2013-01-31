unit uktarrowdbnavs;

interface
uses
(*.IFDEF MSWINDOWS*)
  Windows, Messages,
  Graphics, Controls,
(*.ENDIF*)
  SysUtils, Classes, DB,
  uktDBLinks,
  uktarrownavoptions,
  uktarrownavstrs,
  uktarrownavs,
  dummy;

type

(* TSDVControlDataLink *)

  TSDVControlDataLink = class(TSDVFieldDataLink)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)

    FControl: TControl;
  public
    (* Public declarations *)

    constructor Create();

    property Control: TControl
      read FControl write FControl;
  end;

(* TSDVDBNavigatorDataLink *)

  TCustomSDVDBArrowNavigator = class;
  TSDVDBNavigatorDataLink = class(TSDVControlDataLink)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)

    procedure EditingChanged(); override;
    procedure DataSetChanged(); override;
    procedure ActiveChanged(); override;
  public
    (* Public declarations *)

    function Navigator(): TCustomSDVDBArrowNavigator;
  end;

 (* TCustomSDVDBArrowNavigator *)

  TCustomSDVDBArrowNavigator = class(TCustomSDVArrowNavigator)
  private
    (* Private declarations *)

    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
  protected
    (* Protected declarations *)

    FDataLink: TSDVDBNavigatorDataLink;

    function getDataSource(): TDataSource;

    procedure setDataSource(Value: TDataSource);

    procedure ClickFirst();     override;
    procedure ClickFastPrior(); override;
    procedure ClickPrior();     override;
    procedure ClickRoot();      override;
    procedure ClickParent();    override;
    procedure ClickNext();      override;
    procedure ClickFastNext();  override;
    procedure ClickLast();      override;
  public
    (* Public declarations *)

    procedure DataChanged();
    procedure EditingChanged();
    procedure ActiveChanged();
	
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;

    property DataSource: TDataSource
      read getDataSource write setDataSource;
  end;

(* TSDVDBArrowNavigator *)

  TSDVDBArrowNavigator = class(TCustomSDVDBArrowNavigator)
  published
    (* Published declarations *)

(*.IFDEF MSWINDOWS*)
    property DragCursor;
    property DragKind;
    property Ctl3D;
    property ParentCtl3D;
    property OnEndDock;
    property OnStartDock;
(*.ENDIF*)

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

    (* TCustomSDVArrowNavigator: *)

    //property ButtonsVisible;
    //property ButtonsEnabled;

    property Captions;
    property Delta;
    property Hints;

    property ShowCaptions;
    property ShowHint;

    property OnMouseMove;
    property OnRefreshScroll;

   (* TCustomSDVDBArrowNavigator: *)
   
    property DataSource;
  end;

implementation

(* TSDVControlDataLink *)

constructor TSDVControlDataLink.Create();
begin
  inherited Create();
  VisualControl := true;
  FControl := nil;
  // Objetivo: Preparar el vinculodatos.
  // Goal: To prepare the datalink.
end;

(* TSDVDBNavigatorDataLink *)

procedure TSDVDBNavigatorDataLink.EditingChanged();
begin
  if (Navigator() <> nil)
    then Navigator().EditingChanged();
end;

procedure TSDVDBNavigatorDataLink.DataSetChanged();
begin
  if (Navigator() <> nil)
    then Navigator().DataChanged();
end;

procedure TSDVDBNavigatorDataLink.ActiveChanged();
begin
  if (Navigator() <> nil)
    then Navigator().ActiveChanged();
end;

function TSDVDBNavigatorDataLink.Navigator(): TCustomSDVDBArrowNavigator;
begin
  Result := nil;
  if (Control <> nil) then
  begin
    if (Control is TCustomSDVDBArrowNavigator)
      then Result := (Control as TCustomSDVDBArrowNavigator);
  end;	
end;

(* TCustomSDVDBArrowNavigator *)

procedure TCustomSDVDBArrowNavigator.CMEnabledChanged(var Message: TMessage);
begin
  inherited;

  if (not (csLoading in ComponentState))
    then ActiveChanged();
end;

function TCustomSDVDBArrowNavigator.getDataSource(): TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TCustomSDVDBArrowNavigator.setDataSource(Value: TDataSource);
begin
  FDataLink.DataSource := Value;
  if (not (csLoading in ComponentState))
    then ActiveChanged();
  if (Value <> nil)
    then Value.FreeNotification(Self);
end;

procedure TCustomSDVDBArrowNavigator.ClickFirst();
begin
  if ((DataSource <> nil) and (DataSource.DataSet <> nil))
    then DataSource.DataSet.First();
  // Goal: Do "First" button action.
  // Objetivo: Realizar accion de boton "Primero".
end;

procedure TCustomSDVDBArrowNavigator.ClickPrior();
begin
  if ((DataSource <> nil) and (DataSource.DataSet <> nil))
    then DataSource.DataSet.Prior();
  // Goal: Do "Prior" button action.
  // Objetivo: Realizar accion de boton "Anterior".
end;

procedure TCustomSDVDBArrowNavigator.ClickFastPrior();
var I: Integer;
begin
  I := Delta;
  if ((DataSource <> nil) and (DataSource.DataSet <> nil)) then
  repeat
    DataSource.DataSet.Prior();
    Dec(I);
  until (I = 0);
  // Goal: Do "FastPrior" button action.
  // Objetivo: Realizar accion de boton "AnteriorRapido".
end;

procedure TCustomSDVDBArrowNavigator.ClickRoot();
begin
//  DataSource.DataSet.Prior();
  // Goal: Do "Parent" button action.
  // Objetivo: Realizar accion de boton "Padre".
end;

procedure TCustomSDVDBArrowNavigator.ClickParent();
begin
//  DataSource.DataSet.Prior();
  // Goal: Do "Parent" button action.
  // Objetivo: Realizar accion de boton "Padre".
end;

procedure TCustomSDVDBArrowNavigator.ClickNext();
begin
  if ((DataSource <> nil) and (DataSource.DataSet <> nil))
    then DataSource.DataSet.Next();
  // Goal: Do "Next" button action.
  // Objetivo: Realizar accion de boton "Siguiente".
end;

procedure TCustomSDVDBArrowNavigator.ClickFastNext();
var I: Integer;
begin
  I := Delta;
  if ((DataSource <> nil) and (DataSource.DataSet <> nil)) then  
  repeat
    DataSource.DataSet.Next();
    Dec(I);
  until (I = 0);
  // Goal: Do "FastNext" button action.
  // Objetivo: Realizar accion de boton "SiguienteRapido".
end;

procedure TCustomSDVDBArrowNavigator.ClickLast();
begin
  if ((DataSource <> nil) and (DataSource.DataSet <> nil))
    then DataSource.DataSet.Last();
  // Goal: Do "Last" button action.
  // Objetivo: Realizar accion de boton "Ultimo".
end;

procedure TCustomSDVDBArrowNavigator.DataChanged();
var CanBackward, CanForward: Boolean;
begin
  CanBackward := Enabled and FDataLink.Active and (not FDataLink.DataSet.BoF);
  CanForward := Enabled and FDataLink.Active and (not FDataLink.DataSet.EoF);

  Buttons[btnopFirst].Enabled  := CanBackward;
  Buttons[btnopFastPrior].Enabled  := CanBackward;
  Buttons[btnopPrior].Enabled  := CanBackward;
  Buttons[btnopParent].Enabled := false;
  Buttons[btnopNext].Enabled   := CanForward;
  Buttons[btnopFastNext].Enabled   := CanForward;
  Buttons[btnopLast].Enabled   := CanForward;
end;

procedure TCustomSDVDBArrowNavigator.EditingChanged();
//var CanModify: Boolean;
begin
//  CanModify := Enabled and FDataLink.Active and FDataLink.DataSet.CanModify;
//  Buttons[btnopInsert].Enabled := CanModify;
end;

procedure TCustomSDVDBArrowNavigator.ActiveChanged();
begin
  if (not (Enabled and FDataLink.Active)) then
  begin
    ButtonsEnabled := [];
  end else
  begin
    DataChanged();
    EditingChanged();
  end;
end;

constructor TCustomSDVDBArrowNavigator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FDataLink := TSDVDBNavigatorDataLink.Create();
  FDataLink.Control := Self;
//  FReadOnly := true;
  // Goal: To prepare the navigator.
end;

destructor TCustomSDVDBArrowNavigator.Destroy();
begin
  FDataLink.Free;

  inherited Destroy();
  // Goal: To unprepare the navigator.
end;

end.
