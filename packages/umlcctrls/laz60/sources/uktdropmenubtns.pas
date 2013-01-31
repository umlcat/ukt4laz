unit uktdropmenubtns;

interface
uses
(*.IFDEF MSWINDOWS*)
  Windows, Messages, Graphics,
  Controls, Forms,
  ExtCtrls, Buttons, Menus, Dialogs,
(*.ENDIF*)
  SysUtils, Classes, Types,
  ActnList,
  uktaccolls,
  uktctrls,
  uktcomboctrls,
  uktDropBtns,
  dummy;

type

(* TCustomSDVDropDownMenuButton *)

  TCustomSDVDropDownMenuButton = class(TCustomSDVActionButton)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)

    FDropMenu: TPopUpMenu;
    FActions:  TSDVActionCollection;
    FImages:   TImageList;
  protected
    (* Protected declarations *)

    function getImages: TImageList;

    procedure setImages(const Value: TImageList);
  protected
    (* Protected declarations *)

    procedure DoMainButtonOnClick; override;
    procedure DoComboButtonOnClick; override;

    procedure RefreshCaption; override;
    procedure RefreshGlyph; override;

    procedure AssignAction(Sender: TObject);

    procedure Notification
      (AComponent: TComponent; Operation: TOperation); override;
  protected
    (* Protected declarations *)

    property DropMenu: TPopUpMenu
      read FDropMenu write FDropMenu;
  public
    (* Public declarations *)

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    (* Public declarations *)

    property Actions: TSDVActionCollection
      read FActions write FActions;
    property Images: TImageList
      read getImages write setImages;

    property Action;
    property Orientation;
    property Direction;
    property Position;
    property PopupMenu;
  end;

(* TSDVDropDownMenuButton *)

  TSDVDropDownMenuButton = class(TCustomSDVDropDownMenuButton)
  published
    (* Published declarations *)

    (* TCustomPanel: *)

(*.IFDEF MSWINDOWS*)
    property AutoSize;
    property BiDiMode;
    {$IFDEF DELPHI}
    property Ctl3D;
    {$ENDIF}
    property UseDockManager;
    property DockSite;
    property DragCursor;
    property DragKind;
    property FullRepaint;
    //property Locked;
    property ParentBiDiMode;
    {$IFDEF DELPHI}
    property ParentCtl3D;
    {$ENDIF}

    //property OnCanResize;
    property OnDockDrop;
    property OnDockOver;
    property OnEndDock;
    property OnGetSiteInfo;
    property OnStartDock;
    property OnUnDock;
(*.ENDIF*)

    property Align;
    property Alignment;
    property Anchors;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BorderWidth;
    property BorderStyle;
    property Caption;
    property Color;
    property Constraints;
    property DragMode;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;

    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDrag;

    (* TCustomSDPanel: *)

    (* TCustomSDComboButtonControl: *)

    property ShowCaption;
    property ShowGlyph;

    (* TCustomSDActionButton: *)

    property Actions;
    property Images;

    property Action;
    property Orientation;
    property Direction;
    property Position;
  end;

implementation

(* TCustomSDVDropDownMenuButton *)

procedure TCustomSDVDropDownMenuButton.DoMainButtonOnClick;
begin
  // Nothing;
end;

function TCustomSDVDropDownMenuButton.getImages: TImageList;
begin
  Result := FImages;
  // Goal: "Images" property get method.
  // Objetivo: Metodo lectura para propiedad "Images".
end;

procedure TCustomSDVDropDownMenuButton.setImages(const Value: TImageList);
begin
  FImages := Value;
  DropMenu.Images := Value;
  // Goal: "Images" property set method.
  // Objetivo: Metodo escritura para propiedad "Images".
end;

procedure TCustomSDVDropDownMenuButton.DoComboButtonOnClick;
var I, C: Integer; P: TPoint; Item: TMenuItem;
begin
  P := DropDownLocation;

  DropMenu.Items.Clear;
  C := Pred(Actions.Count);
  for I := 0 to C do
  begin
    Item := TMenuItem.Create(Self);
    DropMenu.Items.Add(Item);
    Item.Action  := (Actions.Items[i] as TSDVActionItem).Action;
    Item.OnClick := {$IFNDEF DELPHI}@{$ENDIF}AssignAction;

    if (Item.Action = nil)
      then Item.Caption := '-';
  end;
  DropMenu.Popup(P.X, P.Y);
end;

procedure TCustomSDVDropDownMenuButton.RefreshCaption;
begin
  if (FMainButton <> nil) then
  begin
    if ((FShowCaption) and (FMainButton.Action <> nil))
      then FMainButton.Caption := (FMainButton.Action as TAction).Caption
      else FMainButton.Caption := '';
  end;  
end;

procedure TCustomSDVDropDownMenuButton.RefreshGlyph;
var BasicAction: TBasicAction; ThisAction: TAction;
begin
  if (FMainButton <> nil) then
  begin
    BasicAction := FMainButton.Action;
    if ((FShowGlyph) and (BasicAction <> nil) and (Images <> nil)) then
    begin
      if (BasicAction is TAction) then
      begin
        ThisAction := (BasicAction as TAction);
        FImages.GetBitmap(ThisAction.ImageIndex, FMainButton.Glyph)
      end;
    end else FMainButton.Glyph.Assign(nil);
  end;
end;

procedure TCustomSDVDropDownMenuButton.AssignAction(Sender: TObject);
begin
  if (Sender is TMenuItem) then
  begin
    FMainButton.Glyph := nil;
    // limpiar imagen
    // clear image

    FMainButton.Action  := (Sender as TMenuItem).Action;
    // copiar accion de elementos a boton principal
    // copy action from items to main button

    FMainButton.Caption := '';
    FMainButton.Glyph.Assign(nil);
    // limpiar texto y mostrar imagen
    // clear text and show image

    RefreshCaption;
    RefreshGlyph;
  end;
end;

procedure TCustomSDVDropDownMenuButton.Notification
  (AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) then
  begin
    if ((FImages <> nil) and (AComponent = Images))
      then FImages := nil
    else if (AComponent is TActionList)
      then Actions.Clear;
  end;
  // Goal: To detect & update when associated components,
  // have been removed.

  // Objetivo: Detectar y actualizar cuando,
  // los componentes asociados se han removido.
end;

constructor TCustomSDVDropDownMenuButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDropMenu := TPopUpMenu.Create(Self);
  FActions  := TSDVActionCollection.Create(Self, TSDVActionItem);
  // Goal: To prepare the control.
end;

destructor TCustomSDVDropDownMenuButton.Destroy;
begin
  FDropMenu.Free;
  FActions.Free;
  inherited Destroy;
  // Goal: To unprepare the control.
end;

end.
