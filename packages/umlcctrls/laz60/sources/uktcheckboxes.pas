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

unit uktcheckboxes;

interface
uses
(*.IFDEF MSWINDOWS*)
  //Windows,
  //Messages,
  Graphics,
  StdCtrls,  
(*.ENDIF*)
  SysUtils, Classes,
  dummy;

type

(* TCustomSDVCheckBox *)

  TCustomSDVCheckBox = class(TCustomCheckBox)
  private
    (* private declarations *)
  protected
    (* protected declarations *)

    FReadOnly: Boolean;

    function getReadOnly: Boolean; virtual;
    function getState: TCheckBoxState; virtual;

    procedure setReadOnly(const Value: Boolean); virtual;
    procedure setState(const Value: TCheckBoxState); virtual;

    procedure setChecked(Value: Boolean); override;

    procedure Click; override;
  public
    (* public declarations *)

    constructor Create(AOwner: TComponent); override;

    property ReadOnly: Boolean
      read getReadOnly write setReadOnly default FALSE;
    property State: TCheckBoxState
      read getState write setState default cbUnchecked;
  end;

(* TSDVCheckBox *)

  TSDVCheckBox = class(TCustomSDVCheckBox)
  published
    (* TCustomCheckBox: *)

(*.IFDEF MSWINDOWS*)
    //property Alignment;
    property BiDiMode;
    //property Ctl3D;
    property DragCursor;
    property DragKind;
    property ParentBiDiMode;
    //property ParentCtl3D;
    property OnEndDock;
    property OnStartDock;
(*.ENDIF*)

    property Action;
    property AllowGrayed;
    property Anchors;
    property Caption;
    property Checked;
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
    property State;
    property TabOrder;
    property TabStop;
    property Visible;

    property OnClick;
    property OnContextPopup;
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

    (* TCustomSDVCheckBox: *)

    property ReadOnly;
  end;

implementation

(* TCustomSDVCheckBox *)

function TCustomSDVCheckBox.getReadOnly: Boolean;
begin
  Result := FReadOnly;
end;

function TCustomSDVCheckBox.getState: TCheckBoxState;
begin
  Result := inherited State;
end;

procedure TCustomSDVCheckBox.setReadOnly(const Value: Boolean);
begin
  FReadOnly := Value;
  ClicksDisabled := Value;
end;

procedure TCustomSDVCheckBox.setState(const Value: TCheckBoxState);
begin
  inherited;
end;

procedure TCustomSDVCheckBox.setChecked(Value: Boolean);
begin
  inherited;
end;

procedure TCustomSDVCheckBox.Click;
begin
  if (not FReadOnly)
    then inherited;
end;

constructor TCustomSDVCheckBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FReadOnly := FALSE
end;

end.
