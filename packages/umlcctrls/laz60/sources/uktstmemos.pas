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

unit uktstmemos;

interface
uses
  SysUtils, Classes,
(*.IFDEF MSWINDOWS*)
  Graphics,
  Controls,
  //StdCtrls,
  Forms,
(*.ENDIF*)  
  //Types,
  uktmemos,
  dummy;

type

(* TCustomSDVStaticMemo *)

  TCustomSDVStaticMemo = class(TCustomSDVMemo)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)
  public
    (* Public declarations *)

    constructor Create(AOwner: TComponent); override;
  end;

(* TSDVStaticMemo *)

  TSDVStaticMemo = class(TCustomSDVStaticMemo)
  published
    (* Published declarations *)

    (* TCustomMemo: *)

(*.IFDEF MSWINDOWS*)
    property BiDiMode;
    //property Ctl3D;
    property DragCursor;
    property DragKind;
    //property ImeMode;
    //property ImeName;
    property ParentBiDiMode;
    //property ParentCtl3D;

    property OnEndDock;
    property OnStartDock;
(*.ENDIF*)

    property Align;
    property Alignment;
    property Anchors;
    property Color;
    property Constraints;
    property DragMode;
    property Enabled;
    property Font;
    property Lines;
    property MaxLength;
    //property OEMConvert;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ScrollBars;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property WantReturns;
    property WantTabs;
    property WordWrap;

    property OnChange;
    property OnClick;
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

    (* TCustomSDVStaticMemo: *)

//    property BorderStyle;
//    property HideSelection;
//    property ReadOnly;
  end;

implementation

(* TCustomSDVStaticMemo *)

constructor TCustomSDVStaticMemo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BorderStyle := bsNone;
  Color := clBtnFace;
  ReadOnly := true;
  HideSelection := true;
end;

end.
