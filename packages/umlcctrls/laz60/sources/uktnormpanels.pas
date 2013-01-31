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

unit uktnormpanels;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uktpanels,
  uktnormobjects,
  dummy;

(**
 ** Description:
 ** This unit declares controls,
 ** that support
 ** partially or fully, the "Normalized Object Software Design Pattern".
 **)

 type

(* TCustomSDVNormalizedPanel *)

  TCustomSDVNormalizedPanel =
    class(
      TCustomSDVPanel,
      ISDVHalfNormalizedObject)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)
  public
    (* Public declarations *)

    // similar to Java's "ToString()"
    function AsText(): string; virtual;

    procedure DoNothing(); // nonvirtual;
  end;

(* TSDVNormalizedPanel *)

  TSDVNormalizedPanel = class(TCustomSDVNormalizedPanel)
  published
    (* Published declarations *)

    (* TPanel: *)

    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BorderSpacing;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BidiMode;
    property BorderWidth;
    property BorderStyle;
    property Caption;
    property ChildSizing;
    property ClientHeight;
    property ClientWidth;
    property Color;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property FullRepaint;
    property ParentBidiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property UseDockManager;
    property Visible;

    property OnClick;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnGetDockCaption;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;

    //property ShowGrid;
    //property Text;

    //property DockManager;

    (* TCustomSDVPanel: *)

    property Activated;
    property ReadOnly;

    property OnChange;
  end;

implementation

(* TCustomSDVNormalizedPanel *)

function TCustomSDVNormalizedPanel.AsText(): string;
begin
  // @to-do: ...
  Result := '';
end;

procedure TCustomSDVNormalizedPanel.DoNothing();
begin
  // Nothing on purpouse !!!
end;

end.

