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

unit uktpgctrls;

interface

uses
(*.IFDEF MSWINDOWS*)
  //Windows,
  //Messages,
  Graphics,
  Controls,
  ComCtrls,
  Forms,
(*.ENDIF*)
  SysUtils, Classes,
  uktactivatedcontrols,
  dummy;

type

(* TSDVTabStyle *)

  TSDVTabStyle = (sdtsCustomTabs, sdtsShowTabs, sdtsHideTabs);

(* TCustomSDVPageControl *)

  TCustomSDVPageControl = class(TPageControl, ISDVActivatedControl)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)

    FActivated: Boolean;
    FTabStyle:  TSDVTabStyle;

    function getActivated: Boolean; virtual;
    function getTabStyle: TSDVTabStyle; virtual;

    procedure setActivated(const Value: Boolean); virtual;
    procedure setTabStyle(const Value: TSDVTabStyle); virtual;

    procedure UpdateShowTabs(const Value: Boolean); virtual;

    procedure ActivateFirst; virtual;
    procedure DeactivateLast; virtual;
  public
    (* Public declarations *)

    constructor Create(AOwner: TComponent); override;

    property Activated: Boolean
      read getActivated write setActivated;

    (* UnPublished declarations *)

    property TabStyle: TSDVTabStyle
      read getTabStyle write setTabStyle;
  end;

(* TSDVPageControl *)

  TSDVPageControl = class(TCustomSDVPageControl)
  published
    (* Published declarations *)

    (* TCustomSDVPageControl: *)

    property TabStyle;
  end;

implementation

(* TCustomSDVPageControl *)

function TCustomSDVPageControl.getActivated: Boolean;
begin
  Result := FActivated;
  // Goal: "Activated" property get method.
  // Objetivo: Metodo lectura para propiedad "Activated".
end;

function TCustomSDVPageControl.getTabStyle: TSDVTabStyle;
begin
  Result := FTabStyle;
  // Goal: "TabStyle" property get method.
  // Objetivo: Metodo lectura para propiedad "TabStyle".
end;

procedure TCustomSDVPageControl.setActivated(const Value: Boolean);
begin
  if (FActivated <> Value) then
  begin
    FActivated := Value;
    if (Value)
      then ActivateFirst
      else DeActivateLast;
  end;
  // Goal: "Activated" property set method.
  // Objetivo: Metodo escritura para propiedad "Activated".
end;

procedure TCustomSDVPageControl.setTabStyle(const Value: TSDVTabStyle);
begin
  if (FTabStyle <> Value) then
  begin
    FTabStyle := Value;

    case FTabStyle of
      sdtsCustomTabs: (*Nothing*);
      sdtsShowTabs:   UpdateShowTabs(true);
      sdtsHideTabs:   UpdateShowTabs(false);
    end;
  end;
  // Goal: "TabStyle" property set method.
  // Objetivo: Metodo escritura para propiedad "TabStyle".
end;

procedure TCustomSDVPageControl.UpdateShowTabs(const Value: Boolean);
var AIndex, ACount: Integer;
begin
  ACount := Pred(PageCount);
  for AIndex := 0 to ACount do
  begin
    Pages[AIndex].TabVisible := Value;
  end;
  // Goal: Assign all tabs the same visible state.
  // Objetivo: Asignar a todas las etiquetas el mismo estado visible.
end;

procedure TCustomSDVPageControl.ActivateFirst;
begin
  // Goal: Performa an specific action when the control is activated
  // by the first time.

  // Objetivo: Realizar una accion especifica cuando el control ha sido
  // activado la primera vez.
end;

procedure TCustomSDVPageControl.DeactivateLast;
begin
  // Goal: Performa an specific action when the control is dectivated
  // by the last time.

  // Objetivo: Realizar una accion especifica cuando el control ha sido
  // deactivado por ultima vez.
end;

constructor TCustomSDVPageControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTabStyle  := sdtsCustomTabs;
  FActivated := false;
end;

end.
