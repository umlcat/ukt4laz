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

unit uktactivatedcontrols;

interface
uses
  SysUtils,
  Classes,
  uktnormobjects,
  uktcomponents,
  dummy;

  // Objetivo:
  // Esta unidad proporciona la interface "ISDVActivatedControl".
  // Esta interface permite controlar algunas operaciones,
  // en controles visuales y componentes no visuales,
  // cuando la forma ha sido activada visualmente, por primera vez,
  // y se tiene que hacer alguna operacion en especifico.

  // Ejemplo:
  // procedure TMyForm.FormActivate(Sender: TObject);
  // begin
  //   if (not ActivadaPreviamente()) then
  //   begin
  //     MyControl.ActivateFirst();
  //   end;
  // end;

  // Goal:
  // This unit provides the "ISDVActivatedControl" interface.
  // This interface allows to control some operations,
  // in visual controls, & non visual components,
  // when then form has been activated visually, for the first time,
  // and some specific operations must be perform.

  // Example:
  // procedure TMyForm.FormActivate(Sender: TObject);
  // begin
  //   if (not PreviouslyActivated()) then
  //   begin
  //     MyControl.ActivateFirst();
  //   end;

type

(* ISDVActivatedControl *)

  ISDVActivatedControl = interface
    (* Private declarations *)

    (* Protected declarations *)

    function getActivated(): Boolean;
    procedure setActivated(const Value: Boolean);

    (* Protected declarations *)

    // after the form has been activated, for the first time,
    // what to do
    procedure ActivateFirst(); // virtual;

    // before the form is going to be deactivated,
    // for the last time, and still active, what to do
    procedure DeactivateLast(); // virtual;

    (* Public declarations *)

    property Activated: Boolean
      read getActivated write setActivated;
  end;
  // Goal: Returns if a visual control has been activated once.
  // Objetivo: Regresa si un control visual ha sido activado alguna vez.

(* TSDVActivatedPersistent *)

  //TSDVActivatedPersistent = class(TSDVNormalizedPersistent, ISDVActivatedControl)

  //end;

  TSDVActivatedPersistent = class(TSDVNormalizedObject, ISDVActivatedControl)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)

    FActivated: Boolean;

    function getActivated(): Boolean;
    procedure setActivated(const Value: Boolean);
  protected
    (* Protected declarations *)

    procedure ActivateFirst(); virtual;
    procedure DeactivateLast(); virtual;
  public
    (* Public declarations *)

    property Activated: Boolean
      read getActivated write setActivated;
  end;

(* TSDVActivatedComponent *)

  TSDVActivatedComponent = class(TSDVHalfNormalizedComponent, ISDVActivatedControl)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)

    FActivated: Boolean;

    function getActivated(): Boolean;
    procedure setActivated(const Value: Boolean);
  protected
    (* Protected declarations *)

    procedure ActivateFirst(); virtual;
    procedure DeactivateLast(); virtual;
  public
    (* Public declarations *)

    property Activated: Boolean
      read getActivated write setActivated;
  end;

implementation

(* TSDVActivatedPersistent *)

function TSDVActivatedPersistent.getActivated(): Boolean;
begin
  Result := FActivated;
  // Goal: "Activated" property get method.
  // Objetivo: Metodo lectura para propiedad "Activated".
end;

procedure TSDVActivatedPersistent.setActivated(const Value: Boolean);
begin
  if (FActivated <> Value) then
  begin
    FActivated := Value;
    if (Value) then
    begin
      ActivateFirst()
    end else
    begin
      DeActivateLast();
    end;
  end;
  // Goal: "Activated" property set method.
  // Objetivo: Metodo escritura para propiedad "Activated".
end;

procedure TSDVActivatedPersistent.ActivateFirst();
begin
  // Goal: Performa an specific action when the control is activated
  // by the first time.

  // Objetivo: Realizar una accion especifica cuando el control ha sido
  // activado la primera vez.
end;

procedure TSDVActivatedPersistent.DeactivateLast();
begin
  // Goal: Performa an specific action when the control is dectivated
  // by the last time.

  // Objetivo: Realizar una accion especifica cuando el control ha sido
  // deactivado por ultima vez.
end;

(* TSDVActivatedComponent *)

function TSDVActivatedComponent.getActivated(): Boolean;
begin
  Result := FActivated;
  // Goal: "Activated" property get method.
  // Objetivo: Metodo lectura para propiedad "Activated".
end;

procedure TSDVActivatedComponent.setActivated(const Value: Boolean);
begin
  if (FActivated <> Value) then
  begin
    FActivated := Value;
    if (Value) then
    begin
      ActivateFirst()
    end else
    begin
      DeActivateLast();
    end;
  end;
  // Goal: "Activated" property set method.
  // Objetivo: Metodo escritura para propiedad "Activated".
end;

procedure TSDVActivatedComponent.ActivateFirst();
begin
  // Goal: Performa an specific action when the control is activated
  // by the first time.

  // Objetivo: Realizar una accion especifica cuando el control ha sido
  // activado la primera vez.
end;

procedure TSDVActivatedComponent.DeactivateLast();
begin
  // Goal: Performa an specific action when the control is dectivated
  // by the last time.

  // Objetivo: Realizar una accion especifica cuando el control ha sido
  // deactivado por ultima vez.
end;

end.
