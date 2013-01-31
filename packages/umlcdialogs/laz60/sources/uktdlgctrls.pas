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

unit uktdlgctrls;

interface
uses
{$IFDEF MSWINDOWS}
  Windows, Messages,
{$ENDIF}
  SysUtils, Classes,
{$IFDEF MSWINDOWS}
  Controls, Forms,
{$ENDIF}
  uktcomponents,
  dummy;

(**
 ** Description:
 ** TSDVDialogComponent. It's the superclass,
 ** for several dialog container components.
 **)

type

(* TSDVDialogComponent *)

  TSDVDialogComponent = class(TSDVNormalizedComponent)
  private
    (* Private declarations *)

    FBeforeExecute,
    FAfterExecute: TNotifyEvent;
  protected
    (* Protected declarations *)

    FData:   pointer;
    FHelpContext: Integer;
    FForm: TCustomForm;
    FTitle: string;
  protected
    (* Protected declarations *)

    function getData(): pointer; virtual;
    function getHelpContext(): Integer; virtual;
    function getDialog(): TCustomForm; virtual;
    function getTitle(): string; virtual;

    procedure setData(Value: pointer); virtual;
    procedure setHelpContext(const Value: Integer); virtual;
    procedure setDialog(const Value: TCustomForm); virtual;
    procedure setTitle(const Value: string); virtual;
  protected
    (* Protected declarations *)

    procedure DelegateBeforeExecute();
    procedure DelegateAfterExecute();

    procedure DoBeforeExecute(); virtual;
    procedure DoAfterExecute(); virtual;

    function CreateDialog(): TCustomForm; virtual;
    procedure DestroyDialog(); virtual;
  public
    (* Public declarations *)

    function Execute(): Boolean;
  public
    (* Public declarations *)

    (* Never Published declarations *)

    property Data: pointer
        read getData write setData;
    property Dialog: TCustomForm
      read getDialog write setDialog;
  public
    (* Public declarations *)

    (* Unpublished declarations *)

    property HelpContext: Integer
      read getHelpContext write setHelpContext;
    property Title: string
        read getTitle write setTitle;

    property BeforeExecute: TNotifyEvent
       read FBeforeExecute write FBeforeExecute;
    property AfterExecute: TNotifyEvent
       read FAfterExecute write FAfterExecute;
  end;

implementation

(* TSDVDialogComponent *)

function TSDVDialogComponent.getData(): pointer;
begin
  Result := FData;
  // Goal: "Data" property get method .
  // Objetivo: Metodo lectura propiedad "Data" .
end;

function TSDVDialogComponent.getHelpContext(): Integer;
begin
  Result := FHelpContext;
  // Goal: "HelpContext" property get method .
  // Objetivo: Metodo lectura propiedad "HelpContext" .
end;

function TSDVDialogComponent.getDialog(): TCustomForm;
begin
  Result := FForm;
  // Goal: "Dialog" property get method .
  // Objetivo: Metodo lectura propiedad "Dialog" .
end;

function TSDVDialogComponent.getTitle(): string;
begin
  Result := FTitle;
  // Goal: "Title" property get method .
  // Objetivo: Metodo lectura propiedad "Title" .
end;

procedure TSDVDialogComponent.setData(Value: pointer);
begin
  FData := Value;
  // Goal: "Data" property get method .
  // Objetivo: Metodo lectura propiedad "Data" .
end;

procedure TSDVDialogComponent.setHelpContext(const Value: Integer);
begin
  FHelpContext := Value;
  // Goal: "HelpContext" property get method .
  // Objetivo: Metodo lectura propiedad "HelpContext" .
end;

procedure TSDVDialogComponent.setDialog(const Value: TCustomForm);
begin
  FForm := Value;
  // Goal: "Dialog" property get method .
  // Objetivo: Metodo lectura propiedad "Dialog" .
end;

procedure TSDVDialogComponent.setTitle(const Value: string);
begin
  FTitle := Value;
  // Goal: "Title" property get method .
  // Objetivo: Metodo lectura propiedad "Title" .
end;

procedure TSDVDialogComponent.DelegateBeforeExecute();
begin
  if (FBeforeExecute <> nil)
    then FBeforeExecute(Self);
  // Objetivo: Ejecutar el evento "BeforeExecute" .
  // Goal: To execute the "BeforeExecute" event .
end;

procedure TSDVDialogComponent.DelegateAfterExecute();
begin
  if (FAfterExecute <> nil)
    then FAfterExecute(Self);
  // Objetivo: Ejecutar el evento "AfterExecute" .
  // Goal: To execute the "AfterExecute" event .
end;

procedure TSDVDialogComponent.DoBeforeExecute();
begin
  DelegateBeforeExecute();
  Dialog := CreateDialog();
end;

procedure TSDVDialogComponent.DoAfterExecute();
begin
  DestroyDialog();
  DelegateAfterExecute();
end;

function TSDVDialogComponent.CreateDialog(): TCustomForm;
begin
  FForm := nil;
  Result := FForm;
  // Goal: To create the dialog box.
  // Objetivo Construir la cajadialogo .
end;

procedure TSDVDialogComponent.DestroyDialog();
begin
  FForm.Release; FForm := nil;
  // Goal: To destroy the dialog box.
  // Objetivo Destruir la cajadialogo .
end;

function TSDVDialogComponent.Execute(): Boolean;
begin
  DoBeforeExecute();
  Result := (Dialog.ShowModal <> mrCancel);
  DoAfterExecute();
  // Goal: Show the dialog box .
  // Objetivo: Mostrar la caja de dialogo .
end;

end.
