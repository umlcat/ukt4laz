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

unit uktevargbtns;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  ukteventargs,
  uktspeedbtns,
  dummy;


(**
 ** This class generates an argument that can be passed,
 ** in event-handler methods.
 **)

type

(* TCustomSDVEventButton *)

  TCustomSDVEventButton = class(TCustomSDVSpeedButton)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)
  public
    (* Public declarations *)

    function readArgs(): TSDVEventArguments; virtual;
  end;

implementation

(* TCustomSDVEventButton *)

/// <summary>
/// Generate read-only arguments of the button,
/// for the events / delegators.
/// </summary>
/// <returns></returns>
function TCustomSDVEventButton.readArgs(): TSDVEventArguments;
begin
  // copiar los datos significativos del boton,
  // a una clase como datos de solo lectura
  Result := TSDVEventArguments.Create();
end;

end.

