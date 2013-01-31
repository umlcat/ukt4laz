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

unit uktstdevents;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  dummy;

(* standard events *)

//type
  //TNotifyEvent = Classes.TNotifyEvent;

(**
 ** Used, sometimes, as "BeforeDoSomething" event-handler.
 **)

type
  TConfirmEvent = (* ^ *) function (Sender: TObject): Boolean of object;
type
  TOnBooleanChangeEvent =
    (* ^ *)procedure (Sender: TObject; Value: Boolean) of object;
type
  TOnStringChangeEvent =
    (* ^ *)procedure (Sender: TObject; const AValue: string) of object;

implementation

end.

