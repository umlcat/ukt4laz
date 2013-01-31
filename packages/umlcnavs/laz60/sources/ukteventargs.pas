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

unit ukteventargs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uktnormobjects,
  dummy;

type

(* TSDVEventArguments *)

  TSDVEventArguments = class(TInterfacedObject, ISDVHalfNormalizedObject)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)

    FSender: TObject;
  public
    (* Public declarations *)

    function AsText(): string; // virtual;

    procedure DoNothing(); // nonvirtual;

  public
    (* Public declarations *)

    property Sender: TObject
      read FSender write FSender;
  end;

implementation

(* TSDVEventArguments *)

function TSDVEventArguments.AsText(): string;
begin
  Result := '';
end;

procedure TSDVEventArguments.DoNothing();
begin
  //
end;

end.

