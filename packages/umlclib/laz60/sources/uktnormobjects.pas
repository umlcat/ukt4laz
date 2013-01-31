(**
 **************************************************************************
 *                                                                        *
 *  This file is part of the uktat Developer's Component Library.        *
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

unit uktnormobjects;

{$mode objfpc}{$H+}

interface
uses
  Classes,
  SysUtils,
  dummy;

(**
 ** Description:
 ** This unit defines an interface for objects,
 ** that supports:
 **
 ** A "normalized" object / class is an object that supports,
 ** partially or fully, the "Normalized Object Software Design Pattern".
 **
 ** . A default virtual constructor without parameters,
 ** . A default virtual destructor without parameters,
 ** . A default virtual function that returns
 **   an string like Java "ToString()" function
 ** . A non virtual method that explicitly does nothing
 **   like z80 CPU instruction "NOP"
 **
 ** The term "Normalized" is "borrowed" from Database Design.
 **
 ** It also provides a base class that supports that interface.
 **)

type

(* ISDVHalfNormalizedObject *)

  ISDVHalfNormalizedObject = interface(IUnknown)
    { interface declarations }

    //procedure Create(); // virtual;
    //procedure Destroy(); // virtual;

    // --> constructor cannot be assigned to an interface

    // let's provide a default virtual without parameters constructor
    //destructor Create(); virtual;

    // --> constructor cannot be assigned to an interface

    // let's provide a default virtual without parameters destructor
    //destructor Destroy(); override;

    function AsText(): string; // virtual;

    procedure DoNothing(); // nonvirtual;
  end;

(* ISDVNormalizedObject *)

  ISDVNormalizedObject = interface(ISDVHalfNormalizedObject)
    { interface declarations }

    // let's provide a default virtual without parameters constructor
    procedure DoCreate(); // virtual;

    // let's provide a default virtual without parameters destructor
    procedure DoDestroy(); // virtual;

    //function AsText(): string; // virtual;

    //procedure DoNothing(); // nonvirtual;
  end;

(* TSDVHalfNormalizedObject *)

  TSDVHalfNormalizedObject = class(TInterfacedObject, ISDVHalfNormalizedObject)
  private
    (* private declarations *)
  protected
    (* protected declarations *)
  public
    (* public declarations *)

    // TObject.Create() is non-virtual
    // let's provide a default virtual without parameters constructor
    constructor Create(); virtual;
    // let's provide a default virtual without parameters destructor
    destructor Destroy(); override;

    // similar to Java's "ToString()"
    function AsText(): string; virtual;

    procedure DoNothing(); // nonvirtual;
  end;

(* TSDVNormalizedObject *)

  TSDVNormalizedObject = class(TInterfacedObject, ISDVNormalizedObject)
  private
    (* private declarations *)
  protected
    (* protected declarations *)
  public
    (* public declarations *)

    // TObject.Create() is non-virtual
    // do not override "TObject.Create()", use "DoCreate", instead

    // do not override this, use "DoDestroy", instead
    destructor Destroy(); reintroduce;

    // let's provide a default virtual without parameters constructor
    procedure DoCreate(); virtual;
    // let's provide a default virtual without parameters destructor
    procedure DoDestroy(); virtual;

    // similar to Java's "ToString()"
    function AsText(): string; virtual;

    procedure DoNothing(); // nonvirtual;
  end;

implementation

{ TSDVNormalizedObject }

destructor TSDVNormalizedObject.Destroy;
begin
  DoNothing();
end;

procedure TSDVNormalizedObject.DoCreate();
begin
  DoNothing();
end;

procedure TSDVNormalizedObject.DoDestroy();
begin
  DoNothing();
end;

function TSDVNormalizedObject.AsText(): string;
begin
  Result := ClassName();
end;

procedure TSDVNormalizedObject.DoNothing;
begin
  // DoNothing !!!
end;

(* TSDVHalfNormalizedObject *)

constructor TSDVHalfNormalizedObject.Create();
begin
  inherited Create();
end;

destructor TSDVHalfNormalizedObject.Destroy();
begin
  inherited Destroy();
end;

function TSDVHalfNormalizedObject.AsText(): string;
begin
  Result := ClassName();
end;

procedure TSDVHalfNormalizedObject.DoNothing();
begin
  // Does nothing on purpose !!!
end;

end.

