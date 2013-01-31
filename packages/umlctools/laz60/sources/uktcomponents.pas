(**
 **************************************************************************
 *                                                                        *
 *  This file is part of the Star Developer's Component Library.          *
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

unit uktcomponents;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  uktnormobjects,
  uktdocsres,
  dummy;

(**
 ** Description:
 ** This unit provides some control basic types,
 ** with commonly used methods.
 **
 ** A "normalized" object / class is an object that supports,
 ** partially or fully, the "Normalized Object Software Design Pattern".
 **
 ** This unit defines classes, that support:
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
 ** * May have additional methods, fields, etc.
 **)

type

 (* TSDVHalfNormalizedPersistent *)

   TSDVHalfNormalizedPersistent = class(TInterfacedPersistent, ISDVHalfNormalizedObject)
   private
     (* private declarations *)
   protected
     (* protected declarations *)
   public
     (* public declarations *)

     // let's provide a default virtual without parameters destructor
     destructor Destroy(); override;

     // similar to Java's "ToString()"
     function AsText(): string; virtual;

     procedure DoNothing(); // nonvirtual;
   end;

 (* TSDVHalfNormalizedComponent *)

   TSDVHalfNormalizedComponent = class(TComponent, ISDVHalfNormalizedObject)
   private
     (* private declarations *)
   protected
     (* protected declarations *)
   public
     (* public declarations *)

     // let's provide a default virtual without parameters destructor
     destructor Destroy(); override;

     // similar to Java's "ToString()"
     function AsText(): string; virtual;

     procedure DoNothing(); // nonvirtual;
   end;

 (* TSDVNormalizedPersistent *)

   TSDVNormalizedPersistent = class(TSDVHalfNormalizedPersistent, ISDVNormalizedObject)
   private
     (* private declarations *)
   protected
     (* protected declarations *)
   public
     (* public declarations *)

     // let's provide a default virtual without parameters constructor
     procedure DoCreate(); virtual;

     // let's provide a default virtual without parameters destructor
     procedure DoDestroy(); virtual;
   end;

 (* TSDVHalfNormalizedComponent *)

   (* TSDVNormalizedComponent *)

   TSDVNormalizedComponent = class(TSDVHalfNormalizedComponent, ISDVNormalizedObject)
   private
     (* private declarations *)
   protected
     (* protected declarations *)
   public
     (* public declarations *)

     // let's provide a default virtual without parameters constructor
     procedure DoCreate(); virtual;

     // let's provide a default virtual without parameters destructor
     procedure DoDestroy(); virtual;
   end;

implementation

(* TSDVNormalizedComponent *)

procedure TSDVNormalizedComponent.DoCreate();
begin
  Self.DoNothing();
end;

procedure TSDVNormalizedComponent.DoDestroy();
begin
  Self.DoNothing();
end;

(* TSDVNormalizedPersistent *)

procedure TSDVNormalizedPersistent.DoCreate();
begin
  Self.DoNothing();
end;

procedure TSDVNormalizedPersistent.DoDestroy();
begin
  Self.DoNothing();
end;

(* TSDVHalfNormalizedPersistent *)

destructor TSDVHalfNormalizedPersistent.Destroy();
begin
  inherited Destroy();
end;

function TSDVHalfNormalizedPersistent.AsText(): string;
begin
  Result := ClassName();
end;

procedure TSDVHalfNormalizedPersistent.DoNothing();
begin
  // Does nothing on purpose !!!
end;

(* TSDVHalfNormalizedComponent *)

destructor TSDVHalfNormalizedComponent.Destroy();
begin
  inherited Destroy();
end;

function TSDVHalfNormalizedComponent.AsText(): string;
begin
  Result := ClassName();
end;

procedure TSDVHalfNormalizedComponent.DoNothing();
begin
  // Does nothing on purpose !!!
end;

end.

