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

unit ukttreeiterators;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  contnrs,
  uktiterators,
  ukttreenodes,
  dummy;

(**
 ** This unit implements concrete classes,
 ** in order to support the "Iterator" Design Pattern,
 ** that are specific to the tree structure.
 **
 ** This iterator is intended to be used with a "while" loop,
 ** instead of a "for" loop, since the "for" loop used,
 ** in the GoF book is not the same in Pascal.
 **
 ** ...
 **)

 type

   (* TSDVAbstractTreeIterator *)

   TSDVAbstractTreeIterator = class(TSDVAbstractIterator)
   private
     (* Private declarations *)
   protected
     (* Protected declarations *)

     FCollection: TSDVTreeCollection;
   public
     (* Public declarations *)

     function Collection(): TSDVTreeCollection;

     constructor Create(); override;
     destructor Destroy(); override;
   end;

   (* TSDVAbstractTreeInmediateIterator *)

   TSDVAbstractTreeInmediateIterator = class(TSDVAbstractIterator)
   private
     (* Private declarations *)
   protected
     (* Protected declarations *)
   public
     (* Public declarations *)
   end;

   (* TSDVTreeInmediateForEachIterator *)

   TSDVTreeInmediateForEachIterator = class(TSDVAbstractIterator)
   private
     (* Private declarations *)
   protected
     (* Protected declarations *)
   public
     (* Public declarations *)

     procedure Start(); override;
     procedure Stop(); override;

     function Next(): Boolean; override;
     function Current(): TObject; override;
   end;

   (* TSDVTreeInmediateForBackIterator *)

   TSDVTreeInmediateForBackIterator = class(TSDVAbstractIterator)
   private
     (* Private declarations *)
   protected
     (* Protected declarations *)
   public
     (* Public declarations *)

     procedure Start(); override;
     procedure Stop(); override;

     function Next(): Boolean; override;
     function Current(): TObject; override;
   end;

   (* TSDVTreeUpIterator *)

   TSDVTreeUpIterator = class(TSDVAbstractTreeIterator)
   private
     (* Private declarations *)
   protected
     (* Protected declarations *)
   public
     (* Public declarations *)

     procedure Start(); override;
     procedure Stop(); override;

     function Next(): Boolean; override;
     function Current(): TObject; override;
   end;

   (* TSDVAbstractTreeIteratorRecursive *)

   TSDVAbstractTreeIteratorRecursive = class(TSDVAbstractTreeIterator)
   private
     (* Private declarations *)
   protected
     (* Protected declarations *)

     FStack: TObjectStack;
   public
     (* Public declarations *)

     procedure Start(); override;
     procedure Stop(); override;

     function Next(): Boolean; override;
     function Current(): TObject; override;
   end;

   (* TSDVTreePrefixIterator *)

   TSDVTreePrefixIterator = class(TSDVAbstractTreeIteratorRecursive)
   private
     (* Private declarations *)
   protected
     (* Protected declarations *)
   public
     (* Public declarations *)

     procedure Start(); override;
     procedure Stop(); override;

     function Next(): Boolean; override;
     function Current(): TObject; override;
   end;

   (* TSDVTreePosfixIterator *)

   TSDVTreePosfixIterator = class(TSDVAbstractTreeIteratorRecursive)
   private
     (* Private declarations *)
   protected
     (* Protected declarations *)
   public
     (* Public declarations *)

     procedure Start(); override;
     procedure Stop(); override;

     function Next(): Boolean; override;
     function Current(): TObject; override;
   end;

   (* TSDVTreePrefixPosfixIterator *)

   TSDVTreePrefixPosfixIterator = class(TSDVAbstractTreeIteratorRecursive)
   private
     (* Private declarations *)
   protected
     (* Protected declarations *)
   public
     (* Public declarations *)

     procedure Start(); override;
     procedure Stop(); override;

     function Next(): Boolean; override;
     function Current(): TObject; override;
   end;

implementation

(* TSDVTreeInmediateForBackIterator *)

procedure TSDVTreeInmediateForBackIterator.Start;
begin
  //inherited Start();
end;

procedure TSDVTreeInmediateForBackIterator.Stop;
begin
  //inherited Stop();
end;

function TSDVTreeInmediateForBackIterator.Next: Boolean;
begin
  Result := false;
end;

function TSDVTreeInmediateForBackIterator.Current: TObject;
begin
  Result := nil;
end;

(* TSDVTreeInmediateForEachIterator *)

procedure TSDVTreeInmediateForEachIterator.Start();
begin
  //inherited Start();
end;

procedure TSDVTreeInmediateForEachIterator.Stop();
begin
  //inherited Stop();
end;

function TSDVTreeInmediateForEachIterator.Next: Boolean;
begin
  Result := false;
end;

function TSDVTreeInmediateForEachIterator.Current: TObject;
begin
  Result := nil;
end;

(* TSDVTreeUpIterator *)

procedure TSDVTreeUpIterator.Start();
begin
  //inherited Start();
end;

procedure TSDVTreeUpIterator.Stop();
begin
  //inherited Stop();
end;

function TSDVTreeUpIterator.Next: Boolean;
begin
  Result := false;
end;

function TSDVTreeUpIterator.Current: TObject;
begin
  Result := nil;
end;

(* TSDVTreePrefixPosfixIterator *)

procedure TSDVTreePrefixPosfixIterator.Start();
begin
  inherited Start();
end;

procedure TSDVTreePrefixPosfixIterator.Stop();
begin
  inherited Stop();
end;

function TSDVTreePrefixPosfixIterator.Next: Boolean;
begin
  Result := false;
end;

function TSDVTreePrefixPosfixIterator.Current: TObject;
begin
  Result := nil;
end;

(* TSDVTreePosfixIterator *)

procedure TSDVTreePosfixIterator.Start();
begin
  inherited Start();
end;

procedure TSDVTreePosfixIterator.Stop();
begin
  inherited Stop();
end;

function TSDVTreePosfixIterator.Next: Boolean;
begin
  Result := false;
end;

function TSDVTreePosfixIterator.Current: TObject;
begin
  Result := nil;
end;

(* TSDVTreePrefixIterator *)

procedure TSDVTreePrefixIterator.Start();
begin
  inherited Start();
end;

procedure TSDVTreePrefixIterator.Stop();
begin
  inherited Stop();
end;

function TSDVTreePrefixIterator.Next(): Boolean;
begin
  Result := false;
end;

function TSDVTreePrefixIterator.Current(): TObject;
begin
  Result := nil;
end;

(* TSDVAbstractTreeIteratorRecursive *)

procedure TSDVAbstractTreeIteratorRecursive.Start;
begin
  if (FStack <> nil) then
  begin
    FStack.Free();
    FStack := nil;
  end;

  FStack := TObjectStack.Create();
end;

procedure TSDVAbstractTreeIteratorRecursive.Stop;
begin
  FStack.Free();
end;

function TSDVAbstractTreeIteratorRecursive.Next: Boolean;
begin
  Result := false;
end;

function TSDVAbstractTreeIteratorRecursive.Current: TObject;
begin
  Result := nil;
end;

(* TSDVAbstractTreeIterator *)

function TSDVAbstractTreeIterator.Collection(): TSDVTreeCollection;
begin
  Result := FCollection;
end;

constructor TSDVAbstractTreeIterator.Create();
begin
  inherited Create();
end;

destructor TSDVAbstractTreeIterator.Destroy();
begin
  inherited Destroy();
end;

end.

