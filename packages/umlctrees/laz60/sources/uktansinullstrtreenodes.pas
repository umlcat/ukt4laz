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

unit uktansinullstrtreenodes;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  uktcomparisons,
  uktansinullstrs,
  ukttreenodes,
  dummy;

(**
 ** Description:
 ** This unit contains several in order to support an non visual,
 ** hierarchical ( "tree" ) collection of data.
 **
 ** It was not designed with generics collections.
 **
 ** Additionally, a "Text" property and helper functions are provided,
 **
 ** The "Text" property is a null-character-terminated,
 ** dynamic memory-allocated string, like ANSI-C strings.
 **)

type

 (* TSDVNullStringTreeNode *)

   TSDVNullStringTreeNode = class(TSDVTreeNode)
   private
     (* Private declarations *)
   protected
     (* Protected declarations *)

     FText: ansinullstring;
   protected
     (* Protected declarations *)

     (* Accessors declarations *)

     function getText(): ansinullstring; virtual;

     procedure setText(const AValue: ansinullstring); virtual;
   public
     (* Public declarations *)

     function IsEmpty(): Boolean;

     function EqualAs(const AStr: ansinullstring): Boolean;
     function SameAs(const AStr: ansinullstring): Boolean;

     function StartsWith(const ASubStr: ansinullstring): Boolean;
     function FinishesWith(const ASubStr: ansinullstring): Boolean;

     function ComparesWith(const AStr: ansinullstring): TComparison;

     procedure Clear();
   public
     (* Public declarations *)

     property Text: ansinullstring
      read getText write setText;
   end;

 (* TSDVNullStringTreeCollection *)

   TSDVNullStringTreeCollection = class(TSDVTreeCollection)
   private
     (* Private declarations *)
   protected
     (* Protected declarations *)
   protected
     (* Protected declarations *)

     function CreateNodeByClass(): TSDVTreeNode; override;
   public
     (* Friend Protected declarations *)


   public
     (* Public declarations *)
   end;

implementation

(* TSDVNullStringTreeNode *)

function TSDVNullStringTreeNode.getText(): ansinullstring;
begin
  Result := FText;
end;

procedure TSDVNullStringTreeNode.setText(const AValue: ansinullstring);
//var ACollection: TSDVNullStringTreeCollection;
begin
  (*
  if (not EqualAs(FText, AValue)) then
  begin
    ACollection := TSDVNullStringTreeCollection(InternalCollection);
    ACollection.RequestChangeText(Self, AValue);
  end;
  *)
end;

function TSDVNullStringTreeNode.IsEmpty(): Boolean;
begin
  Result := uktansinullstrs.IsEmpty(FText);
end;

function TSDVNullStringTreeNode.EqualAs(const AStr: ansinullstring): Boolean;
begin
  Result := uktansinullstrs.Equal(FText, AStr);
end;

function TSDVNullStringTreeNode.SameAs(const AStr: ansinullstring): Boolean;
begin
  Result := uktansinullstrs.SameText(FText, AStr);
  // Goal: Case Insensitive comparison.
end;

function TSDVNullStringTreeNode.StartsWith
  (const ASubStr: ansinullstring): Boolean;
begin
  Result := uktansinullstrs.StartsWith(ASubStr, FText);
end;

function TSDVNullStringTreeNode.FinishesWith
  (const ASubStr: ansinullstring): Boolean;
begin
  Result := uktansinullstrs.FinishesWith(ASubStr, FText);
end;

function TSDVNullStringTreeNode.ComparesWith
  (const AStr: ansinullstring): TComparison;
begin
  Result := uktansinullstrs.Compare(AStr, FText);
end;

procedure TSDVNullStringTreeNode.Clear();
begin
  //Self.Text := '';
end;

(* TSDVNullStringTreeCollection *)

function TSDVNullStringTreeCollection.CreateNodeByClass(): TSDVTreeNode;
begin
  Result := TSDVNullStringTreeNode.Create();
  Result.DoCreate();
end;

end.

