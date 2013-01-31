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

unit uktshortstrtreenodes;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  uktcomparisons,
  uktshortansistrs,
  ukttreenodes,
  dummy;

(**
 ** Description:
 ** This unit contains several in order to support an non visual,
 ** hierarchical ( "tree" ) collection of data.
 **
 ** It was not designed with generics collections.
 **
 ** Additionally, a "Text" property and helper functions are provided.
 **
 ** The "Text" property is a standard Pascal 256 bytes array,
 ** with the first, zero indexed item, storing the count of characters.
 **)

type

 (* TSDVShortStringTreeNode *)

   TSDVShortStringTreeNode = class(TSDVTreeNode)
   private
     (* Private declarations *)
   protected
     (* Protected declarations *)

     FText: shortstring;
   protected
     (* Protected declarations *)

     (* Accessors declarations *)

     function getText(): shortstring; virtual;

     procedure setText(const AValue: shortstring); virtual;
   public
     (* Public declarations *)

     function IsEmpty(): Boolean;

     function EqualAs(const AStr: shortstring): Boolean;
     function SameAs(const AStr: shortstring): Boolean;

     function StartsWith(const ASubStr: shortstring): Boolean;
     function FinishesWith(const ASubStr: shortstring): Boolean;

     function ComparesWith(const AStr: shortstring): TComparison;

     procedure Clear();
   public
     (* Public declarations *)

     property Text: shortstring
      read getText write setText;
   end;

 (* TSDVPascalStringTreeCollection *)

   TSDVPascalStringTreeCollection = class(TSDVTreeCollection)
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

(* TSDVShortStringTreeNode *)

function TSDVShortStringTreeNode.getText(): shortstring;
begin
  Result := FText;
end;

procedure TSDVShortStringTreeNode.setText(const AValue: shortstring);
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

function TSDVShortStringTreeNode.IsEmpty: Boolean;
begin
  Result := uktshortansistrs.IsEmpty(FText);
end;

function TSDVShortStringTreeNode.EqualAs(const AStr: shortstring): Boolean;
begin
  Result := (FText = AStr);
  // Goal: Case Sensitive comparison.
end;

function TSDVShortStringTreeNode.SameAs(const AStr: shortstring): Boolean;
begin
  Result := uktshortansistrs.SameText(FText, AStr);
  // Goal: Case Insensitive comparison.
end;

function TSDVShortStringTreeNode.StartsWith
  (const ASubStr: shortstring): Boolean;
begin
  Result := uktshortansistrs.StartsWith(ASubStr, FText);
end;

function TSDVShortStringTreeNode.FinishesWith
  (const ASubStr: shortstring): Boolean;
begin
  Result := uktshortansistrs.FinishesWith(ASubStr, FText);
end;

function TSDVShortStringTreeNode.ComparesWith
  (const AStr: shortstring): TComparison;
begin
  Result := uktshortansistrs.Compare(AStr, FText);
end;

procedure TSDVShortStringTreeNode.Clear();
begin
  Self.Text := '';
end;

(* TSDVPascalStringTreeCollection *)

function TSDVPascalStringTreeCollection.CreateNodeByClass(): TSDVTreeNode;
begin
  Result := TSDVShortStringTreeNode.Create();
  Result.DoCreate();
end;

end.

