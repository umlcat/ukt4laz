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

unit ukttextreenodes;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  uktcomparisons,
  uktstrings,
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
 ** The "Text" property is a reference-allocated Delphi/FreePascal string,
 ** also known as an "openstring".
 **)

type

 (* TSDVTextTreeNode *)

   TSDVTextTreeNode = class(TSDVTreeNode)
   private
     (* Private declarations *)
   protected
     (* Protected declarations *)

     FText: string;
   protected
     (* Protected declarations *)

     (* Accessors declarations *)

     function getText(): string; virtual;

     procedure setText(const AValue: string); virtual;
   protected
     (* Protected declarations *)

     procedure ConfirmedCopyTo(var ADestNode: TSDVTreeNode); override;
     procedure ConfirmedMoveTo(var ADestNode: TSDVTreeNode); override;
   public
     (* Friend Protected declarations *)

     procedure ConfirmedChangeText(const AText: string); virtual;
   public
     (* Public declarations *)

     procedure DoCreate(); override;
     procedure DoDestroy(); override;
   public
     (* Public declarations *)

     (* readonly functions *)

     function IsEmpty(): Boolean;
   public
     (* Public declarations *)

     function EqualAs(const AStr: string): Boolean;
     function SameAs(const AStr: string): Boolean;

     function StartsWith(const ASubStr: string): Boolean;
     function FinishesWith(const ASubStr: string): Boolean;

     function ComparesWith(const AStr: string): TComparison;

     procedure Clear();
   public
     (* Public declarations *)

     property Text: string
      read getText write setText;
   end;

 (* TSDVTextTreeCollection *)

   TSDVTextTreeCollection = class(TSDVTreeCollection)
   private
     (* Private declarations *)
   protected
     (* Protected declarations *)

     function CreateNodeByClass(): TSDVTreeNode; override;
   protected
     (* Protected declarations *)

     procedure InternalBeforeChangeText
       (const ANode: TSDVTextTreeNode;
        const AText: string); virtual;
     procedure InternalAfterChangeText
       (const ANode: TSDVTextTreeNode); virtual;
     procedure InternalConfirmedChangeText
       (const ANode: TSDVTextTreeNode;
        const AText: string); virtual;

     procedure InternalAfterInsert
       (const AParentNode, ANode: TSDVTreeNode;
        const AIndex: Integer); override;
   public
     (* Friend Protected declarations *)

     procedure RequestChangeText
       (const ANode: TSDVTextTreeNode;
        const AText: string); (* nonvirtual; *)

     function DefaultText(): string; virtual;
   public
     (* Public declarations *)
   end;

implementation

(* TSDVTextTreeNode *)

function TSDVTextTreeNode.getText(): string;
begin
  Result := FText;
end;

procedure TSDVTextTreeNode.setText(const AValue: string);
var ACollection: TSDVTextTreeCollection;
begin
  if (not EqualAs(AValue)) then
  begin
    ACollection := TSDVTextTreeCollection(InternalCollection);
    ACollection.RequestChangeText(Self, AValue);
  end;
end;

procedure TSDVTextTreeNode.ConfirmedCopyTo(var ADestNode: TSDVTreeNode);
var ThisDestNode: TSDVTextTreeNode;
begin
  // perform copy of fields specific to parent class
  inherited ConfirmedCopyTo(ADestNode);

  // cast to current type
  ThisDestNode := TSDVTextTreeNode(ADestNode);

  // perform copy of fields specific to this class
  ThisDestNode.Text := Self.Text;
end;

procedure TSDVTextTreeNode.ConfirmedMoveTo(var ADestNode: TSDVTreeNode);
var ThisDestNode: TSDVTextTreeNode;
begin
  // perform move of fields specific to parent class
  inherited ConfirmedMoveTo(ADestNode);

  // cast to current type
  ThisDestNode := TSDVTextTreeNode(ADestNode);

  // perform move of fields specific to this class
  ThisDestNode.Text := Self.Text;
  Self.Text         := '';
end;

procedure TSDVTextTreeNode.ConfirmedChangeText(const AText: string);
begin
  FText := AText;
end;

procedure TSDVTextTreeNode.DoCreate();
begin
  inherited DoCreate();
  FText := '';
end;

procedure TSDVTextTreeNode.DoDestroy();
begin
  FText := '';
  inherited DoDestroy();
end;

function TSDVTextTreeNode.IsEmpty(): Boolean;
begin
  Result := (FText = '');
end;

function TSDVTextTreeNode.EqualAs(const AStr: string): Boolean;
begin
  Result := (FText = AStr);
  // Goal: Case sensitive comparison.
end;

function TSDVTextTreeNode.SameAs(const AStr: string): Boolean;
begin
  Result := uktstrings.SameText(FText, AStr);
  // Goal: Case Insensitive comparison.
end;

function TSDVTextTreeNode.StartsWith(const ASubStr: string): Boolean;
begin
  Result := uktstrings.StartsWith(ASubStr, FText);
end;

function TSDVTextTreeNode.FinishesWith(const ASubStr: string): Boolean;
begin
  Result := uktstrings.FinishesWith(ASubStr, FText);
end;

function TSDVTextTreeNode.ComparesWith(const AStr: string): TComparison;
begin
  Result := uktstrings.Compare(AStr, FText);
end;

procedure TSDVTextTreeNode.Clear();
begin
  Self.Text := '';
end;

(* TSDVTextTreeCollection *)

function TSDVTextTreeCollection.CreateNodeByClass(): TSDVTreeNode;
begin
  Result := TSDVTextTreeNode.Create();
  Result.DoCreate();
end;

procedure TSDVTextTreeCollection.InternalBeforeChangeText
  (const ANode: TSDVTextTreeNode; const AText: string);
begin
  Self.DoNothing();
end;

procedure TSDVTextTreeCollection.InternalAfterChangeText
  (const ANode: TSDVTextTreeNode);
begin
  Self.DoNothing();
end;

procedure TSDVTextTreeCollection.InternalConfirmedChangeText
  (const ANode: TSDVTextTreeNode; const AText: string);
begin
  ANode.ConfirmedChangeText(AText);
end;

procedure TSDVTextTreeCollection.RequestChangeText
  (const ANode: TSDVTextTreeNode; const AText: string);
begin
  InternalBeforeChangeText(ANode, AText);
  InternalConfirmedChangeText(ANode, AText);
  InternalAfterChangeText(ANode);
end;

procedure TSDVTextTreeCollection.InternalAfterInsert
  (const AParentNode, ANode: TSDVTreeNode; const AIndex: Integer);
var ATextNode: TSDVTextTreeNode; AText: string;
begin
  ATextNode      := TSDVTextTreeNode(ANode);
  AText          := DefaultText();
  ATextNode.Text := AText;
end;

function TSDVTextTreeCollection.DefaultText(): string;
begin
  Result := ClassName() + SysUtils.IntToStr((FGlobalSequencer + 1));
end;

end.

