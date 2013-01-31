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

unit uktansimemotreenodes;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  uktcomparisons,
  uktansimemos,
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
 ** The "Text" property is a special string type,
 ** similar to the standard pascal string,
 ** but, supports more than 256 bytes, includes a field for length.
 **)

 type

  (* TSDVANSIMemoTreeNode *)

    TSDVANSIMemoTreeNode = class(TSDVTreeNode)
    private
      (* Private declarations *)
    protected
      (* Protected declarations *)

      FText: ansimemo;

      (* Accessors declarations *)

      function getText(): ansimemo; virtual;

      procedure setText(const AValue: ansimemo); virtual;
    public
      (* Public declarations *)

      function IsEmpty(): Boolean;

      function EqualAs(const AStr: ansimemo): Boolean;
      function SameAs(const AStr: ansimemo): Boolean;

      function StartsWith(const ASubStr: ansimemo): Boolean;
      function FinishesWith(const ASubStr: ansimemo): Boolean;

      function ComparesWith(const AStr: ansimemo): TComparison;

      procedure Clear();
    public
      (* Public declarations *)

      property Text: ansimemo
       read getText write setText;
    end;

  (* TSDVANSIMemoTreeCollection *)

    TSDVANSIMemoTreeCollection = class(TSDVTreeCollection)
    private
      (* Private declarations *)
    protected
      (* Protected declarations *)

      function CreateNodeByClass(): TSDVTreeNode; override;
    public
      (* Friend Protected declarations *)
    public
      (* Public declarations *)
    end;


implementation

(* TSDVANSIMemoTreeNode *)

function TSDVANSIMemoTreeNode.getText(): ansimemo;
begin
  Result := FText;
end;

procedure TSDVANSIMemoTreeNode.setText(const AValue: ansimemo);
//var ACollection: TSDVNullStringTreeCollection;
begin
  (*
  if (EqualAs(FText, AValue)) then
  begin
    ACollection := TSDVNullStringTreeCollection(InternalCollection);
    ACollection.RequestChangeText(Self, AValue);
  end;
  *)
end;

function TSDVANSIMemoTreeNode.IsEmpty(): Boolean;
begin
  Result := uktansimemos.IsEmpty(FText);
end;

function TSDVANSIMemoTreeNode.EqualAs(const AStr: ansimemo): Boolean;
begin
  Result := uktansimemos.Equal(FText, AStr);
  // Goal: Case Sensitive comparison.
end;

function TSDVANSIMemoTreeNode.SameAs(const AStr: ansimemo): Boolean;
begin
  Result := uktansimemos.SameText(FText, AStr);
  // Goal: Case Insensitive comparison.
end;

function TSDVANSIMemoTreeNode.StartsWith(const ASubStr: ansimemo): Boolean;
begin
  Result := uktansimemos.StartsWith(ASubStr, FText);
end;

function TSDVANSIMemoTreeNode.FinishesWith(const ASubStr: ansimemo): Boolean;
begin
  Result := uktansimemos.FinishesWith(ASubStr, FText);
end;

function TSDVANSIMemoTreeNode.ComparesWith(const AStr: ansimemo): TComparison;
begin
  Result := uktansimemos.Compare(AStr, FText);
end;

procedure TSDVANSIMemoTreeNode.Clear();
begin
  uktansimemos.Clear(Self.FText);
end;

(* TSDVANSIMemoTreeCollection *)

function TSDVANSIMemoTreeCollection.CreateNodeByClass(): TSDVTreeNode;
begin
  Result := TSDVANSIMemoTreeNode.Create();
  Result.DoCreate();
end;



end.

