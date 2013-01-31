(*****************************************************************************
 *                                                                           *
 *  This file is part of the UMLCat Component Library.                       *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
 **)

unit uktsxmltreecntrs;

{$mode objfpc}{$H+}

interface
uses
  Classes, SysUtils,
  uktguids,
  uktguidstrs,
  uktmsgtypes,
  ukttreenodes,
  ukttreecntrs,
  uktmsgtreecntrs,
  uktxmlfileansisymbols,
  uktxmlfiletreenodetokens,
  dummy;

  // --> Simple eXtensible Markup Language
  // --> X.M.L. tags without attributes
  // --> "Think Different" --> "Think Simple"

(**
 ** Description:
 ** This unit declares collections of treenodes,
 ** that store Simple X.M.L. parsed tokens.
 **)

const
  msgTreeNodeAfterChangeSymbol
    = '{5D7621DC-FDF38B46-8DBD1AB3-4B385EC9}';
  msgTreeNodeAfterChangeTextValue
    = '{E11EA271-C8182F46-88943547-542C58E7}';
  msgTreeNodeAfterChangeTreeToken
    = '{D3983359-C867C24B-9D9A0C31-3699D24D}';

type

(* TSDVSXMLTreeNode *)

  TSDVSXMLTreeNode = class(TSDVMsgContainerTreeNode)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)

    (* field declarations *)

    FSymbol: TXMLFileANSISymbol;

    FCanInsert: Boolean;
    FCanEdit:   Boolean;
    FCanRemove: Boolean;

    FNewLineAfter: Boolean;
  protected
    (* Protected declarations *)

    (* accesors declarations *)

    function getSymbol(): TXMLFileANSISymbol;
    function getTextValue(): string;
    function getTreeToken(): TXMLFileTreeNodeToken;
    function getCanEdit(): Boolean; virtual;
    function getCanInsert(): Boolean; virtual;
    function getCanRemove(): Boolean; virtual;
    function getNewLineAfter(): Boolean; virtual;

    procedure setSymbol(const AValue: TXMLFileANSISymbol);
    procedure setTextValue(const AValue: string);
    procedure setTreeToken(const AValue: TXMLFileTreeNodeToken);
    procedure setCanRemove(const AValue: Boolean); virtual;
    procedure setCanEdit(const AValue: Boolean); virtual;
    procedure setCanInsert(const AValue: Boolean); virtual;
    procedure setNewLineAfter(const AValue: Boolean); virtual;
  protected
    (* Protected declarations *)

    procedure ConfirmedCopyTo(var ADestNode: TSDVTreeNode); override;
    procedure ConfirmedMoveTo(var ADestNode: TSDVTreeNode); override;
  public
    (* Public declarations *)

    procedure DoCreate(); override;
    procedure DoDestroy(); override;
  public
    (* Public declarations *)

    property Symbol: TXMLFileANSISymbol
      read getSymbol write setSymbol;

    // can subitems be added to this node
    property CanInsert: Boolean
      read getCanInsert write setCanInsert;
    // can remove this node
    property CanRemove: Boolean
      read getCanRemove write setCanRemove;
    // can chnage the value of this node
    property CanEdit: Boolean
      read getCanEdit write setCanEdit;

    property NewLineAfter: Boolean
      read getNewLineAfter write setNewLineAfter;

    // "TextValue" will store the full parsed value,
    // "Text" will only store a short readable version of the token
    property TextValue: string
      read getTextValue write setTextValue;

    property TreeToken: TXMLFileTreeNodeToken
      read getTreeToken write setTreeToken;
  end;

(* TCustomSDVSXMLTreeCollection *)

  TCustomSDVSXMLTreeCollection = class(TSDVMsgContainerTreeCollection)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)

    function CreateNodeByClass(): TSDVTreeNode; override;
  public
    (* Public declarations *)
  end;

(* TCustomSDVSXMLTreeContainer *)

  TCustomSDVSXMLTreeContainer = class(TCustomSDVMsgTreeContainer)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)
  public
    (* Friend Protected declarations *)

    function CreateCollectionByClass(): TSDVContainerTreeCollection; override;
  public
    (* Public declarations *)
  end;

implementation

(* TSDVSXMLTreeNode *)

function TSDVSXMLTreeNode.getSymbol(): TXMLFileANSISymbol;
begin
  Result := FSymbol;
end;

function TSDVSXMLTreeNode.getTextValue(): string;
begin
  Result := FSymbol.Text;
end;

function TSDVSXMLTreeNode.getTreeToken(): TXMLFileTreeNodeToken;
begin
  Result := FSymbol.TreeToken;
end;

function TSDVSXMLTreeNode.getCanEdit(): Boolean;
begin
  Result := FCanEdit;
end;

function TSDVSXMLTreeNode.getCanInsert(): Boolean;
begin
  Result := FCanInsert;
end;

function TSDVSXMLTreeNode.getCanRemove(): Boolean;
begin
  Result := FCanRemove;
end;

function TSDVSXMLTreeNode.getNewLineAfter(): Boolean;
begin
  Result := FNewLineAfter;
end;

procedure TSDVSXMLTreeNode.setSymbol(const AValue: TXMLFileANSISymbol);
var AMsgRec: TSDVMessageParamsRecord;
begin
  FSymbol := AValue;

  uktguidstrs.DoubleStrToGUID
    (msgTreeNodeAfterChangeSymbol, AMsgRec.Message);

  AMsgRec.Sender  := Self;
  AMsgRec.Param   := @FSymbol;

  Self.SendMessage(AMsgRec);
end;

procedure TSDVSXMLTreeNode.setTextValue(const AValue: string);
var AMsgRec: TSDVMessageParamsRecord;
begin
  FSymbol.Text := AValue;

  uktguidstrs.DoubleStrToGUID
    (msgTreeNodeAfterChangeTextValue, AMsgRec.Message);

  AMsgRec.Sender  := Self;
  AMsgRec.Param   := @FSymbol;

  Self.SendMessage(AMsgRec);
end;

procedure TSDVSXMLTreeNode.setTreeToken(const AValue: TXMLFileTreeNodeToken);
var AMsgRec: TSDVMessageParamsRecord;
begin
  FSymbol.TreeToken := AValue;

  uktguidstrs.DoubleStrToGUID
    (msgTreeNodeAfterChangeTreeToken, AMsgRec.Message);

  AMsgRec.Sender  := Self;
  AMsgRec.Param   := @FSymbol;

  Self.SendMessage(AMsgRec);
end;

procedure TSDVSXMLTreeNode.setCanEdit(const AValue: Boolean);
begin
  FCanEdit := AValue;
end;

procedure TSDVSXMLTreeNode.setCanInsert(const AValue: Boolean);
begin
  FCanInsert := AValue;
end;

procedure TSDVSXMLTreeNode.setNewLineAfter(const AValue: Boolean);
begin

end;

procedure TSDVSXMLTreeNode.setCanRemove(const AValue: Boolean);
begin
  FCanRemove := AValue;
end;

procedure TSDVSXMLTreeNode.ConfirmedCopyTo
  (var ADestNode: TSDVTreeNode);
var ThisDestNode: TSDVSXMLTreeNode;
begin
  // perform copy of fields specific to parent class
  inherited ConfirmedCopyTo(ADestNode);

  // cast to current type
  ThisDestNode := TSDVSXMLTreeNode(ADestNode);

  // perform copy of fields specific to this class
  uktxmlfileansisymbols.Assign(ThisDestNode.FSymbol, Self.FSymbol);

  ThisDestNode.FCanInsert    := Self.FCanInsert;
  ThisDestNode.FCanEdit      := Self.FCanEdit;
  ThisDestNode.FCanRemove    := Self.FCanRemove;
  ThisDestNode.FNewLineAfter := Self.FNewLineAfter;
end;

procedure TSDVSXMLTreeNode.ConfirmedMoveTo
  (var ADestNode: TSDVTreeNode);
var ThisDestNode: TSDVSXMLTreeNode;
begin
  // perform move of fields specific to parent class
  inherited ConfirmedMoveTo(ADestNode);

  // cast to current type
  ThisDestNode := TSDVSXMLTreeNode(ADestNode);

  // perform move of fields specific to this class
  uktxmlfileansisymbols.Move(ThisDestNode.FSymbol, Self.FSymbol);

  ThisDestNode.FCanInsert := Self.FCanInsert;
  Self.FCanInsert      := false;
  ThisDestNode.FCanEdit   := Self.FCanEdit;
  Self.FCanEdit        := false;
  ThisDestNode.FCanRemove := Self.FCanRemove;
  Self.FCanRemove      := false;
  ThisDestNode.FNewLineAfter := Self.FNewLineAfter;
  Self.FNewLineAfter      := false;
end;

procedure TSDVSXMLTreeNode.DoCreate();
begin
  inherited DoCreate();
  uktxmlfileansisymbols.Clear(FSymbol);
end;

procedure TSDVSXMLTreeNode.DoDestroy();
begin
  uktxmlfileansisymbols.Clear(FSymbol);
  inherited DoDestroy();
end;

(* TCustomSDVSXMLTreeCollection *)

function TCustomSDVSXMLTreeCollection.CreateNodeByClass(): TSDVTreeNode;
begin
  Result := TSDVSXMLTreeNode.Create();
  Result.DoCreate();
end;

(* TCustomSDVSXMLTreeContainer *)

function TCustomSDVSXMLTreeContainer.CreateCollectionByClass(): TSDVContainerTreeCollection;
begin
  Result := TCustomSDVSXMLTreeCollection.Create();
  Result.DoCreate();
  // Goal: Create inheretable (polimorphic) collection.
  // Objetivo: Crear coleccion heredable (polimorfica).
end;

end.

