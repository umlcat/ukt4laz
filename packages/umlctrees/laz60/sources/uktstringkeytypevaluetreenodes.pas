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

unit uktstringkeytypevaluetreenodes;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
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
 ** Additionally, a "Key", "Type, "Value" group of properties,
 ** and helper functions are provided,
 ** for each item.
 **
 ** Note: Its not a "KeyTypeValue" list property.
 **)

 type

 (* TSDVStringKeyTypeValueTreeNode *)

   TSDVStringKeyTypeValueTreeNode = class(TSDVTreeNode)
   private
     (* Private declarations *)
   protected
     (* Protected declarations *)

     FKey:    string;
     FTypeID: string;
     FValue:  string;
   protected
     (* Protected declarations *)

   public
     (* Public declarations *)

     (* accesor declarations *)

     function getKey(): string; virtual;
     function getTypeID(): string; virtual;
     function getValue(): string; virtual;

     procedure setKey(const AValue: string); virtual;
     procedure setTypeID(const AValue: string); virtual;
     procedure setValue(const AValue: string); virtual;
   public
     (* Public declarations *)

     procedure DoCreate(); override;
     procedure DoDestroy(); override;
   public
     (* Public declarations *)

     function MatchKey(const AKey: string): Boolean;
     function MatchType(const AType: string): Boolean;
     function MatchValue(const AValue: string): Boolean;

     function EqualKey(const AKey: string): Boolean;
     function EqualType(const AType: string): Boolean;
     function EqualValue(const AValue: string): Boolean;

     function SameKey(const AKey: string): Boolean;
     function SameType(const AType: string): Boolean;
     function SameValue(const AValue: string): Boolean;

     function InsertByKeyTypeValue
       (const AKey, AType, AValue: string): TSDVStringKeyTypeValueTreeNode;
     function InsertByKey
       (const AKey: string): TSDVStringKeyTypeValueTreeNode;
     function InsertByKeyValue
       (const AKey, AValue: string): TSDVStringKeyTypeValueTreeNode;
     function InsertByKeyType
       (const AKey, AType: string): TSDVStringKeyTypeValueTreeNode;
   public
     (* Public declarations *)

     function NodeOfKey
       (const AKey: string): TSDVStringKeyTypeValueTreeNode;
     function NodeOfType
       (const AType: string): TSDVStringKeyTypeValueTreeNode;
     function NodeOfValue
       (const AValue: string): TSDVStringKeyTypeValueTreeNode;

     function KeyFound(const AKey: string): Boolean;
     function TypeFound(const AValue: string): Boolean;
     function ValueFound(const AValue: string): Boolean;

     function ReplaceFirstKey
       (const APrevKey, ANewKey: string): Boolean;
     function ReplaceFirstTypeByKey
       (const AKey, AType: string): Boolean;
     function ReplaceFirstValueByKey
       (const AKey, AValue: string): Boolean;
     function ReplaceFirstValue
       (const APrevValue, ANewValue: string): Boolean;
     function ReplaceAllValues
       (const APrevValue, ANewValue: string): Boolean;
     function ReplaceAllTypes
       (const APrevType, ANewType: string): Boolean;
     function ReplaceAllValuesByType
       (const AType, ANewValue: string): Boolean;
   public
     (* Public declarations *)

     property Key: string
       read getKey write setKey;
     property TypeID: string
       read getTypeID write setTypeID;
     property Value: string
       read getValue write setValue;
   end;

 (* TSDVStringKeyTypeValueTreeCollection *)

   TSDVStringKeyTypeValueTreeCollection = class(TSDVTreeCollection)
   private
     (* Private declarations *)
   protected
     (* Protected declarations *)

     _AllowDuplicates: boolean;
     _IgnoreKeyCase:   boolean;
     _IgnoreTypeCase:  boolean;
     _IgnoreValueCase: boolean;
   protected
     (* Protected declarations *)

     (* accesor declarations *)

     function getAllowDuplicates(): boolean;
     function getIgnoreKeyCase(): boolean;
     function getIgnoreTypeCase(): boolean;
     function getIgnoreValueCase(): boolean;

     procedure setAllowDuplicates(const AValue: boolean);
     procedure setIgnoreKeyCase(const AValue: boolean);
     procedure setIgnoreTypeCase(AValue: boolean);
     procedure setIgnoreValueCase(const AValue: boolean);
   protected
     (* Protected declarations *)

     function CreateNodeByClass(): TSDVTreeNode; override;
   public
     (* Friend Protected declarations *)

     function InternalMatchKeys(const A, B: string): Boolean;
     function InternalMatchTypes(const A, B: string): Boolean;
     function InternalMatchValues(const A, B: string): Boolean;
   public
     (* Public declarations *)

     property AllowDuplicates: boolean
       read getAllowDuplicates write setAllowDuplicates;
     property IgnoreKeyCase: boolean
       read getIgnoreKeyCase write setIgnoreKeyCase;
     property IgnoreTypeCase: boolean
       read getIgnoreTypeCase write setIgnoreTypeCase;
     property IgnoreValueCase: boolean
       read getIgnoreValueCase write setIgnoreValueCase;
   end;

implementation

(* TSDVStringKeyTypeValueTreeNode *)

function TSDVStringKeyTypeValueTreeNode.getKey(): string;
begin
  Result := Self.FKey;
end;

function TSDVStringKeyTypeValueTreeNode.getTypeID(): string;
begin
  Result := Self.FTypeID;
end;

function TSDVStringKeyTypeValueTreeNode.getValue(): string;
begin
  Result := Self.FValue;
end;

procedure TSDVStringKeyTypeValueTreeNode.setKey(const AValue: string);
begin
  Self.FKey := AValue;
end;

procedure TSDVStringKeyTypeValueTreeNode.setTypeID(const AValue: string);
begin
  Self.FTypeID := AValue;
end;

procedure TSDVStringKeyTypeValueTreeNode.setValue(const AValue: string);
begin
  Self.FValue := AValue;
end;

procedure TSDVStringKeyTypeValueTreeNode.DoCreate();
begin
  inherited DoCreate();

  Self.FKey   := '';
  Self.FValue := '';
end;

procedure TSDVStringKeyTypeValueTreeNode.DoDestroy();
begin
  Self.FValue := '';
  Self.FKey   := '';

  inherited DoDestroy();
end;

function TSDVStringKeyTypeValueTreeNode.MatchKey(const AKey: string): Boolean;
var ACollection: TSDVStringKeyTypeValueTreeCollection;
begin
  ACollection := TSDVStringKeyTypeValueTreeCollection(InternalCollection);
  Result := ACollection.InternalMatchKeys(Self.FKey, AKey);
end;

function TSDVStringKeyTypeValueTreeNode.MatchType(const AType: string): Boolean;
var ACollection: TSDVStringKeyTypeValueTreeCollection;
begin
  ACollection := TSDVStringKeyTypeValueTreeCollection(InternalCollection);
  Result := ACollection.InternalMatchKeys(Self.FTypeID, AType);
end;

function TSDVStringKeyTypeValueTreeNode.MatchValue(const AValue: string): Boolean;
var ACollection: TSDVStringKeyTypeValueTreeCollection;
begin
  ACollection := TSDVStringKeyTypeValueTreeCollection(InternalCollection);
  Result := ACollection.InternalMatchKeys(Self.FValue, AValue);
end;

function TSDVStringKeyTypeValueTreeNode.EqualKey(const AKey: string): Boolean;
begin
  Result := (Self.FKey = AKey);
end;

function TSDVStringKeyTypeValueTreeNode.EqualType(const AType: string): Boolean;
begin
  Result := (Self.FTypeID = AType);
end;

function TSDVStringKeyTypeValueTreeNode.EqualValue(const AValue: string): Boolean;
begin
  Result := (Self.FValue = AValue);
end;

function TSDVStringKeyTypeValueTreeNode.SameKey(const AKey: string): Boolean;
begin
  Result := uktstrings.SameText(Self.FKey, AKey);
end;

function TSDVStringKeyTypeValueTreeNode.SameType(const AType: string): Boolean;
begin
  Result := uktstrings.SameText(Self.FTypeID, AType);
end;

function TSDVStringKeyTypeValueTreeNode.SameValue(const AValue: string): Boolean;
begin
  Result := uktstrings.SameText(Self.FValue, AValue);
end;

function TSDVStringKeyTypeValueTreeNode.InsertByKeyTypeValue
  (const AKey, AType, AValue: string): TSDVStringKeyTypeValueTreeNode;
var CanInsert: Boolean; ACollection: TSDVStringKeyTypeValueTreeCollection;
begin
  CanInsert := true;
  ACollection := TSDVStringKeyTypeValueTreeCollection(InternalCollection);
  if (not ACollection.AllowDuplicates) then
  begin
    CanInsert := (not MatchKey(AKey));
  end;

  if (CanInsert) then
  begin
    Result := (Self.Insert() as TSDVStringKeyTypeValueTreeNode);
    Result.Key    := AKey;
    Result.TypeID := AType;
    Result.Value  := AValue;
  end;
end;

function TSDVStringKeyTypeValueTreeNode.InsertByKey
  (const AKey: string): TSDVStringKeyTypeValueTreeNode;
begin
  Result := Self.InsertByKeyTypeValue(AKey, '', '');
end;

function TSDVStringKeyTypeValueTreeNode.InsertByKeyValue
  (const AKey, AValue: string): TSDVStringKeyTypeValueTreeNode;
begin
  Result := Self.InsertByKeyTypeValue(AKey, '', AValue);
end;

function TSDVStringKeyTypeValueTreeNode.InsertByKeyType
  (const AKey, AType: string): TSDVStringKeyTypeValueTreeNode;
begin
  Result := Self.InsertByKeyTypeValue(AKey, AType, '');
end;

function TSDVStringKeyTypeValueTreeNode.NodeOfKey
  (const AKey: string): TSDVStringKeyTypeValueTreeNode;
begin
  Result := nil;
end;

function TSDVStringKeyTypeValueTreeNode.NodeOfType
  (const AType: string): TSDVStringKeyTypeValueTreeNode;
begin
  Result := nil;
end;

function TSDVStringKeyTypeValueTreeNode.NodeOfValue
  (const AValue: string): TSDVStringKeyTypeValueTreeNode;
begin
  Result := nil;
end;

function TSDVStringKeyTypeValueTreeNode.KeyFound(const AKey: string): Boolean;
begin
  Result := false;
end;

function TSDVStringKeyTypeValueTreeNode.TypeFound
  (const AValue: string): Boolean;
begin
  Result := false;
end;

function TSDVStringKeyTypeValueTreeNode.ValueFound(const AValue: string): Boolean;
begin
  Result := false;
end;

function TSDVStringKeyTypeValueTreeNode.ReplaceFirstKey
  (const APrevKey, ANewKey: string): Boolean;
begin
  Result := false;
end;

function TSDVStringKeyTypeValueTreeNode.ReplaceFirstTypeByKey
  (const AKey, AType: string): Boolean;
begin
  Result := false;
end;

function TSDVStringKeyTypeValueTreeNode.ReplaceFirstValueByKey
  (const AKey, AValue: string): Boolean;
begin
  Result := false;
end;

function TSDVStringKeyTypeValueTreeNode.ReplaceFirstValue
  (const APrevValue, ANewValue: string): Boolean;
begin
  Result := false;
end;

function TSDVStringKeyTypeValueTreeNode.ReplaceAllValues
  (const APrevValue, ANewValue: string): Boolean;
begin
  Result := false;
end;

function TSDVStringKeyTypeValueTreeNode.ReplaceAllTypes
  (const APrevType, ANewType: string): Boolean;
begin
  Result := false;
end;

function TSDVStringKeyTypeValueTreeNode.ReplaceAllValuesByType
  (const AType, ANewValue: string): Boolean;
begin
  //
end;

(* TSDVStringKeyTypeValueTreeCollection *)

function TSDVStringKeyTypeValueTreeCollection.getAllowDuplicates(): boolean;
begin
  Result := Self._AllowDuplicates;
end;

function TSDVStringKeyTypeValueTreeCollection.getIgnoreKeyCase(): boolean;
begin
  Result := Self._IgnoreKeyCase;
end;

function TSDVStringKeyTypeValueTreeCollection.getIgnoreTypeCase: boolean;
begin
  Result := Self._IgnoreTypeCase;
end;

function TSDVStringKeyTypeValueTreeCollection.getIgnoreValueCase(): boolean;
begin
  Result := Self._IgnoreValueCase;
end;

procedure TSDVStringKeyTypeValueTreeCollection.setAllowDuplicates(const AValue: boolean);
begin
  Self._AllowDuplicates := AValue;
end;

procedure TSDVStringKeyTypeValueTreeCollection.setIgnoreKeyCase(const AValue: boolean);
begin
  Self._IgnoreKeyCase := AValue;
end;

procedure TSDVStringKeyTypeValueTreeCollection.setIgnoreTypeCase
  (AValue: boolean);
begin
  Self._IgnoreTypeCase := AValue;
end;

procedure TSDVStringKeyTypeValueTreeCollection.setIgnoreValueCase(const AValue: boolean);
begin
  Self._IgnoreValueCase := AValue;
end;

function TSDVStringKeyTypeValueTreeCollection.CreateNodeByClass(): TSDVTreeNode;
begin
  Result := TSDVStringKeyTypeValueTreeNode.Create();
  Result.DoCreate();
end;

function TSDVStringKeyTypeValueTreeCollection.InternalMatchKeys
  (const A, B: string): Boolean;
begin
  Result := false;
  if (IgnoreKeyCase) then
  begin
    Result := uktstrings.SameText(A, B);
  end else
  begin
    Result := (A = B);
  end;
end;

function TSDVStringKeyTypeValueTreeCollection.InternalMatchTypes
  (const A, B: string): Boolean;
begin
  Result := false;
  if (IgnoreTypeCase) then
  begin
    Result := uktstrings.SameText(A, B);
  end else
  begin
    Result := (A = B);
  end;
end;

function TSDVStringKeyTypeValueTreeCollection.InternalMatchValues
  (const A, B: string): Boolean;
begin
  Result := false;
  if (IgnoreValueCase) then
  begin
    Result := uktstrings.SameText(A, B);
  end else
  begin
    Result := (A = B);
  end;
end;

end.

