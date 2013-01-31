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

unit uktstringkeyvaluetreenodes;

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
 ** Additionally, a "Key" & "Value" pair of properties,
 ** and helper functions are provided,
 ** Note: Its not a "KeyValue" list property.
 **)

 type

 (* TSDVStringKeyValueTreeNode *)

   TSDVStringKeyValueTreeNode = class(TSDVTreeNode)
   private
     (* Private declarations *)
   protected
     (* Protected declarations *)

     _Key:   string;
     _Value: string;
   protected
     (* Protected declarations *)

   public
     (* Public declarations *)

     (* accesor declarations *)

     function getKey(): string; virtual;
     procedure setKey(const AValue: string); virtual;

     function getValue(): string; virtual;
     procedure setValue(const AValue: string); virtual;
   public
     (* Public declarations *)

     procedure DoCreate(); override;
     procedure DoDestroy(); override;
   public
     (* Public declarations *)

     function MatchKey(const AKey: string): Boolean;
     function MatchValue(const AValue: string): Boolean;

     function EqualKey(const AKey: string): Boolean;
     function EqualValue(const AValue: string): Boolean;

     function SameKey(const AKey: string): Boolean;
     function SameValue(const AValue: string): Boolean;

     function InsertByKey
       (const AKey: string): TSDVStringKeyValueTreeNode;
     function InsertByKeyValue
       (const AKey, AValue: string): TSDVStringKeyValueTreeNode;
   public
     (* Public declarations *)

     property Key: string
       read getKey write setKey;

     property Value: string
       read getValue write setValue;
   end;

 (* TSDVStringKeyValueTreeCollection *)

   TSDVStringKeyValueTreeCollection = class(TSDVTreeCollection)
   private
     (* Private declarations *)
   protected
     (* Protected declarations *)

     _AllowDuplicates: boolean;
     _IgnoreKeyCase: boolean;
     _IgnoreValueCase: boolean;
   protected
     (* Protected declarations *)

     (* accesor declarations *)

     function getAllowDuplicates(): boolean;
     function getIgnoreKeyCase(): boolean;
     function getIgnoreValueCase(): boolean;

     procedure setAllowDuplicates(const AValue: boolean);
     procedure setIgnoreKeyCase(const AValue: boolean);
     procedure setIgnoreValueCase(const AValue: boolean);
   protected
     (* Protected declarations *)

     function CreateNodeByClass(): TSDVTreeNode; override;
   public
     (* Friend Protected declarations *)

     function InternalMatchKeys(const A, B: string): Boolean;
     function InternalMatchValues(const A, B: string): Boolean;
   public
     (* Public declarations *)

     function NodeOfKey(const AKey: string): TSDVStringKeyValueTreeNode;
     function NodeOfValue(const AValue: string): TSDVStringKeyValueTreeNode;

     function KeyFound(const AKey: string): Boolean;
     function ValueFound(const AValue: string): Boolean;

     function ReplaceFirstKey(const APrevKey, ANewKey: string): Boolean;
     function ReplaceFirstValueByKey(const AKey, AValue: string): Boolean;
     function ReplaceFirstValue(const APrevValue, ANewValue: string): Boolean;
     function ReplaceAllValues(const APrevValue, ANewValue: string): Boolean;
   public
     (* Public declarations *)

     property AllowDuplicates: boolean
       read getAllowDuplicates write setAllowDuplicates;
     property IgnoreKeyCase: boolean
       read getIgnoreKeyCase write setIgnoreKeyCase;
     property IgnoreValueCase: boolean
       read getIgnoreValueCase write setIgnoreValueCase;
   end;

implementation

(* TSDVStringKeyValueTreeNode *)

function TSDVStringKeyValueTreeNode.getKey(): string;
begin
  Result := Self._Key;
end;

procedure TSDVStringKeyValueTreeNode.setKey(const AValue: string);
begin
  Self._Key := AValue;
end;

function TSDVStringKeyValueTreeNode.getValue(): string;
begin
  Result := Self._Value;
end;

procedure TSDVStringKeyValueTreeNode.setValue(const AValue: string);
begin
  Self._Value := AValue;
end;

procedure TSDVStringKeyValueTreeNode.DoCreate();
begin
  inherited DoCreate();

  Self._Key   := '';
  Self._Value := '';
end;

procedure TSDVStringKeyValueTreeNode.DoDestroy();
begin
  Self._Value := '';
  Self._Key   := '';

  inherited DoDestroy();
end;

function TSDVStringKeyValueTreeNode.MatchKey(const AKey: string): Boolean;
var ACollection: TSDVStringKeyValueTreeCollection;
begin
  ACollection := TSDVStringKeyValueTreeCollection(InternalCollection);
  Result := ACollection.InternalMatchKeys(Self._Key, AKey);
end;

function TSDVStringKeyValueTreeNode.MatchValue(const AValue: string): Boolean;
var ACollection: TSDVStringKeyValueTreeCollection;
begin
  ACollection := TSDVStringKeyValueTreeCollection(InternalCollection);
  Result := ACollection.InternalMatchKeys(Self._Value, AValue);
end;

function TSDVStringKeyValueTreeNode.EqualKey(const AKey: string): Boolean;
begin
  Result := (Self._Key = AKey);
end;

function TSDVStringKeyValueTreeNode.EqualValue(const AValue: string): Boolean;
begin
  Result := (Self._Value = AValue);
end;

function TSDVStringKeyValueTreeNode.SameKey(const AKey: string): Boolean;
begin
  Result := uktstrings.SameText(Self._Key, AKey);
end;

function TSDVStringKeyValueTreeNode.SameValue(const AValue: string): Boolean;
begin
  Result := uktstrings.SameText(Self._Value, AValue);
end;

function TSDVStringKeyValueTreeNode.InsertByKey
  (const AKey: string): TSDVStringKeyValueTreeNode;
var CanInsert: Boolean;
begin
  Result := Self.InsertByKeyValue(AKey, '');
end;

function TSDVStringKeyValueTreeNode.InsertByKeyValue
  (const AKey, AValue: string): TSDVStringKeyValueTreeNode;
var CanInsert: Boolean; ACollection: TSDVStringKeyValueTreeCollection;
begin
  Result := nil;

  CanInsert := true;
  ACollection := TSDVStringKeyValueTreeCollection(InternalCollection);
  if (not ACollection.AllowDuplicates) then
  begin
    CanInsert := (not MatchKey(AKey));
  end;

  if (CanInsert) then
  begin
    Result := (Self.Insert() as TSDVStringKeyValueTreeNode);
    Result.Key   := AKey;
    Result.Value := AValue;
  end;
end;

(* TSDVStringKeyValueTreeCollection *)

function TSDVStringKeyValueTreeCollection.getAllowDuplicates(): boolean;
begin
  Result := Self._AllowDuplicates;
end;

function TSDVStringKeyValueTreeCollection.getIgnoreKeyCase(): boolean;
begin
  Result := Self._IgnoreKeyCase;
end;

function TSDVStringKeyValueTreeCollection.getIgnoreValueCase(): boolean;
begin
  Result := Self._IgnoreValueCase;
end;

procedure TSDVStringKeyValueTreeCollection.setAllowDuplicates(const AValue: boolean);
begin
  Self._AllowDuplicates := AValue;
end;

procedure TSDVStringKeyValueTreeCollection.setIgnoreKeyCase(const AValue: boolean);
begin
  Self._IgnoreKeyCase := AValue;
end;

procedure TSDVStringKeyValueTreeCollection.setIgnoreValueCase(const AValue: boolean);
begin
  Self._IgnoreValueCase := AValue;
end;

function TSDVStringKeyValueTreeCollection.CreateNodeByClass(): TSDVTreeNode;
begin
  Result := TSDVStringKeyValueTreeNode.Create();
  Result.DoCreate();
end;

function TSDVStringKeyValueTreeCollection.InternalMatchKeys
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

function TSDVStringKeyValueTreeCollection.InternalMatchValues
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

function TSDVStringKeyValueTreeCollection.NodeOfKey
  (const AKey: string): TSDVStringKeyValueTreeNode;
begin
  Result := nil;
end;

function TSDVStringKeyValueTreeCollection.NodeOfValue
  (const AValue: string): TSDVStringKeyValueTreeNode;
begin
  Result := nil;
end;

function TSDVStringKeyValueTreeCollection.KeyFound(const AKey: string): Boolean;
begin
  Result := false;
end;

function TSDVStringKeyValueTreeCollection.ValueFound
  (const AValue: string): Boolean;
begin
  Result := false;
end;

function TSDVStringKeyValueTreeCollection.ReplaceFirstKey
  (const APrevKey, ANewKey: string): Boolean;
begin
  Result := false;
end;

function TSDVStringKeyValueTreeCollection.ReplaceFirstValueByKey
  (const AKey, AValue: string): Boolean;
begin
  Result := false;
end;

function TSDVStringKeyValueTreeCollection.ReplaceFirstValue
  (const APrevValue, ANewValue: string): Boolean;
begin
  Result := false;
end;

function TSDVStringKeyValueTreeCollection.ReplaceAllValues
  (const APrevValue, ANewValue: string): Boolean;
begin
  Result := false;
end;

end.

