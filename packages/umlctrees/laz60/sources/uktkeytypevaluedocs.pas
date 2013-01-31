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

unit uktkeytypevaluedocs;

{$mode objfpc}{$H+}

interface

uses
  // ##temp {
  Crt, uktstrings,
  // ##temp }
  Classes, SysUtils,
  uktnormobjects,
  uktbooleans,
  uktlists,
  uktreclists,
  ukttreenodes,
  ukttreecntrs,
  uktkeyvaluemodes,
  uktkeytypevaluetokens,
  uktonlystringkeytypevaluelists,
  uktkeytypevaluelisttreecntrs,
  dummy;

  //sdvXMLFileTokens, xmlfileAnsiSymbols, xmlfileAnsiScanners,
  //sdvsxmltreenodes, sdvsxmldocs,
  //sdvmcpptokens, sdvmcpptreenodes, sdvmcppdocs;

type

(* TSDVKeyTypeValueListDocument *)

  TSDVKeyTypeValueListDocument = class(TCustomSDVTreeContainer)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)
  public
    (* Public declarations *)
  end;

(* TSDVKeyTypeValueXMLDocument *)

  TSDVKeyTypeValueXMLDocument = class(TSDVKeyTypeValueListDocument)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)

    //_SXMLDoc: TSDVSimpleXMLDocument;

    KVRootNode: TSDVKeyTypeValueListTreeNode;
    KVCurrentNode: TSDVKeyTypeValueListTreeNode;

    CurrentKVType: TSDVKeyValueModes;
    CurrentKey, CurrentValue, CurrentType: string;
  protected
    (* Protected declarations *)

    function getItems(): TCustomSDVKeyTypeValueTreeCollection;
    procedure setItems(const Value: TCustomSDVKeyTypeValueTreeCollection);
  protected
    (* Protected declarations *)

    function CreateCollectionByClass(): TSDVContainerTreeCollection; override;

    procedure TransferNode
      (var Node: TSDVTreeNode;
      const Param: pointer; const Direction: TSDVTreeDirection);

    procedure DisplayNodeAsJSON
      (var Node: TSDVTreeNode;
      const Param: pointer; const Direction: TSDVTreeDirection);

    procedure DisplayNodeAsSXML
      (var Node: TSDVTreeNode;
      const Param: pointer; const Direction: TSDVTreeDirection);

    (*
    procedure TransferKVTNodeToMCPP
      (var Node: TSDVTreeNode;
      const Param: pointer; const Direction: TSDVTreeDirection);
    *)
  public
    (* Public declarations *)

    procedure TransferSXMLToKeyValueType();

    //function TransferSXMLToMCPP: TSDVMCPPDocument;

    procedure DisplayAsJSON();
    procedure DisplayAsSXML();

    //property SXMLDoc: TSDVSimpleXMLDocument
      //read _SXMLDoc write _SXMLDoc;

    property Items: TCustomSDVKeyTypeValueTreeCollection
      read getItems write setItems;
  end;

implementation

(* TSDVKeyTypeValueXMLDocument *)

function TSDVKeyTypeValueXMLDocument.getItems(): TCustomSDVKeyTypeValueTreeCollection;
begin
  Result := TCustomSDVKeyTypeValueTreeCollection(FItems);
end;

procedure TSDVKeyTypeValueXMLDocument.setItems
  (const Value: TCustomSDVKeyTypeValueTreeCollection);
begin
  FItems := Value;
end;

function TSDVKeyTypeValueXMLDocument.CreateCollectionByClass(): TSDVContainerTreeCollection;
begin
  Result := TCustomSDVKeyTypeValueTreeCollection.Create();
  Result.DoCreate();
end;

procedure TSDVKeyTypeValueXMLDocument.TransferNode
  (var Node: TSDVTreeNode;
  const Param: pointer; const Direction: TSDVTreeDirection);
(*
var EachNode: TSDVSXMLTreeNode;
    SymbolText: string;
    KTVToken: TKeyValueTypeFileToken;

    KTVNode: TSDVKeyTypeValueListTreeNode;
    KeyTypeValue: TSDVOnlyStringKeyTypeValueItem;
*)
begin
  (*
  EachNode := (Node as TSDVSXMLTreeNode);
  SymbolText := uktstrings.LowercaseCopy(EachNode.Symbol.Text);
  KTVToken := XMLTagToKeyValueType(SymbolText);

  if (Direction = tdStart) then
  begin
    if ((EachNode.IsRoot) and (KTVToken = kvtfiletkObject)) then
    begin
      // --> el marcador raiz, genera un objeto por nodo-arbol
      KTVNode := (Items.InsertRoot as TSDVKeyTypeValueListTreeNode);
      KVRootNode := KTVNode;
      KVCurrentNode := KTVNode;
    end else
    begin
      // "items", y otros tags son ignorados
      if (EachNode.Symbol.Token = xmlfiletkBlock) then
      begin
        if (KTVToken = kvtfiletkObject) then
        begin
          // --> agrega un nodo al nodo actual
          KTVNode := (Items.Insert(KVCurrentNode) as TSDVKeyTypeValueListTreeNode);
          KVCurrentNode := KTVNode;
        end else if (KTVToken = kvtfiletkKey) then
        begin
          CurrentKVType := kvtKey;
        end else if (KTVToken = kvtfiletkType) then
        begin
          CurrentKVType := kvtType;
        end else if (KTVToken = kvtfiletkValue) then
        begin
          CurrentKVType := kvtValue;
        end else
        begin
          CurrentKVType := kvtNone;
        end;
      end else if (EachNode.Symbol.Token = xmlfiletkText) then
      begin
        case (CurrentKVType) of
          kvtKey:   CurrentKey   := EachNode.Symbol.Text;
          kvtType:  CurrentType  := EachNode.Symbol.Text;
          kvtValue: CurrentValue := EachNode.Symbol.Text;
          else None();
        end;
      end else if (EachNode.Symbol.Token = xmlfiletkFinish) then
      begin
        if (KTVToken = kvtfiletkItem) then
        begin
          // al cerrar con el marcador "</item>",
          // guardar propiedad
          CurrentKVType := kvtNone;

          KeyTypeValue := TSDVOnlyStringKeyTypeValueItem.Create();
            KeyTypeValue.Key    := CurrentKey;
            KeyTypeValue.TypeID := CurrentType;
            KeyTypeValue.Value  := CurrentValue;
          KVCurrentNode.Properties.Insert(KeyTypeValue);
        end;
      end else
      begin
        //
      end;
    end;
  end else
  begin
    if (EachNode.Symbol.Token = xmlfiletkBlock) then
    begin
      if (KTVToken = kvtfiletkItem) then
      begin
        // --> al cerrar con el marcador "</item>",
        // --> guardar propiedad
        CurrentKVType := kvtNone;

        KeyTypeValue := TSDVOnlyStringKeyTypeValueItem.Create();
          KeyTypeValue.Key    := CurrentKey;
          KeyTypeValue.TypeID := CurrentType;
          KeyTypeValue.Value  := CurrentValue;
        KVCurrentNode.Properties.Insert(KeyTypeValue);
      end
      else if ((not EachNode.IsRoot) and (KTVToken = kvtfiletkObject)) then
      begin
        // --> al cerrar con el marcador "</object>",
        // --> regresar apuntador al nodo padre

        KVCurrentNode := (KVCurrentNode.Group as TSDVKeyTypeValueListTreeNode);
      end;
    end;
  end;
  *)
end;

procedure TSDVKeyTypeValueXMLDocument.TransferSXMLToKeyValueType();
begin
  (*
  if (SXMLDoc <> nil) then
  begin
    CurrentKVType := kvtNone;
    SXMLDoc.Items.ForBoth(@TransferNode, nil);
  end;
  *)
end;

(*
procedure TSDVKeyTypeValueXMLDocument.TransferKVTNodeToMCPP
  (var Node: TSDVTreeNode; const Param: pointer;
  const Direction: TSDVTreeDirection);
var Result: TSDVMCPPDocument;
    EachMCPPNode: TSDVMCPPTreeNode;
    EachKVTNode: TSDVKeyTypeValueListTreeNode;
    AIndex, ALast: Integer; Item: TSDVOnlyStringKeyTypeValueItem;
begin
  Result := TSDVMCPPDocument(Param);
  EachKVTNode := TSDVKeyTypeValueListTreeNode(Node);

  if (Direction = tdStart) then
  begin
    if (EachKVTNode.IsRoot) then
    begin
      EachMCPPNode := TSDVMCPPTreeNode(Result.Items.InsertRoot());

      ALast := (EachKVTNode.Properties().Count - 1);
      for AIndex := 0 to ALast do
      begin
        Item := EachKVTNode.Properties[AIndex];
        if (Item.MatchesKey('id')) then
        begin
          EachMCPPNode.Name := Item.Value;
        end else if (Item.MatchesKey('token')) then
        begin
          EachMCPPNode.Token:=
            sdvmcpptokens.StrToMCPPToken(Item.Value);
        end else if (Item.MatchesKey('isautomatic')) then
        begin
          EachMCPPNode.TokenExtension.IsAutomatic :=
            sdvBooleans.StrToBool(Item.Value);
        end;
      end;

      EachMCPPNode.AssignExtension();
    end else
    begin
      // ...
    end;

    // ...
  end else
  begin
    // ...
  end;

  // ...
end;
*)

(*
function TSDVKeyTypeValueXMLDocument.TransferSXMLToMCPP: TSDVMCPPDocument;
begin
  Result := TSDVMCPPDocument.Create(nil);
  self.Items.ForBoth(@TransferKVTNodeToMCPP, Result);
end;
*)

procedure TSDVKeyTypeValueXMLDocument.DisplayNodeAsJSON
  (var Node: TSDVTreeNode;
  const Param: pointer; const Direction: TSDVTreeDirection);
var EachNode: TSDVKeyTypeValueListTreeNode;
    S: string;
    ID: string; AIndex: Integer;
    I, C, L: Integer; Item: TSDVOnlyStringKeyTypeValueItem;
begin
  EachNode := TSDVKeyTypeValueListTreeNode(Node);
  S := uktstrings.StringOfChar(' ', EachNode.Level() * 2);

  if (Direction = tdStart) then
  begin
    // --> obtain identifier
    AIndex := EachNode.Properties.IndexOfKey('id');
    if (AIndex > -1) then
    begin
      Item := EachNode.Properties.Items[AIndex];
      ID := Item.Value;
    end else
    begin
      ID := '<no id>';
    end;

    WriteLn(S, ID, ' = {');

    // --> ahora si, seguir con el resto de las propiedades
    // imprimir lista de propiedades
    C := (EachNode.Properties.Count);
    if (C > 0) then
    begin
      L := (C - 1);
      for I := 0 to L do
      begin
        // obtener referencia
        Item := EachNode.Properties.Items[I];

        if (Item.Key <> 'id') then
        begin
          WriteLn(S, S, Item.Key, ': "', Item.Value, '": ', Item.TypeID, ',');
        end;
      end;
    end;

    C := (EachNode.List.Count);
    if (C > 0) then
    begin
      WriteLn(S, S, 'items = {');
    end;

  end else
  begin
    C := (EachNode.List.Count);
    if (C > 0) then
    begin
      WriteLn(S, S, '} // items');
    end;

    WriteLn(S, '} // ');
  end;
end;

procedure TSDVKeyTypeValueXMLDocument.DisplayNodeAsSXML
  (var Node: TSDVTreeNode;
  const Param: pointer; const Direction: TSDVTreeDirection);
var EachNode: TSDVKeyTypeValueListTreeNode;
    S: string;
    I, C, L: Integer; Item: TSDVOnlyStringKeyTypeValueItem;
begin
  EachNode := TSDVKeyTypeValueListTreeNode(Node);
  S := uktstrings.StringOfChar(' ', EachNode.Level() * 2);

  if (Direction = tdStart) then
  begin
    // imprimir encabezado objeto
    WriteLn(S, '<object>');

    // imprimir lista de propiedades
    C := (EachNode.Properties.Count);
    if (C > 0) then
    begin
      WriteLn(S, S, '<properties>');

      L := (C - 1);
      for I := 0 to L do
      begin
        // obtener referencia
        Item := EachNode.Properties.Items[I];
        WriteLn(S, S, S, '<item>');
        Write(S, S, S, S, '<key>', Item.Key, '</key>');
        Write(S, S, S, S, '<value>', Item.Value, '</value>');
        Write(S, S, S, S, '<type>', Item.TypeID, '</type>');
        WriteLn(S, S, S, '</item>');
      end;

      WriteLn(S, S, '</properties>');
    end;

    C := (EachNode.List.Count);
    if (C > 0) then
    begin
      WriteLn(S, S, '<items>');
    end;
  end else
  begin
    C := (EachNode.List.Count);
    if (C > 0) then
    begin
      WriteLn(S, S, '</items>');
    end;

    // imprimir piepagina objeto
    WriteLn(S, '</object>');
  end;
end;

procedure TSDVKeyTypeValueXMLDocument.DisplayAsJSON();
begin
  ClrScr();
  Items.Root().ForBoth(@DisplayNodeAsJSON, nil);
end;

procedure TSDVKeyTypeValueXMLDocument.DisplayAsSXML();
begin
  ClrScr();
  Items.Root().ForBoth(@DisplayNodeAsSXML, nil);
end;

end.

