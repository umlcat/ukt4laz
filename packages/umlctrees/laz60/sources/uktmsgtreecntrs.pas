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

unit uktmsgtreecntrs;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  uktguids,
  uktguidstrs,
  uktlists,
  uktmsgtypes,
  ukttreenodes,
  ukttreecntrs,
  dummy;

(**
 ** Description:
 ** This unit contains several classes.
 **
 ** The main class is "TCustomSDVMsgTreeContainer",
 ** a non-visual component that contains a hierarchical,
 ** tree structure.
 **
 ** Additionaly, it supports the Subject part,
 ** of the Subject-Observer Pattern,
 ** and its observers, maybe visual controls, or not.
 **)

 // default messages

 const
   msgTreeNodeBeforeInsert
     = '{8A1BB14E-F285D14A-A8272FEB-631A0181}';
   msgTreeNodeAfterInsert
     = '{324D2E02-79A3024D-B8975C52-6010CCA0}';

   msgTreeNodeBeforeRemove
     = '{72BE8F90-66EE5A46-87C345F4-A121FD05}';
   msgTreeNodeAfterRemove
     = '{3F9715ED-32BFB648-8E0A2197-7F10A164}';

   msgTreeNodeBeforeChangeText
     = '{663499E3-BF0AAB45-90AB21AE-FBFCD728}';
   msgTreeNodeAfterChangeText
     = '{690F1DDE-A82C1E49-A548E4AF-08FEB6B4}';

   msgTreeNodeBeforeChangeSelected
     = '{7EF47716-E5129340-8FB19308-8577B33A}';
   msgTreeNodeAfterChangeSelected
     = '{85E39117-921E3148-BC5CBDB0-341164A4}';

type

(* TSDVMsgContainerTreeNode *)

  TSDVMsgContainerTreeNode = class(TSDVContainerTreeNode)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)

    procedure SendMessage
      (const AMsgRec: TSDVMessageParamsRecord);
  public
    (* Public declarations *)

    procedure DoCreate(); override;
    procedure DoDestroy(); override;
  end;

(* TSDVMsgContainerTreeCollection *)

  TSDVMsgContainerTreeCollection = class(TSDVContainerTreeCollection)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)

    function CreateNodeByClass(): TSDVTreeNode; override;

    procedure SendMessage
      (const AMsgRec: TSDVMessageParamsRecord);
  public
    (* Public declarations *)

    procedure DoCreate(); override;
    procedure DoDestroy(); override;
  end;

 (* TCustomSDVMsgTreeContainer *)

   TCustomSDVMsgTreeContainer =
     class(TCustomSDVTreeContainer, ISDVMessageServer)
   private
     (* Private declarations *)
   protected
     (* Protected declarations *)

     FClients: TSDVMsgClientList;
   protected
     (* Protected declarations *)

     (* accesors declarations *)

     function getClients(): TSDVMsgClientList;

     procedure setClients(const AValue: TSDVMsgClientList);
   protected
     (* Protected declarations *)

     function CreateCollectionByClass(): TSDVContainerTreeCollection; override;

     function CreateClients(): TSDVMsgClientList; virtual;
   public
     (* Friend Protected declarations *)

     procedure NotifyBeforeChangeText
       (const ANode: TSDVContainerTreeNode;
        const AText: string); override;
     procedure NotifyAfterChangeText
       (const ANode: TSDVContainerTreeNode); override;

     procedure NotifyBeforeChangeSelected
       (const ANode: TSDVContainerTreeNode;
        const ASelected: Boolean); override;
     procedure NotifyAfterChangeSelected
       (const ANode: TSDVContainerTreeNode); override;

     procedure NotifyBeforeInsert
       (const AParentNode, ANode: TSDVContainerTreeNode;
        const AIndex: Integer); override;
     procedure NotifyAfterInsert
       (const AParentNode, ANode: TSDVContainerTreeNode;
        const AIndex: Integer); override;

     procedure NotifyBeforeRemove
       (const ANode: TSDVContainerTreeNode); override;
     procedure NotifyAfterRemove
       (const ANode: TSDVContainerTreeNode); override;

     procedure NotifyBeforeEmpty
       (const ANode: TSDVContainerTreeNode); override;
     procedure NotifyAfterEmpty
       (const ANode: TSDVContainerTreeNode); override;
   public
     (* Public declarations *)

     constructor Create(AOwner: TComponent); override;
     destructor Destroy(); override;
   public
     (* Public declarations *)

     procedure InsertClient(const AClient: ISDVMessageClient);
     procedure RemoveClient(const AClient: ISDVMessageClient);

     function AsComponent(): TComponent;

     procedure SendMessage
       (const AMsgRec: TSDVMessageParamsRecord);
     procedure SendMessageSingle
       (const AClient: ISDVMessageClient;
        const AMsgRec: TSDVMessageParamsRecord);
   public
     (* Public declarations *)

     (* Read-Only properties *)

     function ClientsCount(): Integer;
     function HasClients(): Boolean;

     (* Never Published declarations *)

     property Clients: TSDVMsgClientList
       read getClients write setClients;
   end;

 (* TSDVMsgTreeContainer *)

   TSDVMsgTreeContainer = class(TCustomSDVMsgTreeContainer)
   published
     (* Published declarations *)

     (* TCustomSDVTreeContainer: *)

     (* TCustomSDVMsgTreeContainer: *)
   end;

implementation

(* TSDVMsgContainerTreeNode *)

procedure TSDVMsgContainerTreeNode.SendMessage
  (const AMsgRec: TSDVMessageParamsRecord);
var ACollection: TSDVMsgContainerTreeCollection;
begin
  ACollection := TSDVMsgContainerTreeCollection(Self.InternalCollection);
  ACollection.SendMessage(AMsgRec);
end;

procedure TSDVMsgContainerTreeNode.DoCreate();
begin
  inherited DoCreate();
end;

procedure TSDVMsgContainerTreeNode.DoDestroy();
begin
  inherited DoDestroy();
end;

(* TSDVMsgContainerTreeCollection *)

function TSDVMsgContainerTreeCollection.CreateNodeByClass(): TSDVTreeNode;
begin
  Result := TSDVMsgContainerTreeNode.Create();
  Result.DoCreate();
end;

procedure TSDVMsgContainerTreeCollection.SendMessage
  (const AMsgRec: TSDVMessageParamsRecord);
var AMsgContainer: TCustomSDVMsgTreeContainer;
begin
  AMsgContainer := TCustomSDVMsgTreeContainer(Self.Container);
  AMsgContainer.SendMessage(AMsgRec);
end;

procedure TSDVMsgContainerTreeCollection.DoCreate();
begin
  inherited DoCreate();
end;

procedure TSDVMsgContainerTreeCollection.DoDestroy();
begin
  inherited DoDestroy();
end;

(* TCustomSDVMsgTreeContainer *)

function TCustomSDVMsgTreeContainer.getClients(): TSDVMsgClientList;
begin
  Result := FClients
  // Goal: "Clients" property get method .
  // Objetivo: Metodo lectura para propiedad "Clients".
end;

procedure TCustomSDVMsgTreeContainer.setClients(const AValue: TSDVMsgClientList);
begin
  FClients := AValue;
  // Goal: "Clients" property set method .
  // Objetivo: Metodo escritura para propiedad "Clients".
end;

function TCustomSDVMsgTreeContainer.CreateCollectionByClass(): TSDVContainerTreeCollection;
begin
  Result := TSDVMsgContainerTreeCollection.Create();
  Result.DoCreate();
  // Goal: Create inheretable (polimorphic) collection.
  // Objetivo: Crear coleccion heredable (polimorfica).
end;

procedure TCustomSDVMsgTreeContainer.NotifyBeforeChangeText
  (const ANode: TSDVContainerTreeNode; const AText: string);
var AMsgRec: TSDVMessageParamsRecord;
begin
  uktguidstrs.DoubleStrToGUID
    (msgTreeNodeBeforeChangeText, AMsgRec.Message);

  AMsgRec.Sender  := ANode;
  AMsgRec.Param   := @AText;
  SendMessage(AMsgRec);
end;

procedure TCustomSDVMsgTreeContainer.NotifyAfterChangeText
  (const ANode: TSDVContainerTreeNode);
var AMsgRec: TSDVMessageParamsRecord; AText: string;
begin
  uktguidstrs.DoubleStrToGUID
    (msgTreeNodeAfterChangeText, AMsgRec.Message);

  AText := ANode.Text;
  AMsgRec.Sender  := ANode;
  AMsgRec.Param   := @AText;
  SendMessage(AMsgRec);
end;

procedure TCustomSDVMsgTreeContainer.NotifyBeforeChangeSelected
  (const ANode: TSDVContainerTreeNode; const ASelected: Boolean);
var AMsgRec: TSDVMessageParamsRecord;
begin
  uktguidstrs.DoubleStrToGUID
    (msgTreeNodeBeforeChangeSelected, AMsgRec.Message);

  AMsgRec.Sender  := ANode;
  AMsgRec.Param   := @ASelected;
  SendMessage(AMsgRec);
end;

procedure TCustomSDVMsgTreeContainer.NotifyAfterChangeSelected
  (const ANode: TSDVContainerTreeNode);
var AMsgRec: TSDVMessageParamsRecord; ASelected: Boolean;
begin
  uktguidstrs.DoubleStrToGUID
    (msgTreeNodeAfterChangeSelected, AMsgRec.Message);

  ASelected := ANode.Selected;
  AMsgRec.Sender  := ANode;
  AMsgRec.Param   := @ASelected;
  SendMessage(AMsgRec);
end;

procedure TCustomSDVMsgTreeContainer.NotifyBeforeInsert
  (const AParentNode, ANode: TSDVContainerTreeNode; const AIndex: Integer);
var AMsgRec: TSDVMessageParamsRecord;
begin
  // notify before insertion to clients
  uktguidstrs.DoubleStrToGUID
    (msgTreeNodeBeforeInsert, AMsgRec.Message);

  AMsgRec.Sender  := AParentNode;
  AMsgRec.Param   := ANode;
  SendMessage(AMsgRec);
end;

procedure TCustomSDVMsgTreeContainer.NotifyAfterInsert
  (const AParentNode, ANode: TSDVContainerTreeNode; const AIndex: Integer);
var AMsgRec: TSDVMessageParamsRecord;
begin
  // notify after insertion to clients
  uktguidstrs.DoubleStrToGUID
    (msgTreeNodeAfterInsert, AMsgRec.Message);

  AMsgRec.Sender  := AParentNode;
  AMsgRec.Param   := ANode;
  SendMessage(AMsgRec);
end;

procedure TCustomSDVMsgTreeContainer.NotifyBeforeRemove
  (const ANode: TSDVContainerTreeNode);
var AMsgRec: TSDVMessageParamsRecord;
begin
  // notify before remotion to clients
  uktguidstrs.DoubleStrToGUID
    (msgTreeNodeBeforeRemove, AMsgRec.Message);

  AMsgRec.Sender  := ANode;
  AMsgRec.Param   := ANode;
  SendMessage(AMsgRec);
end;

procedure TCustomSDVMsgTreeContainer.NotifyAfterRemove
  (const ANode: TSDVContainerTreeNode);
var AMsgRec: TSDVMessageParamsRecord;
begin
  // notify before remotion to clients
  uktguidstrs.DoubleStrToGUID
    (msgTreeNodeAfterRemove, AMsgRec.Message);

  AMsgRec.Sender  := ANode;
  AMsgRec.Param   := ANode;
  SendMessage(AMsgRec);
end;

procedure TCustomSDVMsgTreeContainer.NotifyBeforeEmpty
  (const ANode: TSDVContainerTreeNode);
//var AMsgRec: TSDVMessageParamsRecord;
begin
  inherited NotifyBeforeEmpty(ANode);
  (*
  uktguidstrs.DoubleStrToGUID
    (msgTreeNodeBeforeEmpty, AMsgRec.Message);

  AMsgRec.Sender  := ANode;
  AMsgRec.Param   := ANode;
  SendMessage(AMsgRec);
  *)
end;

procedure TCustomSDVMsgTreeContainer.NotifyAfterEmpty
  (const ANode: TSDVContainerTreeNode);
//var AMsgRec: TSDVMessageParamsRecord;
begin
  inherited NotifyAfterEmpty(ANode);
  (*
  uktguidstrs.DoubleStrToGUID
    (msgTreeNodeAfterEmpty, AMsgRec.Message);

  AMsgRec.Sender  := ANode;
  AMsgRec.Param   := ANode;
  SendMessage(AMsgRec);
  *)
end;

function TCustomSDVMsgTreeContainer.CreateClients(): TSDVMsgClientList;
begin
  Result := TSDVMsgClientList.Create();
  Result.RecordSize := System.SizeOf(TSDVMsgItemRec);
end;

constructor TCustomSDVMsgTreeContainer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FClients := CreateClients();
end;

destructor TCustomSDVMsgTreeContainer.Destroy;
begin
  FClients.Free();
  inherited Destroy();
end;

procedure TCustomSDVMsgTreeContainer.InsertClient
  (const AClient: ISDVMessageClient);
var AMsgRec: TSDVMessageParamsRecord;
begin
  if (Clients.IndexOf(AClient) = IndexNotFound) then
  begin
    // perform subscription
    Clients.Insert(AClient);

    // confirm subscription to client
    uktguidstrs.DoubleStrToGUID
      (msgServerAssign, AMsgRec.Message);

    AMsgRec.Sender  := Self;
    AMsgRec.Param   := AClient;
    SendMessageSingle(AClient, AMsgRec);
  end;
end;

procedure TCustomSDVMsgTreeContainer.RemoveClient
  (const AClient: ISDVMessageClient);
var AMsgRec: TSDVMessageParamsRecord;
begin
  if (Clients.IndexOf(AClient) = IndexNotFound) then
  begin
    // confirm cancelation of subscription to client
    uktguidstrs.DoubleStrToGUID
      (msgServerDeassign, AMsgRec.Message);

    AMsgRec.Sender  := Self;
    AMsgRec.Param   := AClient;
    SendMessageSingle(AClient, AMsgRec);

    // perform cancelation of subscription
    Clients.Remove(AClient);
  end;
end;

function TCustomSDVMsgTreeContainer.AsComponent: TComponent;
begin
  Result := Self;
end;

procedure TCustomSDVMsgTreeContainer.SendMessage
  (const AMsgRec: TSDVMessageParamsRecord);
var I, ALast: Integer; EachClient: ISDVMessageClient;
begin
  if (Clients <> nil) then
  begin
    ALast := (Clients.Count - 1);
    if (HasClients()) then
    begin
      for I := 0 to ALast do
      begin
        EachClient := Clients[i];
        EachClient.AnswerMessage(AMsgRec);
      end;
    end;
  end;
  // Goal: Notifies clients that server has changed.
  // Objetivo: Notificar a los clientes que el servidor ha cambiado.
end;

procedure TCustomSDVMsgTreeContainer.SendMessageSingle
  (const AClient: ISDVMessageClient; const AMsgRec: TSDVMessageParamsRecord);
begin
  if (AClient <> nil) then
  begin
    AClient.AnswerMessage(AMsgRec);
  end;
end;

function TCustomSDVMsgTreeContainer.ClientsCount(): Integer;
begin
  Result := Clients.Count;
end;

function TCustomSDVMsgTreeContainer.HasClients(): Boolean;
begin
  Result := (Clients.Count > 0);
end;

end.

