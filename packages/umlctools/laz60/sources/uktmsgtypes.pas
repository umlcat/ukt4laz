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

unit uktmsgtypes;

interface
uses
  SysUtils, 
  Classes,
  uktguids,
  uktguidstrs,
  uktactivatedcontrols,  
  uktlists,
  uktreclists,
  dummy;

type

(* TSDVMessage *)

 TSDVMessage = TGUID;
 PSDVMessage = ^TSDVMessage;

// default messages
// remember that even that there is several string
// representations of a G.U.I.D. / U.U.I.D.,
// there should be considered always,
// as binary, maybe integer, data.

const
  msgNone =
    '{00000000-00000000-00000000-00000000}';

  msgServerAssign =
    '{E5267115-D7CD5F41-82D9E797-0751C401}';
  msgServerDeassign =
    '{04158D75-93451C43-B1CD62DD-3130F277}';

type

(* TSDVMessageParamsRecord *)

(**
 ** Used to send & recieve messages.
 **)

  TSDVMessageParamsRecord = record
    Sender:  TObject;
    Message: TSDVMessage;
    Param:   pointer;
  end;

 PSDVMessageParamsRecord = ^TSDVMessageParamsRecord;

(* TSDVMsgEventHandler *)

(**
 ** Used to delegate the message to an object pascal event (property).
 **)

  TSDVMsgEventHandler =
    procedure
      (const AMsgRec: TSDVMessageParamsRecord) of object;

(* Clients List Iterators *)

  ISDVMessageClient = interface;

  TSDVMsgClientFirstThat =
    function
      (const AItem: ISDVMessageClient;
       const AParam: Pointer): Boolean of object;
  TSDVMsgClientForEach =
    procedure
      (const AItem: ISDVMessageClient;
       const AParam: pointer) of object;

(* TSDVMsgItemRec *)

(**
 ** Interfaces cannot be directly stored in a dynamic allocated
 ** list, so a record wrapper must be used.
 **)

  TSDVMsgItemRec = record
    Item: ISDVMessageClient;
  end;
 PSDVMsgItemRec = ^TSDVMsgItemRec;

(* TSDVMsgClientList *)

  TSDVMsgClientList = class(TSDVRecordList)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)

    function getItems(AIndex: Integer): ISDVMessageClient;

    procedure setItems(AIndex: Integer; AItem: ISDVMessageClient);
  public
    (* Public declarations *)

    constructor Create(); override;

    function IndexOf(const AItem: ISDVMessageClient): Integer;
    function First(): ISDVMessageClient;
    function Last(): ISDVMessageClient;

    function Insert(const AItem: ISDVMessageClient): Integer;
    procedure InsertAt(const AIndex: Integer; const AItem: ISDVMessageClient);

    function Remove(const AItem: ISDVMessageClient): Integer;
    function Extract(const AIndex: Integer): ISDVMessageClient;

    function FirstThat
      (const Func: TSDVMsgClientFirstThat; AParam: Pointer): ISDVMessageClient;

    procedure ForEach(const Proc: TSDVMsgClientForEach; const AParam: pointer);
    procedure ForBack(const Proc: TSDVMsgClientForEach; const AParam: pointer);

    property Items[Index: Integer]: ISDVMessageClient
      read getItems write setItems; default;
  end;

(* TSDVMessageHandlerRecord *)

(**
 ** Used to relate messages to a handler procedure.
 **)

  TSDVMessageHandlerRecord = record
    Message: TSDVMessage;
    Handler: TSDVMsgEventHandler;
  end;

 PSDVMessageHandlerRecord = ^TSDVMessageHandlerRecord;

(* Handler List Iterators *)

  TSDVMessageHandlerFirstThat =
    function (const AItem:PSDVMessageHandlerRecord; const AParam: Pointer): Boolean of object;

(* TSDVMessageHandlerList *)

(**
 ** Stores a list of "TSDVMessageHandlerRecord" items,
 ** where each "Message" field should have a unique value,
 ** in the list.
 **)

  TSDVMessageHandlerList = class(TSDVRecordList)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)

    function getItems(AIndex: Integer):PSDVMessageHandlerRecord;

    procedure setItems(AIndex: Integer; AItem:PSDVMessageHandlerRecord);
  protected
    (* Protected declarations *)

    function MatchesMessage
        (const AItem:PSDVMessageHandlerRecord;
         const AParam: Pointer): Boolean;
  public
    (* Public declarations *)

    constructor Create(); override;

    function Insert
      (const AMsg: TSDVMessage; AHandler: TSDVMsgEventHandler): Integer;

    function FirstThat
      (const Func: TSDVMessageHandlerFirstThat; AParam: Pointer):PSDVMessageHandlerRecord;

    function HandlerOf(const AMsg: TSDVMessage): TSDVMsgEventHandler;
  end;

(* TSDVCurrentMsgClientList *)

  TSDVCurrentMsgClientList = class(TSDVMsgClientList)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)

    FCurrentIndex: Integer;

    function getCurrentIndex(): Integer;

    procedure setCurrentIndex(const AItem: Integer);
  public
    (* Public declarations *)

    property CurrentIndex: Integer
      read getCurrentIndex write setCurrentIndex;
  end;

(* ISDVMessageServer *)

  ISDVMessageServer = interface(IUnknown)
    (* Interface declarations *)

    (* Accessors declarations *)

    function getClients(): TSDVMsgClientList;

    procedure setClients(const AValue: TSDVMsgClientList);

    (* Public declarations *)

    procedure InsertClient(const AClient: ISDVMessageClient);
    procedure RemoveClient(const AClient: ISDVMessageClient);

    function AsComponent(): TComponent;

    procedure SendMessage
      (const AMsgRec: TSDVMessageParamsRecord);
    procedure SendMessageSingle
      (const AClient: ISDVMessageClient; const AMsgRec: TSDVMessageParamsRecord);

    (* Read-Only properties *)

    function ClientsCount(): Integer;
    function HasClients(): Boolean;

    (* Never Published declarations *)

    property Clients: TSDVMsgClientList
      read getClients write setClients;
  end;
  // Goal: Notifies client objects that server have change.
  // Objetivo: Notifica a objetos cliente que el servidor ha cambiado.

(* ISDVSingleMessageServer *)

  ISDVSingleMessageServer = interface(ISDVMessageServer)
    (* Interface declarations *)

    function CurrentClient(): ISDVMessageClient;

    procedure SendMessageCurrent(const AMsgRec: TSDVMessageParamsRecord);
  end;
  // Goal: Notifies CurrentIndex client object that server have change.
  // Objetivo: Notifica al objeto cliente actual que el servidor ha cambiado.

(* ISDVMessageClient *)

  ISDVMessageClient = interface(IUnknown)
    (* Interface declarations *)

    procedure AnswerMessage
      (const AMsgRec: TSDVMessageParamsRecord);

    function AsComponent(): TComponent;
  end;
  // Goal: Client object that answers to server*s changes.
  // Doesn't know anything about a server.

  // Objetivo: Objetos cliente que responde a cambios del servidor.
  // No sabe nada del servidor.

  (* ISDVSingleServerMessageClient *)

  ISDVSingleServerMessageClient = interface(ISDVMessageClient)
    (* Interface declarations *)

    (* Accessors declarations *)

    function getServer(): ISDVMessageServer;

    procedure setServer(const AValue: ISDVMessageServer);

    (* Public declarations *)

    property Server: ISDVMessageServer
      read getServer write setServer;
  end;
  // Goal: Client object that answers to server*s changes.
  // It intended to have a single known server.

  // Objetivo: Objetos cliente que responde a cambios del servidor.
  // Se pretende que solo haya un servidor conocido.

  (* ISDVMultipleServerMessageClient *)

  ISDVMultipleServerMessageClient = interface(ISDVMessageClient)
    (* Interface declarations *)

    // @to-do: ...
  end;
  // Goal: Client object that answers to server*s changes.
  // Has several registered servers,
  // from the client side.

  // Objetivo: Objetos cliente que responde a cambios del servidor.
  // Tiene varios servidores registrados,
  // del lado del cliente.

implementation

(* TSDVMsgClientList *)

function TSDVMsgClientList.getItems(AIndex: Integer): ISDVMessageClient;
var ARec:PSDVMsgItemRec;
begin
  ARec :=PSDVMsgItemRec(getInternalItems(AIndex));
  Result := ARec^.Item;
end;

procedure TSDVMsgClientList.setItems(AIndex: Integer; AItem: ISDVMessageClient);
var ARec:PSDVMsgItemRec;
begin
  ARec :=PSDVMsgItemRec(getInternalItems(AIndex));
  ARec^.Item := AItem;
end;

constructor TSDVMsgClientList.Create();
begin
  inherited Create();
  RecordSize := System.SizeOf(TSDVMsgItemRec)
end;

function TSDVMsgClientList.IndexOf(const AItem: ISDVMessageClient): Integer;
var CanContinue: Boolean; AMsgItemRec:PSDVMsgItemRec;
begin
  Result := IndexNotFound;

  if ((FList <> nil) and (FCount > 0)) then
  begin
    Result := 0;
    CanContinue := false;
    repeat
      CanContinue := (Result < FCount);
      if (CanContinue) then
      begin
        AMsgItemRec :=PSDVMsgItemRec(FList^[Result]);
        CanContinue := (AMsgItemRec^.Item <> AItem);
        if (CanContinue) then
        begin
          Inc(Result);
        end;
      end;
    until (not CanContinue);

    (*
    while ((Result < FCount) and (PuktMsgItemRec(FList^[Result])^.Item <> AItem)) do
    begin
      Inc(Result);
    end;
    *)

    if (Result = FCount) then
    begin
      Result := IndexNotFound;
    end;
  end;
end;

function TSDVMsgClientList.First(): ISDVMessageClient;
var Link:PSDVMsgItemRec;
begin
  Link :=PSDVMsgItemRec(InternalFirst());
  Result := Link^.Item;
end;

function TSDVMsgClientList.Last(): ISDVMessageClient;
var Link:PSDVMsgItemRec;
begin
  Link :=PSDVMsgItemRec(InternalLast());
  Result := Link^.Item;
end;

function TSDVMsgClientList.Insert(const AItem: ISDVMessageClient): Integer;
var Link:PSDVMsgItemRec;
begin
  Link   := InternalCreateItem();
  Link^.Item := AItem;
  Result := InternalInsert(Link);
end;

procedure TSDVMsgClientList.InsertAt(const AIndex: Integer; const AItem: ISDVMessageClient);
var Link:PSDVMsgItemRec;
begin
  Link := InternalCreateItem();
  Link^.Item := AItem;
  InternalInsertAt(AIndex, Link);
end;

function TSDVMsgClientList.Remove(const AItem: ISDVMessageClient): Integer;
begin
  Result := IndexOf(AItem);
  
  if (Result >= 0) then
  begin
    DeleteAt(Result);
  end;
end;

function TSDVMsgClientList.Extract(const AIndex: Integer): ISDVMessageClient;
var Link:PSDVMsgItemRec;
begin
  Result := nil;
  if (AIndex >= Count) then
  begin
//  Link := getInternalItems(Index);
    Link :=PSDVMsgItemRec(InternalLast());
    Result := Link^.Item;
    FList^[AIndex] := nil;

//  Notify(Link, lnExtracted);
    DeleteAt(AIndex);
  end;
end;

function TSDVMsgClientList.FirstThat
  (const Func: TSDVMsgClientFirstThat; AParam: Pointer): ISDVMessageClient;
var AIndex: Integer; Found: Boolean; AItem: ISDVMessageClient;
begin
  AIndex := 0; Found := FALSE; AItem := nil;
  while ((AIndex < Count) and not Found) do
  begin
    AItem  := Items[AIndex];
    Found := Func(AItem, AParam);
    Inc(AIndex);
  end;

  if (Found)
    then Result := AItem
    else Result := nil;
  // Goal: Returns the index of the item that follows the given predicate.
  // Objetivo: Regresa el indice del elemento que siga el predicado dado.
end;

procedure TSDVMsgClientList.ForEach
  (const Proc: TSDVMsgClientForEach; const AParam: pointer);
var AIndex: Integer; AItem: ISDVMessageClient;
begin
  for AIndex := 0 to Pred(Count) do
  begin
    AItem := Items[AIndex];
    Proc(AItem, AParam);
  end;
end;

procedure TSDVMsgClientList.ForBack
  (const Proc: TSDVMsgClientForEach; const AParam: pointer);
var AIndex: Integer; AItem: ISDVMessageClient;
begin
  for AIndex := Pred(Count) downto 0 do
  begin
    AItem := Items[AIndex];
    Proc(AItem, AParam);
  end;
end;

(* TSDVMessageHandlerList *)

function TSDVMessageHandlerList.getItems
  (AIndex: Integer):PSDVMessageHandlerRecord;
begin
  Result :=PSDVMessageHandlerRecord(getInternalItems(AIndex));
end;

procedure TSDVMessageHandlerList.setItems
  (AIndex: Integer; AItem:PSDVMessageHandlerRecord);
var ARec: pointer;
begin
  ARec := AItem;
  setInternalItems(AIndex, ARec)
end;

function TSDVMessageHandlerList.MatchesMessage
  (const AItem:PSDVMessageHandlerRecord; const AParam: Pointer): Boolean;
var AMessage:PSDVMessage;
begin
  Result := false;
  AMessage :=PSDVMessage(AParam);
  Result := uktguids.EqualGUID(AItem^.Message, AMessage^);
end;

constructor TSDVMessageHandlerList.Create();
begin
  RecordSize := System.SizeOf(TSDVMessageHandlerRecord)
end;

function TSDVMessageHandlerList.Insert
  (const AMsg: TSDVMessage; AHandler: TSDVMsgEventHandler): Integer;
var ARec:PSDVMessageHandlerRecord;
begin
  ARec   := InternalCreateItem();
  ARec^.Message := AMsg;
  ARec^.Handler := AHandler;
  Result := InternalInsert(ARec);
end;

function TSDVMessageHandlerList.FirstThat
  (const Func: TSDVMessageHandlerFirstThat; AParam: Pointer):PSDVMessageHandlerRecord;
var AIndex: Integer; Found: Boolean; AItem:PSDVMessageHandlerRecord;
begin
  AIndex := 0; Found := FALSE; AItem := nil;
  while ((AIndex < Count) and not Found) do
  begin
    AItem  := Items[AIndex];
    Found := Func(AItem, AParam);
    Inc(AIndex);
  end;

  if (Found)
    then Result := AItem
    else Result := nil;
  // Goal: Returns the index of the item that follows the given predicate.
  // Objetivo: Regresa el indice del elemento que siga el predicado dado.
end;

function TSDVMessageHandlerList.HandlerOf
  (const AMsg: TSDVMessage): TSDVMsgEventHandler;
var ARec:PSDVMessageHandlerRecord;
begin
  Result := nil;

  ARec := FirstThat(@MatchesMessage, @AMsg);
  if (ARec <> nil) then
  begin
    Result := ARec^.Handler;
  end;
end;


(* TSDVCurrentMsgClientList *)

function TSDVCurrentMsgClientList.getCurrentIndex(): Integer;
begin
  Result := FCurrentIndex;
end;

procedure TSDVCurrentMsgClientList.setCurrentIndex(const AItem: Integer);
begin
  FCurrentIndex := AItem;
end;

end.
