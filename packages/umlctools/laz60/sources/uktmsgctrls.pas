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

unit uktmsgctrls;

interface
uses
  SysUtils, 
  Classes,
  uktlists,
  //uktreclists,
  uktactivatedcontrols,
  uktguidstrs,
  uktmsgtypes,
  dummy;

type

(* TSDVMsgServer *)

  TSDVMsgServer = class(TSDVActivatedComponent, ISDVMessageServer)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)

    FClients: TSDVMsgClientList;
  protected
    (* Protected declarations *)

    function getClients(): TSDVMsgClientList;

    procedure setClients(const AValue: TSDVMsgClientList);
  protected
    (* Protected declarations *)

    function CreateClients(): TSDVMsgClientList; virtual;
  public
    (* Public declarations *)

    procedure InsertClient(const AClient: ISDVMessageClient);
    procedure RemoveClient(const AClient: ISDVMessageClient);

    function AsComponent(): TComponent;

    procedure SendMessage
      (const AMsgRec: TSDVMessageParamsRecord);
    procedure SendMessageSingle
      (const AClient: ISDVMessageClient; const AMsgRec: TSDVMessageParamsRecord);
  public
    (* Public declarations *)

    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
  public
    (* Public declarations *)

    (* Read-Only properties *)

    function ClientsCount(): Integer;
    function HasClients(): Boolean;

    (* Never Published declarations *)

    property Clients: TSDVMsgClientList
      read getClients write setClients;
  end;

(* TSDVSingleClientMsgServer *)

  TSDVSingleClientMsgServer = class(TSDVMsgServer, ISDVSingleMessageServer)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)

    function CurrentClient(): ISDVMessageClient;
    function CreateClients(): TSDVMsgClientList; override;
  public
    (* Public declarations *)

    procedure SendMessageCurrent(const AMsgRec: TSDVMessageParamsRecord);
  end;

(* TSDVMsgClient *)

  TSDVMsgClient = class(TSDVActivatedComponent, ISDVMessageClient)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)

    FServer: TSDVMsgServer;
    FOnAnswerMessage: TSDVMsgEventHandler;

    function getServer(): TSDVMsgServer; virtual;

    procedure setServer(const AValue: TSDVMsgServer); virtual;
  protected
    (* Protected declarations *)

    procedure DelegateOnAnswerMessage(const AMsgRec: TSDVMessageParamsRecord);

    procedure Notification
      (AComponent: TComponent; Operation: TOperation); override;
  public
    (* Public declarations *)

    constructor Create(AOwner: TComponent); override;
  public
    (* Public declarations *)

    procedure AnswerMessage
     (const AMsgRec: TSDVMessageParamsRecord); virtual;

    function AsComponent(): TComponent;
  published
    (* Published declarations *)

    property Server: TSDVMsgServer
      read getServer write setServer;
    property OnAnswerMessage: TSDVMsgEventHandler
      read FOnAnswerMessage write FOnAnswerMessage;
  end;

(* TSDVSingleServerMsgClient *)

  TSDVSingleServerMsgClient =
    class(
    TSDVActivatedComponent,
    ISDVMessageClient,
    ISDVSingleServerMessageClient)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)

    FServer: ISDVMessageServer;
    FOnAnswerMessage: TSDVMsgEventHandler;

    function getServer(): ISDVMessageServer;

    procedure setServer(const AValue: ISDVMessageServer);
  protected
    (* Protected declarations *)

    procedure DelegateOnAnswerMessage(const AMsgRec: TSDVMessageParamsRecord);

    procedure Notification
      (AComponent: TComponent; Operation: TOperation); override;
  public
    (* Public declarations *)

    procedure AnswerMessage
     (const AMsgRec: TSDVMessageParamsRecord); virtual;

    function AsComponent(): TComponent;

    property Server: ISDVMessageServer
      read getServer write setServer;
  end;

implementation

(* TSDVMsgServer *)

function TSDVMsgServer.getClients(): TSDVMsgClientList;
begin
  Result := FClients
  // Goal: "Clients" property get method .
  // Objetivo: Metodo lectura para propiedad "Clients".
end;

procedure TSDVMsgServer.setClients(const AValue: TSDVMsgClientList);
begin
  FClients := AValue;
  // Goal: "Clients" property set method .
  // Objetivo: Metodo escritura para propiedad "Clients".
end;

function TSDVMsgServer.CreateClients(): TSDVMsgClientList;
begin
  Result := TSDVMsgClientList.Create();
  Result.RecordSize := System.SizeOf(TSDVMsgItemRec);
end;

procedure TSDVMsgServer.InsertClient(const AClient: ISDVMessageClient);
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

procedure TSDVMsgServer.RemoveClient(const AClient: ISDVMessageClient);
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

function TSDVMsgServer.AsComponent(): TComponent;
begin
  Result := Self;
end;

procedure TSDVMsgServer.SendMessage
  (const AMsgRec: TSDVMessageParamsRecord);
var I, ALast: Integer; Client: ISDVMessageClient;
begin
  if (Clients <> nil) then
  begin
    ALast := (Clients.Count - 1);
    if (HasClients()) then
    begin
      for I := 0 to ALast do
      begin
        Client := Clients[i];
        Client.AnswerMessage(AMsgRec);
      end;
    end;
  end;
  // Goal: Notifies clients that server has changed.
  // Objetivo: Notificar a los clientes que el servidor ha cambiado.
end;

procedure TSDVMsgServer.SendMessageSingle
  (const AClient: ISDVMessageClient; const AMsgRec: TSDVMessageParamsRecord);
begin
  if (AClient <> nil) then
  begin
    AClient.AnswerMessage(AMsgRec);
  end;
end;

constructor TSDVMsgServer.Create(AOwner: TComponent);
begin
  //inherited Create(AOwner);
  FClients := CreateClients();
  // Goal: To prepare the navigator.
  // Objetivo: Preparar el navegador.
end;

destructor TSDVMsgServer.Destroy();
begin
  FClients.Empty();
  FClients.Free();
  //inherited Destroy();
  // Goal: To unprepare the navigator.
  // Objetivo: Despreparar el navegador.
end;

function TSDVMsgServer.ClientsCount(): Integer;
begin
  Result := Clients.Count;
end;

function TSDVMsgServer.HasClients(): Boolean;
begin
  Result := (Clients.Count > 0);
end;

(* TSDVSingleClientMsgServer *)

function TSDVSingleClientMsgServer.CurrentClient(): ISDVMessageClient;
var Index: Integer;
begin
  Index  := (Clients as TSDVCurrentMsgClientList).CurrentIndex;
  Result := Clients.Items[Index];
end;

procedure TSDVSingleClientMsgServer.SendMessageCurrent
  (const AMsgRec: TSDVMessageParamsRecord);
var Client: ISDVMessageClient;
begin
  Client := CurrentClient();
  if (Client <> nil) then
  begin
    Client.AnswerMessage(AMsgRec);
  end;
end;

function TSDVSingleClientMsgServer.CreateClients(): TSDVMsgClientList;
begin
  Result := TSDVCurrentMsgClientList.Create();
end;

(* TSDVMsgClient *)

procedure TSDVMsgClient.DelegateOnAnswerMessage
 (const AMsgRec: TSDVMessageParamsRecord);
begin
  if (FOnAnswerMessage <> nil) then
  begin
    FOnAnswerMessage(AMsgRec);
  end;
end;

procedure TSDVMsgClient.AnswerMessage
  (const AMsgRec: TSDVMessageParamsRecord);
begin
  DelegateOnAnswerMessage(AMsgRec);
end;

function TSDVMsgClient.AsComponent(): TComponent;
begin
  Result := Self;
end;

function TSDVMsgClient.getServer(): TSDVMsgServer;
begin
  Result := FServer;
  // Goal: "Server" property set method.
  // Objetivo: Metodo lectura propiedad "Server".
end;

procedure TSDVMsgClient.setServer(const AValue: TSDVMsgServer);
begin
  if (FServer <> AValue) then
  begin
    if (FServer <> nil) then
    begin
      FServer.RemoveClient(Self);
    end;
    // erase client from previous server
    // borrar cliente de servidor anterior

    FServer := AValue;
    // update reference
    // actualizar referencia

    if (FServer <> nil) then
    begin
      FServer.InsertClient(Self);
    end;
    // insert client in new server
    // insertar cliente en servidor nuevo
  end;
  // Goal: "Server" property set method.
  // Objetivo: Metodo escritura propiedad "Server".
end;

procedure TSDVMsgClient.Notification
  (AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if ((Operation = opRemove) and (AComponent = FServer)) then
  begin
    if (FServer <> nil) then
    begin
      FServer := nil;
    end;
  end;
  // Goal: To detect & update when associated components,
  // have been removed.

  // Objetivo: Detectar y actualizar cuando,
  // los componentes asociados se han removido.
end;

constructor TSDVMsgClient.Create(AOwner: TComponent);
begin
  inherited;
  FServer := nil;
end;

(* TSDVSingleServerMsgClient *)

function TSDVSingleServerMsgClient.getServer(): ISDVMessageServer;
begin
  Result := FServer;
  // Goal: "Server" property set method.
  // Objetivo: Metodo lectura propiedad "Server".
end;

procedure TSDVSingleServerMsgClient.setServer(const AValue: ISDVMessageServer);
var AClient: ISDVMessageClient;
begin
  if (FServer <> AValue) then
  begin
    if (FServer <> nil) then
    begin
      AClient := ISDVMessageClient(Self);
      FServer.RemoveClient(AClient);
    end;
    // erase client from previous server
    // borrar cliente de servidor anterior

    FServer := AValue;
    // update reference
    // actualizar referencia

    if (FServer <> nil) then
    begin
      FServer.InsertClient(Self);
    end;
    // insert client in new server
    // insertar cliente en servidor nuevo
  end;
  // Goal: "Server" property set method.
  // Objetivo: Metodo escritura propiedad "Server".
end;

procedure TSDVSingleServerMsgClient.DelegateOnAnswerMessage
 (const AMsgRec: TSDVMessageParamsRecord);
begin
  if (FOnAnswerMessage <> nil) then
  begin
    FOnAnswerMessage(AMsgRec);
  end;
end;

procedure TSDVSingleServerMsgClient.Notification
  (AComponent: TComponent; Operation: TOperation);
var FComponent: TComponent;
begin
  inherited Notification(AComponent, Operation);

  if (FServer <> nil) then
  begin
    FComponent := FServer.AsComponent();
    if ((Operation = opRemove) and (AComponent = FComponent)) then
    begin
      FServer := nil;
    end;
  end;
  // Goal: To detect & update when associated components,
  // have been removed.

  // Objetivo: Detectar y actualizar cuando,
  // los componentes asociados se han removido.
end;

procedure TSDVSingleServerMsgClient.AnswerMessage
  (const AMsgRec: TSDVMessageParamsRecord);
begin
  DelegateOnAnswerMessage(AMsgRec);
end;

function TSDVSingleServerMsgClient.AsComponent(): TComponent;
begin
  Result := Self;
end;



end.
