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

unit ukttagdictionaries;

interface
uses
  SysUtils, Classes,
  dummy;

type

(* TSDVTagExecuteEvent *)

  TSDVTagDictionaryItem = class;

  TSDVTagExecuteEvent =
     procedure
  (*^*)(TagDictionaryItem: TSDVTagDictionaryItem; const Param: pointer) of object;

(* TSDVTagDictionaryItem *)

  TCustomSDVTagDictionary = class;

  TSDVTagDictionaryItem = class(TCollectionItem)
  private
    (* private declarations *)
  protected
    (* protected declarations *)

    FKeyword: string;
//    FStyle: TSDVTagStyle;
    FIsSystem: Boolean;

    FOnSingle: TSDVTagExecuteEvent;
    FOnStart:  TSDVTagExecuteEvent;
    FOnFinish: TSDVTagExecuteEvent;

    procedure DelegateOnSingle(const Param: pointer);
    procedure DelegateOnStart(const Param: pointer);
    procedure DelegateOnFinish(const Param: pointer);
  public
    (* public declarations *)

    constructor Create(ACollection: TCollection); override;

    procedure ExecuteSingle(const Param: pointer); dynamic;
    procedure ExecuteStart(const Param: pointer); dynamic;
    procedure ExecuteFinish(const Param: pointer); dynamic;
  published
    (* published declarations *)

    property IsSystem: Boolean
      read FIsSystem write FIsSystem;
    property Keyword: string
      read FKeyword write FKeyword;

    property OnSingle: TSDVTagExecuteEvent
      read FOnSingle write FOnSingle;
    property OnStart: TSDVTagExecuteEvent
      read FOnStart write FOnStart;
    property OnFinish: TSDVTagExecuteEvent
      read FOnFinish write FOnFinish;
  end;

(* TSDVTagDictionaryCollection *)

  TSDVTagDictionaryCollection = class(TCollection)
  private
    (* private declarations *)
  protected
    (* protected declarations *)
  public
    (* public declarations *)

    constructor Create(AOwner: TPersistent; AItemClass: TCollectionItemClass);
    destructor Destroy(); override;
  published
    (* published declarations *)
  end;

  TItemNotFoundEvent =
    procedure (Sender: TObject; const Keyword: string) of object;

(* TCustomSDVTagDictionary *)

  TCustomSDVTagDictionary = class(TComponent)
  private
    (* private declarations *)
  protected
    (* protected declarations *)

    FItems: TSDVTagDictionaryCollection;

    FOnItemNotFound:   TItemNotFoundEvent;

    procedure DelegateOnItemNotFound(const Keyword: string);

    procedure ItemNotFound(const Keyword: string); dynamic;
  public
    (* public declarations *)

    function ItemByKeyword(const Keyword: string): TSDVTagDictionaryItem;

    function RegisterKeyword
      (const Keyword: string; IsSystem: Boolean): TSDVTagDictionaryItem;

    procedure ExecuteSingle(const Keyword: string; const Param: pointer);
    procedure ExecuteStart(const Keyword: string; const Param: pointer);
    procedure ExecuteFinish(const Keyword: string; const Param: pointer);

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Items: TSDVTagDictionaryCollection
      read FItems write FItems;

    property OnItemNotFound: TItemNotFoundEvent
      read FOnItemNotFound write FOnItemNotFound;
  end;

(* TSDVTagDictionary *)

  TSDVTagDictionary = class(TCustomSDVTagDictionary)
  published
    (* published declarations *)

    (* TCustomSDVTagDictionary : *)

    property Items;

    property OnItemNotFound;
  end;

implementation

(* TSDVTagDictionaryItem *)

procedure TSDVTagDictionaryItem.DelegateOnSingle(const Param: pointer);
begin
  if Assigned(FOnSingle) then FOnSingle(Self, Param);
end;

procedure TSDVTagDictionaryItem.DelegateOnStart(const Param: pointer);
begin
  if Assigned(FOnStart) then FOnStart(Self, Param);
end;

procedure TSDVTagDictionaryItem.DelegateOnFinish(const Param: pointer);
begin
  if Assigned(FOnFinish) then FOnFinish(Self, Param);
end;

constructor TSDVTagDictionaryItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FKeyword := '';

  FOnSingle := nil;
  FOnStart  := nil;
  FOnFinish := nil;
end;

procedure TSDVTagDictionaryItem.ExecuteSingle(const Param: pointer);
begin
  DelegateOnSingle(Param);
end;

procedure TSDVTagDictionaryItem.ExecuteStart(const Param: pointer);
begin
  DelegateOnStart(Param);
end;

procedure TSDVTagDictionaryItem.ExecuteFinish(const Param: pointer);
begin
  DelegateOnFinish(Param);
end;

(* TSDVTagDictionaryCollection *)

constructor TSDVTagDictionaryCollection.Create(AOwner: TPersistent; AItemClass: TCollectionItemClass);
begin
  inherited Create(AItemClass);
  (*Your Code...*)
end;

destructor TSDVTagDictionaryCollection.Destroy();
begin
  (*Your Code...*)
  inherited Destroy();
end;

(* TCustomSDVTagDictionary *)

procedure TCustomSDVTagDictionary.DelegateOnItemNotFound
  (const Keyword: string);
begin
  if Assigned(FOnItemNotFound) then FOnItemNotFound(Self, Keyword);
end;

procedure TCustomSDVTagDictionary.ItemNotFound(const Keyword: string);
begin
  DelegateOnItemNotFound(Keyword);
end;

function TCustomSDVTagDictionary.
  ItemByKeyword(const Keyword: string): TSDVTagDictionaryItem;
var Found: Boolean; Index: Integer;
begin
  Index := 0; Found := FALSE; Result := nil;
  while ((Index < Items.Count) and (not Found)) do
  begin
    Result := (Items.Items[Index] as TSDVTagDictionaryItem);
    Found  := ANSISameText(Result.Keyword, Keyword);
    System.Inc(Index);
  end;
  if not Found
    then Result := nil;
end;

function TCustomSDVTagDictionary.
  RegisterKeyword(const Keyword: string; IsSystem: Boolean): TSDVTagDictionaryItem;
begin
  Result := (Items.Add as TSDVTagDictionaryItem);
  Result.Keyword := Keyword;
  Result.IsSystem := IsSystem;
end;

procedure TCustomSDVTagDictionary.ExecuteSingle
  (const Keyword: string; const Param: pointer);
var Item: TSDVTagDictionaryItem;
begin
  Item := ItemByKeyword(Keyword);
  if (Assigned(Item))
    then Item.ExecuteSingle(Param)
    else ItemNotFound(Keyword);
end;

procedure TCustomSDVTagDictionary.ExecuteStart
 (const Keyword: string; const Param: pointer);
var Item: TSDVTagDictionaryItem;
begin
  Item := ItemByKeyword(Keyword);
  if (Assigned(Item))
    then Item.ExecuteStart(Param)
    else ItemNotFound(Keyword);
end;

procedure TCustomSDVTagDictionary.ExecuteFinish
 (const Keyword: string; const Param: pointer);
var Item: TSDVTagDictionaryItem;
begin
  Item := ItemByKeyword(Keyword);
  if (Assigned(Item))
    then Item.ExecuteFinish(Param)
    else ItemNotFound(Keyword);
end;

constructor TCustomSDVTagDictionary.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItems := TSDVTagDictionaryCollection.Create(Self, TSDVTagDictionaryItem);
  (*Your Code...*)
end;

destructor TCustomSDVTagDictionary.Destroy;
begin
  (*Your Code...*)
  FItems.Free;
  inherited Destroy;
end;

end.
 
