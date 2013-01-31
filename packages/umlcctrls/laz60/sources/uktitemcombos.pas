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

unit uktitemcombos;

(*$R-,T-,H+,X+*)

interface
uses
(*.IFDEF MSWINDOWS*)
  Windows, Messages, Graphics,
  Controls, StdCtrls, Forms,
(*.ENDIF*)
  SysUtils, Classes,
  uktkeyconsts, uktdays, uktmonths, ukttimes,
  uktconfigs, uktlangs, uktenumstrs,
  uktcomponents,
  uktctrls, uktcomboboxes,
  dummy;

type

(* TSDVItemComboBoxObject *)

  TSDVItemComboBoxObject = class(TObject)
  protected
    (* Protected declarations *)

    FInternalObject: TObject;
  public
    (* Public declarations *)

    constructor Create;  reintroduce; virtual;
    destructor Destroy; override;

    property InternalObject: TObject
      read FInternalObject write FInternalObject;
  end;
  TSDVItemComboBoxObjectClass = class of TSDVItemComboBoxObject;

(* TCustomSDVItemCombobox *)

  TCustomSDVItemCombobox = class(TCustomSDVCombobox)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)

    function InternalObjectClass: TSDVItemComboBoxObjectClass; virtual;

    function getStringAt(Index: Integer): string;
    procedure setStringAt(Index: Integer; Value: string);
  public
    (* Public declarations *)

    {$IFDEF DELPHI}
    procedure AddItem(Item: String; AObject: TObject); override;
    {$ENDIF}
    {$IFDEF FPC}
    procedure AddItem(const Item: String; AnObject: TObject); override;
    {$ENDIF}

    property Strings[Index: Integer]: string
      read getStringAt write setStringAt;

    property AutoComplete default True;
    property AutoDropDown default False;

    {$IFDEF DELPHI}
    property BevelKind default bkNone;
    {$ENDIF}

    property ItemIndex default -1;
  end;

(* TCustomSDVObjectCombobox *)

  TCustomSDVObjectCombobox = class(TCustomSDVItemCombobox)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)

    function getObjectAt(Index: Integer): TObject;
    procedure setObjectAt(Index: Integer; Value: TObject);
  public
    (* Public declarations *)

    procedure AddObject(const ACaption: string; AObject: TObject); dynamic;

    property Objects[Index: Integer]: TObject
      read getObjectAt write setObjectAt;
  end;

(* TSDVDataComboBoxObject *)

  TSDVDataComboBoxObject = class(TSDVItemComboBoxObject)
  protected
    (* Protected declarations *)

    FData: pointer;
  public
    (* Public declarations *)

    constructor Create;  override;
    destructor Destroy; override;

    property Data: pointer
      read FData write FData;
  end;

(* TCustomSDVDataCombobox *)

  TCustomSDVDataCombobox = class(TCustomSDVItemCombobox)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)

    function getDataAt(Index: Integer): pointer;
    procedure setDataAt(Index: Integer; Value: pointer);

    function InternalObjectClass: TSDVItemComboBoxObjectClass; override;
  public
    (* Public declarations *)

    procedure AddData(const ACaption: string; AData: pointer); dynamic;

    property Data[Index: Integer]: pointer
      read getDataAt write setDataAt;
  end;

(* TSDVIntegerComboBoxObject *)

  TSDVIntegerComboBoxObject = class(TSDVDataComboBoxObject)
  protected
    (* Protected declarations *)

    FValue: Integer;
  public
    (* Public declarations *)

    constructor Create; override;
    destructor Destroy; override;

    property Value: Integer
      read FValue write FValue;
  end;

(* TCustomSDVIntegerCombobox *)

  TCustomSDVIntegerCombobox = class(TCustomSDVDataCombobox)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)

    function getValueAt(Index: Integer): Integer;
    procedure setValueAt(Index: Integer; Value: Integer);

    function InternalObjectClass: TSDVItemComboBoxObjectClass; override;
  public
    (* Public declarations *)

    procedure AddInteger(const ACaption: string; AValue: Integer); dynamic;

    property Value[Index: Integer]: Integer
      read getValueAt write setValueAt;
  end;

(* TSDVDoubleIntegerRecord *)

  TSDVDoubleIntegerRecord = record
    Hi, Lo: Integer;
  end;

(* TSDVDoubleIntegerComboBoxObject *)

  TSDVDoubleIntegerComboBoxObject = class(TSDVDataComboBoxObject)
  protected
    (* Protected declarations *)

    FValue: TSDVDoubleIntegerRecord;
  public
    (* Public declarations *)

    constructor Create; override;
    destructor Destroy; override;

    property Value: TSDVDoubleIntegerRecord
      read FValue write FValue;
  end;

(* TCustomSDVDoubleIntegerCombobox *)

  TCustomSDVDoubleIntegerCombobox = class(TCustomSDVDataCombobox)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)

    function getValueAt(Index: Integer): TSDVDoubleIntegerRecord;
    procedure setValueAt(Index: Integer; Value: TSDVDoubleIntegerRecord);

    function InternalObjectClass: TSDVItemComboBoxObjectClass; override;
  public
    (* Public declarations *)

    procedure AddDoubleInteger(const ACaption: string; AValue: TSDVDoubleIntegerRecord); dynamic;

    property Value[Index: Integer]: TSDVDoubleIntegerRecord
      read getValueAt write setValueAt;
  end;

(* TSDVObjectCombobox *)

  TSDVObjectCombobox = class(TCustomSDVObjectCombobox)
  published
    (* Public declarations *)

(*
    property AutoComplete default True;
    property AutoDropDown default False;
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property Style; {Must be published before Items}
    property Anchors;
    property BiDiMode;
    property CharCase;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownCount;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property ItemHeight;
    property ItemIndex default -1;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnCloseUp;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnSelect;
    property OnStartDock;
    property OnStartDrag;
    property Items; { Must be published after OnMeasureItem }
*)

    property AutoComplete;
    property AutoDropDown;
    {$IFDEF DELPHI}
    property BevelEdges;
    property BevelInner;
    property BevelKind;
    property BevelOuter;
    {$ENDIF}
    property Style; {Must be published before Items}
    property Anchors;
    property BiDiMode;
    property CharCase;
    property Color;
    property Constraints;
    {$IFDEF DELPHI}
    property Ctl3D;
    {$ENDIF}
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownCount;
    property Enabled;
    property Font;
    {$IFDEF DELPHI}
    property ImeMode;
    property ImeName;
    {$ENDIF}
    property ItemHeight;
    property ItemIndex;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    {$IFDEF DELPHI}
    property ParentCtl3D;
    {$ENDIF}
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnCloseUp;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnSelect;
    property OnStartDock;
    property OnStartDrag;
    property Items; { Must be published after OnMeasureItem }
  end;

(* TSDVDataCombobox *)

  TSDVDataCombobox = class(TCustomSDVDataCombobox)
  published
    (* Public declarations *)

    property AutoComplete;
    property AutoDropDown;
    {$IFDEF DELPHI}
    property BevelEdges;
    property BevelInner;
    property BevelKind;
    property BevelOuter;
    {$ENDIF}
    property Style; {Must be published before Items}
    property Anchors;
    property BiDiMode;
    property CharCase;
    property Color;
    property Constraints;
    {$IFDEF DELPHI}
    property Ctl3D;
    {$ENDIF}
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownCount;
    property Enabled;
    property Font;
    {$IFDEF DELPHI}
    property ImeMode;
    property ImeName;
    {$ENDIF}
    property ItemHeight;
    property ItemIndex;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    {$IFDEF DELPHI}
    property ParentCtl3D;
    {$ENDIF}
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnCloseUp;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnSelect;
    property OnStartDock;
    property OnStartDrag;
    property Items; { Must be published after OnMeasureItem }
  end;

(* TSDVIntegerCombobox *)

  TSDVIntegerCombobox = class(TCustomSDVIntegerCombobox)
  published
    (* Public declarations *)

    property AutoComplete;
    property AutoDropDown;
    {$IFDEF DELPHI}
    property BevelEdges;
    property BevelInner;
    property BevelKind;
    property BevelOuter;
    {$ENDIF}
    property Style; {Must be published before Items}
    property Anchors;
    property BiDiMode;
    property CharCase;
    property Color;
    property Constraints;
    {$IFDEF DELPHI}
    property Ctl3D;
    {$ENDIF}
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownCount;
    property Enabled;
    property Font;
    {$IFDEF DELPHI}
    property ImeMode;
    property ImeName;
    {$ENDIF}
    property ItemHeight;
    property ItemIndex;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    {$IFDEF DELPHI}
    property ParentCtl3D;
    {$ENDIF}
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnCloseUp;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnSelect;
    property OnStartDock;
    property OnStartDrag;
    property Items; { Must be published after OnMeasureItem }
  end;

(* TSDVDoubleIntegerCombobox *)

  TSDVDoubleIntegerCombobox = class(TCustomSDVDoubleIntegerCombobox)
  published
    (* Public declarations *)

    property AutoComplete;
    property AutoDropDown;
    {$IFDEF DELPHI}
    property BevelEdges;
    property BevelInner;
    property BevelKind;
    property BevelOuter;
    {$ENDIF}
    property Style; {Must be published before Items}
    property Anchors;
    property BiDiMode;
    property CharCase;
    property Color;
    property Constraints;
    {$IFDEF DELPHI}
    property Ctl3D;
    {$ENDIF}
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownCount;
    property Enabled;
    property Font;
    {$IFDEF DELPHI}
    property ImeMode;
    property ImeName;
    {$ENDIF}
    property ItemHeight;
    property ItemIndex;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    {$IFDEF DELPHI}
    property ParentCtl3D;
    {$ENDIF}
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnCloseUp;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnSelect;
    property OnStartDock;
    property OnStartDrag;
    property Items; { Must be published after OnMeasureItem }
  end;

implementation

(* TSDVItemComboBoxObject *)

constructor TSDVItemComboBoxObject.Create;
begin
  inherited Create;
  FInternalObject := nil;
end;

destructor TSDVItemComboBoxObject.Destroy;
begin
  FInternalObject.Free;
  inherited Destroy;
end;

(* TCustomSDVItemCombobox *)

function TCustomSDVItemCombobox.InternalObjectClass: TSDVItemComboBoxObjectClass;
begin
  Result := TSDVItemComboBoxObject;
end;

function TCustomSDVItemCombobox.getStringAt(Index: Integer): string;
begin
  Result := InternalItems.Strings[Index];
end;

procedure TCustomSDVItemCombobox.setStringAt(Index: Integer; Value: string);
begin
  InternalItems.Strings[Index] := Value;
end;

{$IFDEF DELPHI}
procedure TCustomSDVItemCombobox.AddItem
  (Item: String; AObject: TObject);
var AItemObject: TSDVItemComboBoxObject;
begin
  AItemObject := InternalObjectClass().Create;
  // create object to store object reference

  AItemObject.InternalObject := AObject;
  // redirect object reference

  //Items.AddObject(Item, AItemObject);

  inherited AddItem(Item, AItemObject);
  // add substitute object with reference to real data object
end;
{$ENDIF}
{$IFDEF FPC}
procedure TCustomSDVItemCombobox.AddItem
  (const Item: String; AnObject: TObject);
var AItemObject: TSDVItemComboBoxObject;
begin
  AItemObject := InternalObjectClass().Create;
  // create object to store object reference

  AItemObject.InternalObject := AnObject;
  // redirect object reference

  //Items.AddObject(Item, AItemObject);

  inherited AddItem(Item, AItemObject);
  // add substitute object with reference to real data object
end;
{$ENDIF}

(* TCustomSDVObjectCombobox *)

function TCustomSDVObjectCombobox.getObjectAt(Index: Integer): TObject;
var Item: TSDVItemComboBoxObject;
begin
  Item := (InternalItems.Objects[Index] as TSDVItemComboBoxObject);
  Result := Item.InternalObject;
end;

procedure TCustomSDVObjectCombobox.setObjectAt(Index: Integer; Value: TObject);
var Item: TSDVItemComboBoxObject;
begin
  Item := (InternalItems.Objects[Index] as TSDVItemComboBoxObject);
  Item.InternalObject  := Value;
end;

procedure TCustomSDVObjectCombobox.AddObject(const ACaption: string; AObject: TObject);
begin
  AddItem(ACaption, AObject);
  Application.ProcessMessages;
end;

(* TSDVDataComboBoxObject *)

constructor TSDVDataComboBoxObject.Create;
begin
  inherited Create;
  FData := nil;
end;

destructor TSDVDataComboBoxObject.Destroy;
begin
  FData := nil;
  inherited Destroy;
end;

(* TCustomSDVDataCombobox *)

function TCustomSDVDataCombobox.getDataAt(Index: Integer): pointer;
var Item: TSDVDataComboBoxObject;
begin
  Item := (InternalItems.Objects[Index] as TSDVDataComboBoxObject);
  Result := Item.Data;
end;

procedure TCustomSDVDataCombobox.setDataAt(Index: Integer; Value: pointer);
var Item: TSDVDataComboBoxObject;
begin
  Item := (InternalItems.Objects[Index] as TSDVDataComboBoxObject);
  Item.Data  := TObject(Value);
end;

function TCustomSDVDataCombobox.InternalObjectClass: TSDVItemComboBoxObjectClass;
begin
  Result := TSDVDataComboBoxObject;
end;

procedure TCustomSDVDataCombobox.AddData(const ACaption: string; AData: pointer);
var ADataObject: TSDVDataComboBoxObject;
begin
  ADataObject := (InternalObjectClass().Create as TSDVDataComboBoxObject);
  // create object to store object reference
  ADataObject.InternalObject := ADataObject;
  ADataObject.Data := AData;
  // redirect object reference

  Items.AddObject(ACaption, ADataObject);
  // add substitute object with reference to real data object
end;

(* TSDVIntegerComboBoxObject *)

constructor TSDVIntegerComboBoxObject.Create;
begin
  inherited Create;
  FValue := 0;
end;

destructor TSDVIntegerComboBoxObject.Destroy;
begin
  FValue := 0;
  inherited Destroy;
end;

(* TCustomSDVIntegerCombobox *)

function TCustomSDVIntegerCombobox.getValueAt(Index: Integer): Integer;
var Item: TSDVIntegerComboBoxObject;
begin
  Item := (InternalItems.Objects[Index] as TSDVIntegerComboBoxObject);
  Result := Item.Value;
end;

procedure TCustomSDVIntegerCombobox.setValueAt(Index: Integer; Value: Integer);
var Item: TSDVIntegerComboBoxObject;
begin
  Item := (InternalItems.Objects[Index] as TSDVIntegerComboBoxObject);
  Item.Value := Value;
end;

function TCustomSDVIntegerCombobox.InternalObjectClass: TSDVItemComboBoxObjectClass;
begin
  Result := TSDVIntegerComboBoxObject;
end;

procedure TCustomSDVIntegerCombobox.AddInteger
  (const ACaption: string; AValue: Integer);
var AObject: TSDVIntegerComboBoxObject;
begin
  AObject := (InternalObjectClass().Create as TSDVIntegerComboBoxObject);
  // create object to store object reference
  AObject.Data := nil;
  AObject.Value := AValue;
  // redirect object reference

  Items.AddObject(ACaption, AObject);
  // add substitute object with reference to real data object
end;

(* TSDVDoubleIntegerComboBoxObject *)

constructor TSDVDoubleIntegerComboBoxObject.Create;
begin
  inherited Create;
  FValue.Lo := 0;
  FValue.Hi := 0;
end;

destructor TSDVDoubleIntegerComboBoxObject.Destroy;
begin
  FValue.Hi := 0;
  FValue.Lo := 0;
  inherited Destroy;
end;

(* TCustomSDVDoubleIntegerCombobox *)

function TCustomSDVDoubleIntegerCombobox.getValueAt(Index: Integer): TSDVDoubleIntegerRecord;
var Item: TSDVDoubleIntegerComboBoxObject;
begin
  Item := (InternalItems.Objects[Index] as TSDVDoubleIntegerComboBoxObject);
  Result := Item.Value;
end;

procedure TCustomSDVDoubleIntegerCombobox.setValueAt(Index: Integer; Value: TSDVDoubleIntegerRecord);
var Item: TSDVDoubleIntegerComboBoxObject;
begin
  Item := (InternalItems.Objects[Index] as TSDVDoubleIntegerComboBoxObject);
  Item.Value := Value;
end;

function TCustomSDVDoubleIntegerCombobox.InternalObjectClass: TSDVItemComboBoxObjectClass;
begin
  Result := TSDVDoubleIntegerComboBoxObject;
end;

procedure TCustomSDVDoubleIntegerCombobox.AddDoubleInteger
  (const ACaption: string; AValue: TSDVDoubleIntegerRecord);
var AObject: TSDVDoubleIntegerComboBoxObject;
begin
  AObject := (InternalObjectClass().Create as TSDVDoubleIntegerComboBoxObject);
  // create object to store object reference
  AObject.Data := nil;
  AObject.Value := AValue;
  // redirect object reference

  Items.AddObject(ACaption, AObject);
  // add substitute object with reference to real data object
end;

end.
