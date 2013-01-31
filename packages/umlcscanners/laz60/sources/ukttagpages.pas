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

unit ukttagpages;

interface

uses
  //Windows,
  SysUtils, Classes,
  ukttreenodes, ukttreecntrs,
  ukttagprops, ukttagstyles,
  dummy;

type

(* TSDVTagItem *)

  TSDVTagItem = class(TSDVContainerTreeNode)
  private
    (* private declarations *)
  protected
    (* protected declarations *)

    FKeyword: string;
    FStyle:   TSDVTagStyle;

    FValue:   string;
    // valor unico almacenado en etiqueta como bloques de texto
    // single value stored in tag such text blocks

    FUsesBreak: Boolean;
    // generar salto de linea despues de marcadores inicio o final de bloque ?
    // generate a line break after block*s start or finish markers ?

    FProperties: TSDVTagProperties;
    // varios valores almacenados en propiedades
    // '<body forecolor = "%CCAA00" backcolor = "%AF07B3"> '

    // several values stored in properties such
    // '<body forecolor = "%CCAA00" backcolor = "%AF07B3"> '
  public
    (* public declarations *)

    procedure DoCreate(); override;
    procedure DoDestroy(); override;

    function ToStartText(): string; dynamic;
    function ToFinishText(): string; dynamic;
    function ToText(): string; dynamic;

    function PropByKeyword(const Keyword: string): TSDVTagProperty;
    function RegisterProperty(const Keyword: string): TSDVTagProperty;
    function RegisterPropertyValue
      (const Keyword, Value: string): TSDVTagProperty;

    procedure CopyPropTo(const Dest: TSDVTagItem);
  published
    (* published declarations *)

    property UsesBreak: Boolean
      read FUsesBreak write FUsesBreak;
    property Keyword: string
      read FKeyword write FKeyword;
    property Value: string
      read FValue write FValue;
    property Style: TSDVTagStyle
      read FStyle write FStyle;
    property Properties: TSDVTagProperties
      read FProperties write FProperties;
  end;

(* TSDVTagCollection *)

  TSDVTagCollection = class(TSDVContainerTreeCollection)
  private
    (* private declarations *)
  protected
    (* protected declarations *)

    function CreateNodeByClass(): TSDVTreeNode; override;
  public
    (* public declarations *)

    procedure DoCreate(); override;
    procedure DoDestroy(); override;
  published
    (* published declarations *)
  end;

(* TCustomSDVTagPage *)

  TCustomSDVTagPage = class(TCustomSDVTreeContainer)
  private
    (* private declarations *)
  protected
    (* protected declarations *)

    function CreateCollectionByClass(): TSDVContainerTreeCollection; override;
  public
    (* public declarations *)

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

(* TSDVTagPage *)

  TSDVTagPage = class(TCustomSDVTagPage)
  published
    (* published declarations *)

    { TCustomSDVTreeContainer: }

    (*
    property OnInsert;
    property OnRemove;
    property OnUpdateName;
    property OnUpdateText;
    *)
  end;

implementation

(* TSDVTagItem *)

procedure TSDVTagItem.DoCreate();
begin
  inherited DoCreate();

  FKeyword := '';
  FUsesBreak := FALSE;
  FStyle := tsNone;

  FValue   := '';
  FProperties := TSDVTagProperties.Create(TSDVTagProperty);
end;

procedure TSDVTagItem.DoDestroy();
begin
  FProperties.Free;
  FValue   := '';

  FStyle := tsNone;
  FKeyword := '';

  inherited DoDestroy();
end;

function TSDVTagItem.ToStartText(): string;
begin
  Result := FKeyword;
  // to-do: include properties
end;

function TSDVTagItem.ToFinishText(): string;
begin
  Result := FKeyword;
  // to-do: include properties
end;

function TSDVTagItem.ToText(): string;
begin
  Result := FKeyword;
  // to-do: include properties
end;

function TSDVTagItem.PropByKeyword(const Keyword: string): TSDVTagProperty;
var Found: Boolean; Index: Integer;
begin
  Index := 0; Found := FALSE; Result := nil;
  while ((Index < Count) and (not Found)) do
  begin
    Result := (FProperties.Items[Index] as TSDVTagProperty);
    Found  := ANSISameText(Result.Keyword, Keyword);
    System.Inc(Index);
  end;
  if not Found
    then Result := nil;
end;

function TSDVTagItem.RegisterProperty(const Keyword: string): TSDVTagProperty;
begin
  Result := PropByKeyword(Keyword);
  if (Result = nil) then
  begin
    Result := (FProperties.Add as TSDVTagProperty);
    Result.Keyword := Keyword;
  end;
end;

function TSDVTagItem.RegisterPropertyValue
  (const Keyword, Value: string): TSDVTagProperty;
begin
  Result := PropByKeyword(Keyword);
  if (Result = nil) then
  begin
    Result := (FProperties.Add as TSDVTagProperty);
    Result.Keyword := Keyword;
    Result.Value   := Value;
  end;
end;

procedure TSDVTagItem.CopyPropTo(const Dest: TSDVTagItem);
var I: Integer; S, D: TSDVTagProperty;
begin
  Dest.Properties.Clear;

  for I := 0 to Pred(Self.Properties.Count) do
  begin
    S := Self.Properties.PropByIndex(i);
    D := Dest.RegisterProperty(S.Keyword);
    D.Value := S.Value;
  end;
end;

(* TSDVTagCollection *)

function TSDVTagCollection.CreateNodeByClass(): TSDVTreeNode;
begin
  Result := TSDVTagItem.Create();
  Result.DoCreate();
end;

procedure TSDVTagCollection.DoCreate();
begin
  inherited DoCreate();
  {Your Code...}
end;

procedure TSDVTagCollection.DoDestroy();
begin
  {Your Code...}
  inherited DoDestroy();
end;

(* TCustomSDVTagPage *)

function TCustomSDVTagPage.CreateCollectionByClass(): TSDVContainerTreeCollection;
begin
  Result := TSDVTagCollection.Create();
  Result.DoCreate();
end;

constructor TCustomSDVTagPage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {Your Code...}
end;

destructor TCustomSDVTagPage.Destroy();
begin
  {Your Code...}
  inherited Destroy();
end;

end.
 
