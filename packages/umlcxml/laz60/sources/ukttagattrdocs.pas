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
 
unit ukttagattrdocs;

interface

uses
  Windows, SysUtils, Classes,
  ukttreenodes, ukttreecntrs,
  ukttagprops, ukttagstyles,
  dummy;

type

(* TSDVTagAttributesItem *)

  TSDVTagAttributesItem = class(TSDVContainerTreeNode)
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
  public
    (* public declarations *)

    function ToStartText: string; dynamic;
    function ToFinishText: string; dynamic;
    function ToText: string; dynamic;

    function PropByKeyword(const Keyword: string): TSDVTagProperty;
    function RegisterProperty(const Keyword: string): TSDVTagProperty;
    function RegisterPropertyValue
      (const Keyword, Value: string): TSDVTagProperty;

    procedure CopyPropTo(const Dest: TSDVTagAttributesItem);
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

(* TSDVTagAttributesCollection *)

  TSDVTagAttributesCollection = class(TSDVContainerTreeCollection)
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

(* TCustomSDVTagAttributesTreeContainer *)

  TCustomSDVTagAttributesTreeContainer = class(TCustomSDVTreeContainer)
  private
    (* private declarations *)
  protected
    (* protected declarations *)

    function CreateCollectionByClass: TSDVContainerTreeCollection; override;
  public
    (* public declarations *)

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

(* TSDVTagAttributesTreeContainer *)

  TSDVTagAttributesTreeContainer = class(TCustomSDVTagAttributesTreeContainer)
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

(* TSDVTagAttributesItem *)

procedure TSDVTagAttributesItem.DoCreate();
begin
  inherited DoCreate();

  FKeyword := '';
  FUsesBreak := FALSE;
  FStyle := tsNone;

  FValue   := '';
  FProperties := TSDVTagProperties.Create(TSDVTagProperty);
end;

procedure TSDVTagAttributesItem.DoDestroy();
begin
  FProperties.Free();
  FValue   := '';

  FStyle := tsNone;
  FKeyword := '';
  inherited DoDestroy();
end;

function TSDVTagAttributesItem.ToStartText: string;
begin
  Result := FKeyword;
  // to-do: include properties
end;

function TSDVTagAttributesItem.ToFinishText: string;
begin
  Result := FKeyword;
  // to-do: include properties
end;

function TSDVTagAttributesItem.ToText: string;
begin
  Result := FKeyword;
  // to-do: include properties
end;

function TSDVTagAttributesItem.PropByKeyword(const Keyword: string): TSDVTagProperty;
var Found: Boolean; AIndex: Integer;
begin
  AIndex := 0; Found := FALSE; Result := nil;
  while ((AIndex < Count) and (not Found)) do
  begin
    Result := (FProperties.Items[AIndex] as TSDVTagProperty);
    Found  := ANSISameText(Result.Keyword, Keyword);
    System.Inc(AIndex);
  end;
  if (not Found)
    then Result := nil;
end;

function TSDVTagAttributesItem.RegisterProperty(const Keyword: string): TSDVTagProperty;
begin
  Result := PropByKeyword(Keyword);
  if (Result = nil) then
  begin
    Result := (FProperties.Add as TSDVTagProperty);
    Result.Keyword := Keyword;
  end;
end;

function TSDVTagAttributesItem.RegisterPropertyValue
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

procedure TSDVTagAttributesItem.CopyPropTo(const Dest: TSDVTagAttributesItem);
var I: Integer; S, D: TSDVTagProperty;
begin
  Dest.Properties.Clear();

  for I := 0 to (Self.Properties.Count - 1) do
  begin
    S := Self.Properties.PropByIndex(i);
    D := Dest.RegisterProperty(S.Keyword);
    D.Value := S.Value;
  end;
end;

(* TSDVTagAttributesCollection *)

function TSDVTagAttributesCollection.CreateNodeByClass(): TSDVTreeNode;
begin
  Result := TSDVTagAttributesItem.Create();
  Result.DoCreate();
end;

procedure TSDVTagAttributesCollection.DoCreate();
begin
  inherited DoCreate();
  {Your Code...}
end;

procedure TSDVTagAttributesCollection.DoDestroy();
begin
  {Your Code...}
  inherited DoDestroy();
end;

(* TCustomSDVTagAttributesTreeContainer *)

function TCustomSDVTagAttributesTreeContainer.CreateCollectionByClass(): TSDVContainerTreeCollection;
begin
  Result := TSDVTagAttributesCollection.Create();
end;

constructor TCustomSDVTagAttributesTreeContainer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {Your Code...}
end;

destructor TCustomSDVTagAttributesTreeContainer.Destroy();
begin
  {Your Code...}
  inherited Destroy();
end;

end.
 
