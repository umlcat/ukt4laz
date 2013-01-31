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

unit uktxmltreecntrs;

interface

uses
  SysUtils, Classes,
  ukttreenodes, ukttreecntrs,
  uktTagProps, ukttagattrdocs,
  uktxmlFileTokens,
  dummy;

type

(* TXMLItem *)

  TXMLItem = class(TSDVTagAttributesItem)
  private
    (* private declarations *)
  protected
    (* protected declarations *)

    FToken: TXMLFileToken;

    procedure PropToText(var S: string);
  public
    (* public declarations *)

    procedure DoCreate(); override;
    procedure DoDestroy(); override;
  public
    (* public declarations *)

    function ToStartText(): string; override;
    function ToFinishText(): string; override;
    function ToText(): string; override;
  published
    (* published declarations *)

    property Token: TXMLFileToken
      read FToken write FToken;
  end;

(* TXMLCollection *)

  TXMLCollection = class(TSDVTagAttributesCollection)
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

(* TCustomXMLTreeContainer *)

  TCustomXMLTreeContainer = class(TCustomSDVTagAttributesTreeContainer)
  private
    (* private declarations *)
  protected
    (* protected declarations *)

    function CreateCollectionByClass(): TSDVContainerTreeCollection; override;
  public
    (* public declarations *)

    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
  end;

{ TXMLTreeContainer }

  TXMLTreeContainer = class(TCustomXMLTreeContainer)
  published
    (* published declarations *)

    { TCustomSDTreeContainer: }

    (*
    property OnInsert;
    property OnRemove;
    *)
  end;

implementation

(* TXMLItem *)

procedure TXMLItem.PropToText(var S: string);
var I, PropCount: Integer; EachProp: TSDVTagProperty;
begin
  PropCount := Properties.Count;
  for I := 0 to Pred(PropCount) do
  begin
    EachProp := (Properties.Items[i] as TSDVTagProperty);
    S := S + EachProp.Text;
  end;
  // agregar las propiedades con sus valores para el nodo indicado
  // add the properties with their values for the given node
end;

procedure TXMLItem.DoCreate();
begin
  inherited DoCreate();

  FToken := xmlfiletkNone;
end;

procedure TXMLItem.DoDestroy();
begin
  FToken := xmlfiletkNone;

  inherited DoDestroy();
end;

function TXMLItem.ToStartText(): string;
begin
  Result := FKeyword;
  // to-do: include properties

  Result := '';
  case (Token) of
    xmlfiletkEoF: ;
    xmlfiletkEoPg: ;
    xmlfiletkEoLn: ;
    xmlfiletkSpace: ;

    xmlfiletkComment: ;
    xmlfiletkEncoding: ;

    xmlfiletkStart:
      begin
        Result := Result + '<+' + FKeyword;
        PropToText(Result);
        Result := Result + '>';
      end;
    xmlfiletkFinish:
      Result := Result + '<-' + FKeyword + '>';
    xmlfiletkSingle:
      begin
        Result := '<';
        PropToText(Result);
        Result := Result + '>';
      end;

    xmlfiletkText: ;

    xmlfiletkEntity: ;

    xmlfiletkDecimal: ;
    xmlfiletkHexaDecimal: ;

    // xmlfiletkNone: ;
    else ;
  end;
end;

function TXMLItem.ToFinishText(): string;
begin
  Result := '<-' + FKeyword;
  if (Token = xmlfiletkEncoding)
    then Result := Result + ' ?>'
    else Result := Result + '>';
  // to-do: include properties
end;

function TXMLItem.ToText(): string;
begin
  Result := FKeyword;
  // to-do: include properties

  Result := '';
  case (Token) of
    xmlfiletkEoF: ;
    xmlfiletkEoPg:  Result := Result + #12;
    xmlfiletkEoLn:  Result := Result + #13;
    xmlfiletkSpace: Result := Result + #32;

    xmlfiletkText:  Result := FValue;

    xmlfiletkComment: ;
    xmlfiletkEncoding: ;

    xmlfiletkStart, // to-do
    xmlfiletkSingle:
      begin
        Result := '<' + FKeyword;
        PropToText(Result);
      Result := Result + '>';
      end;

    xmlfiletkEntity: ;

    xmlfiletkDecimal: ;
    xmlfiletkHexaDecimal: ;

    // xmlfiletkNone: ;
    // xmlfiletkStart: ;
    // xmlfiletkFinish: ;
    else ;
  end;
end;

(* TXMLCollection *)

function TXMLCollection.CreateNodeByClass(): TSDVTreeNode;
begin
  Result := TXMLItem.Create();
  Result.DoCreate();
end;

procedure TXMLCollection.DoCreate();
begin
  inherited DoCreate();
  {Your Code...}
end;

procedure TXMLCollection.DoDestroy();
begin
  {Your Code...}
  inherited DoDestroy();
end;

(* TCustomXMLTreeContainer *)

function TCustomXMLTreeContainer.CreateCollectionByClass(): TSDVContainerTreeCollection;
begin
  Result := TXMLCollection.Create();
end;

constructor TCustomXMLTreeContainer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {Your Code...}
end;

destructor TCustomXMLTreeContainer.Destroy();
begin
  {Your Code...}
  inherited Destroy();
end;

end.
 
