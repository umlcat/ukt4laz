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

unit uktkeytypevaluelisttreecntrs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uktlists,
  uktonlystringkeytypevaluelists,
  ukttreenodes,
  ukttreecntrs,
  dummy;

(**
 ** Description:
 ** This unit contains several in order to support an non visual,
 ** hierarchical ( "tree" ) collection of data.
 **
 ** It was not designed with generics collections.
 **
 ** Each element of the tree-collection, has a sequential,
 ** non-hierarchical list of key-type-value elements.
 **)

type

(* TSDVKeyTypeValueListTreeNode *)

  TSDVKeyTypeValueListTreeNode = class(TSDVContainerTreeNode)
  private
    (* Private declarations *)

  protected
    (* Protected declarations *)

    FKeyTypeValueList : TSDVOnlyStringKeyTypeValueList;
  public
    (* Public declarations *)

    procedure DoCreate(); override;
    procedure DoDestroy(); override;
  public
    (* Public declarations *)

    (* read-only properties *)

    function Properties(): TSDVOnlyStringKeyTypeValueList;
  end;

(* TCustomSDVKeyTypeValueTreeCollection *)

  TCustomSDVKeyTypeValueTreeCollection = class(TSDVContainerTreeCollection)
  private
    (* Private declarations *)

  protected
    (* Protected declarations *)

    function CreateNodeByClass(): TSDVTreeNode; override;
  public
    (* Public declarations *)
  end;

(* TCustomSDVKeyTypeValueTreeContainer *)

  TCustomSDVKeyTypeValueTreeContainer = class(TCustomSDVTreeContainer)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)

    function CreateCollectionByClass(): TSDVContainerTreeCollection; override;
  public
    (* Public declarations *)
  end;

implementation

(* TSDVKeyTypeValueListTreeNode *)

procedure TSDVKeyTypeValueListTreeNode.DoCreate();
begin
  inherited DoCreate();
  FKeyTypeValueList := TSDVOnlyStringKeyTypeValueList.Create();
end;

procedure TSDVKeyTypeValueListTreeNode.DoDestroy();
begin
  FKeyTypeValueList.Empty();
  FKeyTypeValueList.Free();
  inherited DoDestroy();
end;

// "read-only"
function TSDVKeyTypeValueListTreeNode.Properties(): TSDVOnlyStringKeyTypeValueList;
begin
  Result := FKeyTypeValueList;
end;

(* TCustomSDVKeyTypeValueTreeCollection *)

function TCustomSDVKeyTypeValueTreeCollection.CreateNodeByClass(): TSDVTreeNode;
begin
  Result := TSDVKeyTypeValueListTreeNode.Create();
  Result.DoCreate();
end;

(* TCustomSDVKeyTypeValueTreeContainer *)

function TCustomSDVKeyTypeValueTreeContainer.CreateCollectionByClass(): TSDVContainerTreeCollection;
begin
  Result := TCustomSDVKeyTypeValueTreeCollection.Create();
  Result.DoCreate();
end;

end.

