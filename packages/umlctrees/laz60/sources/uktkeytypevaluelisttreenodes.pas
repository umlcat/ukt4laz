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

unit uktkeytypevaluelisttreenodes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  //uktlists,
  ukttreenodes,
  uktonlystringkeytypevaluelists,
  dummy;

(**
 ** Description:
 ** This unit contains several in order to support an non visual,
 ** hierarchical ( "tree" ) collection of data.
 **
 ** It was not designed with generics collections.
 **
 ** Additionally, a "KeyTypeValueList" property and helper functions are provided,
 ** Note: Its a list of "KeyTypeValue" items, property,
 ** not a "KeyTypeValue" property, itself.
 **)

type

(* TSDVKeyTypeValueListTreeNode *)

  TSDVKeyTypeValueListTreeNode = class(TSDVTreeNode)
  private
    (* Private declarations *)

  protected
    (* Protected declarations *)

    FKeyTypeValueList : TSDVOnlyStringKeyTypeValueList;
  public
    (* Public declarations *)

    procedure DoCreate(); override;
    procedure DoDestroy(); override;

    function KeyTypeValueList(): TSDVOnlyStringKeyTypeValueList;
  end;

(* TSDVKeyTypeValueListTreeCollection *)

  TSDVKeyTypeValueListTreeCollection = class(TSDVTreeCollection)
  private
    (* Private declarations *)

  protected
    (* Protected declarations *)

    function CreateNodeByClass(): TSDVTreeNode; override;
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
function TSDVKeyTypeValueListTreeNode.KeyTypeValueList(): TSDVOnlyStringKeyTypeValueList;
begin
  Result := Self.FKeyTypeValueList;
end;

(* TSDVKeyTypeValueListTreeCollection *)

function TSDVKeyTypeValueListTreeCollection.CreateNodeByClass(): TSDVTreeNode;
begin
  Result := TSDVKeyTypeValueListTreeNode.Create();
  Result.DoCreate();
end;

end.

