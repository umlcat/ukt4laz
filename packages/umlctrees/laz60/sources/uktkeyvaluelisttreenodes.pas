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

unit uktkeyvaluelisttreenodes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  //uktlists,
  ukttreenodes,
  uktkeyvaluemodes,
  uktonlystringkeyvaluelists,
  dummy;

(**
 ** Description:
 ** This unit contains several in order to support an non visual,
 ** hierarchical ( "tree" ) collection of data.
 **
 ** It was not designed with generics collections.
 **
 ** Additionally, a "KeyValueList" property and helper functions are provided,
 ** Note: Its a list of "KeyValue" items, property,
 ** not a "KeyValue" property, itself.
 **)

type

(* TSDVKeyValueListTreeNode *)

  TSDVKeyValueListTreeNode = class(TSDVTreeNode)
  private
    (* Private declarations *)

  protected
    (* Protected declarations *)

    _KeyValueList : TSDVOnlyStringKeyValueList;
  public
    (* Public declarations *)

    procedure DoCreate(); override;
    procedure DoDestroy(); override;

    function KeyValueList(): TSDVOnlyStringKeyValueList;
  end;

(* TSDVKeyValueListTreeCollection *)

  TSDVKeyValueListTreeCollection = class(TSDVTreeCollection)
  private
    (* Private declarations *)

  protected
    (* Protected declarations *)

    function CreateNodeByClass(): TSDVTreeNode; override;
  public
    (* Public declarations *)
  end;

implementation

(* TSDVKeyValueListTreeNode *)

procedure TSDVKeyValueListTreeNode.DoCreate();
begin
  inherited DoCreate();

  _KeyValueList := TSDVOnlyStringKeyValueList.Create();
end;

procedure TSDVKeyValueListTreeNode.DoDestroy();
begin
  _KeyValueList.Empty();
  _KeyValueList.Free();

  inherited DoDestroy();
end;

// "read-only"
function TSDVKeyValueListTreeNode.KeyValueList(): TSDVOnlyStringKeyValueList;
begin
  Result := Self._KeyValueList;
end;

(* TSDVKeyValueListTreeCollection *)

function TSDVKeyValueListTreeCollection.CreateNodeByClass(): TSDVTreeNode;
begin
  Result := TSDVKeyValueListTreeNode.Create();
  Result.DoCreate();
end;

end.

