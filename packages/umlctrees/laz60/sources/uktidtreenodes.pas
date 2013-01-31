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

unit uktidtreenodes;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  ukttreenodes,
  ukttreecntrs,
  dummy;

type

(* TSDVIdTreeNode *)

  TSDVIdTreeNode = class(TSDVContainerTreeNode)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)

    function getIdentifier(): string;
    procedure setIdentifier(const Value: string);
  public
    (* Public declarations *)

    property Identifier: string
      read getIdentifier write setIdentifier;
  end;

(* TSDVIdTreeCollection *)

  TSDVIdTreeCollection = class(TSDVContainerTreeCollection)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)
  public
    (* Public declarations *)

    function CreateNodeByClass(): TSDVTreeNode; override;
  end;

(* TSDVIdTreeContainer *)

  TSDVIdTreeContainer = class(TCustomSDVTreeContainer)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)

    function CreateCollectionByClass(): TSDVContainerTreeCollection; override;
  public
    (* Public declarations *)
  end;

implementation

(* TSDVIdTreeNode *)

function TSDVIdTreeNode.getIdentifier(): string;
begin
  Result := Self.Text;
end;

procedure TSDVIdTreeNode.setIdentifier(const Value: string);
begin
  Self.Text := Value;
end;

(* TSDVIdTreeCollection *)

function TSDVIdTreeCollection.CreateNodeByClass(): TSDVTreeNode;
begin
  Result := TSDVIdTreeNode.Create();
  Result.DoCreate();
end;

(* TSDVIdTreeContainer *)

function TSDVIdTreeContainer.CreateCollectionByClass(): TSDVContainerTreeCollection;
begin
  Result := TSDVIdTreeCollection.Create();
  Result.DoCreate();
end;

end.

