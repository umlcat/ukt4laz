(**
 **************************************************************************
 *                                                                        *
 *  This file is part of the uktat Developer's Component Library.        *
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

unit uktstrstrlists;

{$mode objfpc}{$H+}

interface
uses
  Classes, SysUtils,
  uktobjtypes,
  uktlists,
  dummy;

// Declares a TStringList descendant,
// where data objects are TStringObjects,
// that can be removed automatically.

type

  (* TSDVStringStringList *)

    TSDVStringStringList = class(TStringList)
    private
      (* Private declarations *)
    protected
      (* Protected declarations *)
    public
      (* Public declarations *)

      // TObject.Create() is non-virtual
      // let's provide a default virtual without parameters constructor
      constructor Create(); reintroduce; virtual;
      // let's provide a default virtual without parameters destructor
      destructor Destroy(); override;
    public
      (* Public declarations *)
      function AddWithData(const AItem, AData: string): Integer; virtual;

      function IndexOfItem(const AItem: string): Integer; virtual;
      function IndexOfData(const AData: string): Integer; virtual;
    end;

implementation

(* TSDVStringStringList *)

constructor TSDVStringStringList.Create();
begin
  inherited Create();
  // call each item data object destructor
  // and deallocate from memory
  Self.OwnsObjects := true;
end;

destructor TSDVStringStringList.Destroy();
begin
  Self.Clear();
  inherited Destroy();
end;

function TSDVStringStringList.AddWithData(const AItem, AData: string): Integer;
var AString: TSDVStringObject;
begin
  Result := -1;

  // prepare string storage
  AString := TSDVStringObject.Create();
  AString.Value := AData;

  Result := Self.AddObject(AItem, AString);
end;

function TSDVStringStringList.IndexOfItem(const AItem: string): Integer;
begin
  Result := Self.IndexOf(AItem);
end;

function TSDVStringStringList.IndexOfData(const AData: string): Integer;
begin
  Result := -1;
end;

end.

