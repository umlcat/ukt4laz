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

unit uktfilefilters;

interface
uses
  SysUtils, Classes,
  dummy;

const
  IndexNotFound = -1;

type

(* TSDVFileFilter *)

  TSDVFileFilter = class(TCollectionItem)
  private
    (* private declarations *)
  protected
    (* protected declarations *)

    FText: string;
    FExtension: string;
  public
    (* public declarations *)

    function Wildcard(): string;
    function Filter(): string;

    constructor Create(ACollection: TCollection); override;
    destructor Destroy(); override;
  published
    (* published declarations *)

    property Text: string
      read FText write FText;
    property Extension: string
      read FExtension write FExtension;
  end;

(* TSDVFileFilters *)

  TSDVFileFilters = class(TOwnedCollection)
  private
    (* private declarations *)
  protected
    (* protected declarations *)
  public
    (* public declarations *)

    function Filter: string;
    function FilterByExt(const Extension: string): Integer;
    function FilterByIndex(const Index: Integer): string;
    function ExtByIndex(const Index: Integer): string;

    constructor Create(AOwner: TPersistent; AItemClass: TCollectionItemClass);
    destructor Destroy(); override;
  published
    (* published declarations *)
  end;

(* TCustomSDVFileFiltersContainer *)

  TCustomSDVFileFiltersContainer = class(TComponent)
  private
    (* private declarations *)
  protected
    (* protected declarations *)

    FItems: TSDVFileFilters;
  public
    (* public declarations *)

    function Filter(): string;

    function LoadFilter(const Ext, Text: string): TSDVFileFilter;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;

    property Items: TSDVFileFilters
      read FItems write FItems;
  end;

(* TSDVFileFiltersContainer *)

  TSDVFileFiltersContainer = class(TCustomSDVFileFiltersContainer)
  published
    (* published declarations *)

    property Items;
  end;

implementation

(* TSDVFileFilter *)

function TSDVFileFilter.Wildcard(): string;
begin
  Result := '*.' + FExtension;
end;

function TSDVFileFilter.Filter(): string;
begin
  Result := FText + ' (' + Wildcard + ')' + '|' + Wildcard;
end;

constructor TSDVFileFilter.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FText := '';
  FExtension := '';
end;

destructor TSDVFileFilter.Destroy();
begin
  FText := '';
  FExtension := '';
  inherited Destroy();
end;

(* TSDVFileFilters *)

function TSDVFileFilters.Filter: string;
var AIndex, ACount: Integer; Item: TSDVFileFilter;
begin
  Result := '';
  ACount := Pred(Count);
  for AIndex := 0 to ACount do
  begin
    Item := (Items[AIndex] as TSDVFileFilter);
    if (AIndex = 0)
      then Result := Item.Filter
      else Result := Result + '|' + Item.Filter;
  end;
end;

function TSDVFileFilters.FilterByExt(const Extension: string): Integer;
var Index: Integer; Found: Boolean; EachFilter: TSDVFileFilter;
begin
  Result := IndexNotFound;
  Index := 0; Found := FALSE;
  while ((Index < Count) and (not Found)) do
  begin
    EachFilter := (Items[Index] as TSDVFileFilter);
    Found  := SysUtils.AnsiSameText(Extension, EachFilter.Extension);
    Inc(Index);
  end;

  if (Found)
    then Result := Pred(Index);
end;

function TSDVFileFilters.FilterByIndex(const Index: Integer): string;
begin
  Result := (Items[Index] as TSDVFileFilter).Filter;
end;

function TSDVFileFilters.ExtByIndex(const Index: Integer): string;
begin
  Result := (Items[Index] as TSDVFileFilter).Extension;
end;

constructor TSDVFileFilters.Create(AOwner: TPersistent; AItemClass: TCollectionItemClass);
begin
  inherited Create(AOwner, AItemClass);
  // Your Code...
end;

destructor TSDVFileFilters.Destroy();
begin
  // Your Code...
  inherited Destroy();
end;

(* TCustomSDVFileFiltersContainer *)

function TCustomSDVFileFiltersContainer.Filter(): string;
begin
  Result := (FItems as TSDVFileFilters).Filter;
end;

function TCustomSDVFileFiltersContainer.LoadFilter
  (const Ext, Text: string): TSDVFileFilter;
var Item: TSDVFileFilter;
begin
  Item := (Items.Add as TSDVFileFilter);
  Item.Text := Text;
  Item.Extension := Ext;

  Result := Item;
end;

constructor TCustomSDVFileFiltersContainer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItems := TSDVFileFilters.Create(Self, TSDVFileFilter);
end;

destructor TCustomSDVFileFiltersContainer.Destroy();
begin
  FItems.Free;
  inherited Destroy();
end;

end.


