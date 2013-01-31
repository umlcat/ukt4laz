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

unit uktbookmngrs;

interface

uses
  SysUtils, Classes,
  dummy;

const
  IndexNotFound = -1;

type

(* TSDVOnBookmarkEvent *)

  TSDVOnBookmarkEvent = procedure (Sender: TObject; LineNo: Integer) of object;

(* TSDVBookmark *)

  TSDVBookmark = class(TCollectionItem)
  private
    (* private declarations *)
  protected
    (* protected declarations *)

    FLineNo: Integer;
  public
    (* public declarations *)

    constructor Create(ACollection: TCollection); override;
    destructor Destroy(); override;
  published
    (* published declarations *)

    property LineNo: Integer
      read FLineNo write FLineNo;
  end;

(* TSDVBookmarks *)

  TSDVBookmarks = class(TOwnedCollection)
  private
    (* private declarations *)
  protected
    (* protected declarations *)
  public
    (* public declarations *)

    constructor Create(AOwner: TPersistent; AItemClass: TCollectionItemClass);
    destructor Destroy(); override;
  published
    (* published declarations *)
  end;

(* TCustomuktBookmarkMngr *)

  TCustomuktBookmarkMngr = class(TComponent)
  private
    (* private declarations *)
  protected
    (* protected declarations *)

    FOnInsert: TSDVOnBookmarkEvent;
    FOnRemove: TSDVOnBookmarkEvent;

    FBookmarks: TSDVBookmarks;

    procedure DelegateOnInsert(const Value: Integer);
    procedure DelegateOnRemove(const Value: Integer);
  public
    (* public declarations *)

    function IndexOf(const LineNo: Integer): Integer;
    function HasBookmark(const LineNo: Integer): Boolean;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Insert(const LineNo: Integer);
    procedure Remove(const LineNo: Integer);

    property Bookmarks: TSDVBookmarks
      read FBookmarks write FBookmarks;
    property OnInsert: TSDVOnBookmarkEvent
      read FOnInsert write FOnInsert;
    property OnRemove: TSDVOnBookmarkEvent
      read FOnRemove write FOnRemove;
  end;

(* TSDVBookmarkMngr *)

  TSDVBookmarkMngr = class(TCustomuktBookmarkMngr)
  published
    (* published declarations *)

    (* TCustomuktBookmarkMngr: *)

    property Bookmarks;
    property OnInsert;
    property OnRemove;
  end;

implementation

(* TSDVBookmark *)

constructor TSDVBookmark.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FLineNo := -1;
end;

destructor TSDVBookmark.Destroy();
begin
  FLineNo := -1;
  inherited Destroy();
end;

(* TSDVBookmarks *)

constructor TSDVBookmarks.Create(AOwner: TPersistent; AItemClass: TCollectionItemClass);
begin
  inherited Create(AOwner, AItemClass);
  // Your Code...
end;

destructor TSDVBookmarks.Destroy();
begin
  // Your Code...
  inherited Destroy();
end;

(* TCustomuktBookmarkMngr *)


procedure TCustomuktBookmarkMngr.DelegateOnInsert(const Value: Integer);
begin
  if (Assigned(FOnInsert))
    then FOnInsert(Self, Value);
end;

procedure TCustomuktBookmarkMngr.DelegateOnRemove(const Value: Integer);
begin
  if (Assigned(FOnRemove))
    then FOnRemove(Self, Value);
end;

function TCustomuktBookmarkMngr.IndexOf(const LineNo: Integer): Integer;
var AIndex, ACount: Integer; Found: Boolean; Bookmark: TSDVBookmark;
begin
  Result := IndexNotFound;
  AIndex := 0; ACount := FBookmarks.Count; Found := FALSE;
  while ((AIndex < ACount) and (not Found)) do
  begin
    Bookmark := (FBookmarks.Items[AIndex] as TSDVBookmark);
    Found := (Bookmark.LineNo = LineNo);
    Inc(AIndex);
  end;
  if (Found)
    then Result := Pred(AIndex);
end;

function TCustomuktBookmarkMngr.HasBookmark(const LineNo: Integer): Boolean;
begin
  Result := (IndexOf(LineNo) <> IndexNotFound);
end;

constructor TCustomuktBookmarkMngr.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBookmarks := TSDVBookmarks.Create(Self, TSDVBookmark);
end;

destructor TCustomuktBookmarkMngr.Destroy;
begin
  FBookmarks.Free;
  inherited Destroy;
end;

procedure TCustomuktBookmarkMngr.Insert(const LineNo: Integer);
begin
  if (IndexOf(LineNo) = IndexNotFound) then
  begin
    (FBookmarks.Add as TSDVBookmark).LineNo := LineNo;
    DelegateOnInsert(LineNo);
  end;
end;

procedure TCustomuktBookmarkMngr.Remove(const LineNo: Integer);
var AIndex: Integer;
begin
  AIndex := IndexOf(LineNo);
  if (AIndex <> IndexNotFound) then
  begin
    FBookmarks.Delete(AIndex);
    DelegateOnRemove(LineNo);
  end;
end;

end.
