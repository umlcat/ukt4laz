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

unit ukttagprops;

interface
uses
  SysUtils, Classes;

type

(* TSDVTagProperty *)

  TSDVTagProperty = class(TCollectionItem)
  private
    (* private declarations *)
  protected
    (* protected declarations *)

    FKeyword: string;
    FValue: string;

    function getKeyword(): string; virtual;
    function getValue(): string; virtual;

    procedure setKeyword(const Value: string); virtual;
    procedure setValue(const Value: string); virtual;
  public
    (* public declarations *)

    function Text: string;

    constructor Create(ACollection: TCollection); override;
    destructor Destroy(); override;

    property Keyword: string
      read getKeyword write setKeyword;
    property Value: string
      read getValue write setValue;
  end;

(* TSDVTagProperties *)

  TSDVTagProperties = class(TCollection)
  private
    (* private declarations *)
  protected
    (* protected declarations *)
  public
    (* public declarations *)

    function IsEmpty: Boolean;

    function Text: string;

    function PropByKeyword(const Keyword: string): TSDVTagProperty;
    function PropByIndex(const Index: Integer): TSDVTagProperty;

    function RegisterProperty(const Keyword: string): TSDVTagProperty;
  end;

implementation

(* TSDVTagProperty *)

function TSDVTagProperty.getKeyword(): string;
begin
  Result := FKeyword;
  {Your Code...}
end;

function TSDVTagProperty.getValue(): string;
begin
  Result := FValue;
end;

procedure TSDVTagProperty.setKeyword(const Value: string);
begin
  if (Value <> FKeyword) then
  begin
    FKeyword := Value;
  end;
end;

procedure TSDVTagProperty.setValue(const Value: string);
begin
  if (Value <> FValue) then
  begin
    FValue := Value;
  end;
end;

function TSDVTagProperty.Text: string;
begin
  Result := '';
  if (FValue <> '')
    then Result := Format(' %s = "%s"', [FKeyword, FValue]);
end;

constructor TSDVTagProperty.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FKeyword := '';
  FValue   := '';
end;

destructor TSDVTagProperty.Destroy();
begin
  FValue   := '';
  FKeyword := '';
  inherited Destroy();
end;

(* TSDVTagProperties *)

function TSDVTagProperties.IsEmpty(): Boolean;
begin
  Result := (Count < 1);
end;

function TSDVTagProperties.Text: string;
var I: Integer;
begin
  Result := '';
  for I := 0 to Pred(Count) do
    Result := Result + (Items[I] as TSDVTagProperty).Text;
end;

function TSDVTagProperties.PropByKeyword(const Keyword: string): TSDVTagProperty;
var Found: Boolean; Index: Integer;
begin
  Index := 0; Found := FALSE; Result := nil;
  while ((Index < Count) and (not Found)) do
  begin
    Result := (Items[Index] as TSDVTagProperty);
    Found  := ANSISameText(Result.Keyword, Keyword);
    System.Inc(Index);
  end;
  if (not Found)
    then Result := nil;
end;

function TSDVTagProperties.PropByIndex(const Index: Integer): TSDVTagProperty;
begin
  Result := nil;
  if (Index <= Count) then
  begin
    Result := (Items[Index] as TSDVTagProperty);
  end;
end;

function TSDVTagProperties.RegisterProperty(const Keyword: string): TSDVTagProperty;
begin
  Result := PropByKeyword(Keyword);
  if (Result = nil) then
  begin
    Result := (Self.Add as TSDVTagProperty);
    Result.Keyword := Keyword;
  end;
end;

end.
 
