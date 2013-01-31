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

unit uktmacros;

interface
uses
  SysUtils, Classes,
  dummy;

type

{ TSDVMacroStyle }

  TSDVMacroStyle = (msNone, msExpansion, msCustom);

{ TSDVMacroExecuteEvent }

  TSDVMacro = class;
  TSDVMacroExecuteEvent =
    {^}function (Macro: TSDVMacro; const AParam: pointer): string of object;

{ TSDVMacro }

  TCustomSDVMacroContainer = class;
  TSDVMacro = class(TCollectionItem)
  private
    { private declarations }
  protected
    { protected declarations }

    FKeyword: string;
    FExpansion: string;
    FStyle: TSDVMacroStyle;

    FOnExecute: TSDVMacroExecuteEvent;

    function DelegateOnExecute(const AParam: pointer): string;
  public
    { public declarations }

    constructor Create(ACollection: TCollection); override;

    function Execute(const AParam: pointer): string; dynamic;
  published
    { published declarations }

    property Keyword: string
      read FKeyword write FKeyword;
    property Expansion: string
      read FExpansion write FExpansion;
    property Style: TSDVMacroStyle
      read FStyle write FStyle;

    property OnExecute: TSDVMacroExecuteEvent
      read FOnExecute write FOnExecute;
  end;

{ TSDVMacros }

  TSDVMacros = class(TCollection)
  private
    { private declarations }
  protected
    { protected declarations }
  public
    { public declarations }

    constructor Create(AOwner: TPersistent; AItemClass: TCollectionItemClass);
    destructor Destroy(); override;
  published
    { published declarations }
  end;

  TMacroNotFoundEvent =
    procedure (Sender: TObject; const Macro: string) of object;

{ TCustomSDVMacroContainer }

  TCustomSDVMacroContainer = class(TComponent)
  private
    { private declarations }
  protected
    { protected declarations }

    FItems: TSDVMacros;

    FOnMacroNotFound:   TMacroNotFoundEvent;

    procedure DelegateOnMacroNotFound(const Macro: string);

    procedure MacroNotFound(const Macro: string); dynamic;
  public
    { public declarations }

    function MacroByKeyword(const Keyword: string): TSDVMacro;
    function RegisterExpansion(const Keyword, Expansion: string): TSDVMacro;
    function RegisterCustom(const Keyword: string): TSDVMacro;
    function Execute(const Keyword: string; const Param: pointer): string;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Items: TSDVMacros
      read FItems write FItems;

    property OnMacroNotFound: TMacroNotFoundEvent
      read FOnMacroNotFound write FOnMacroNotFound;
  end;

{ TSDVMacrosContainer }

  TSDVMacrosContainer = class(TCustomSDVMacroContainer)
  published
    { published declarations }

    property Items;

    property OnMacroNotFound;
  end;

implementation

{ TSDVMacro }

function TSDVMacro.DelegateOnExecute(const AParam: pointer): string;
begin
  Result := '';
  if Assigned(FOnExecute) then Result := FOnExecute(Self, AParam);
end;

constructor TSDVMacro.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FKeyword := '';
  FExpansion := '';
  FStyle := msNone;

  FOnExecute := nil;
end;

function TSDVMacro.Execute(const AParam: pointer): string;
begin
  case (FStyle) of
    msExpansion: Result := FExpansion;
    msCustom:    Result := DelegateOnExecute(AParam);
//    msNone: ;
    else Result := '';
  end;
end;

{ TSDVMacros }

constructor TSDVMacros.Create(AOwner: TPersistent; AItemClass: TCollectionItemClass);
begin
  inherited Create(AItemClass);
  {Your Code...}
end;

destructor TSDVMacros.Destroy();
begin
  {Your Code...}
  inherited Destroy();
end;

{ TCustomSDVMacroContainer }

procedure TCustomSDVMacroContainer.DelegateOnMacroNotFound
  (const Macro: string);
begin
  if Assigned(FOnMacroNotFound) then FOnMacroNotFound(Self, Macro);
end;

procedure TCustomSDVMacroContainer.MacroNotFound(const Macro: string);
begin
  DelegateOnMacroNotFound(Macro);
end;

function TCustomSDVMacroContainer.
  MacroByKeyword(const Keyword: string): TSDVMacro;
var Found: Boolean; Index: Integer;
begin
  Index := 0; Found := FALSE; Result := nil;
  while ((Index < Items.Count) and (not Found)) do
  begin
    Result := (Items.Items[Index] as TSDVMacro);
    Found  := ANSISameText(Result.Keyword, Keyword);
    System.Inc(Index);
  end;
  if not Found
    then Result := nil;
end;

function TCustomSDVMacroContainer.RegisterExpansion
  (const Keyword, Expansion: string): TSDVMacro;
begin
  Result := (Items.Add as TSDVMacro);
  Result.Style   := msExpansion;
  Result.Keyword := Keyword;
  Result.Expansion := Expansion;
end;

function TCustomSDVMacroContainer.RegisterCustom
  (const Keyword: string): TSDVMacro;
begin
  Result := (Items.Add as TSDVMacro);
  Result.Style   := msCustom;
  Result.Keyword := Keyword;
end;

function TCustomSDVMacroContainer.Execute
  (const Keyword: string; const Param: pointer): string;
var Item: TSDVMacro;
begin
  Result := '';
  Item := MacroByKeyword(Keyword);
  if (Assigned(Item))
    then Result := Item.Execute(Param)
    else MacroNotFound(Keyword);
end;

constructor TCustomSDVMacroContainer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItems := TSDVMacros.Create(Self, TSDVMacro);
  {Your Code...}
end;

destructor TCustomSDVMacroContainer.Destroy;
begin
  {Your Code...}
  FItems.Free;
  inherited Destroy;
end;

end.
 
