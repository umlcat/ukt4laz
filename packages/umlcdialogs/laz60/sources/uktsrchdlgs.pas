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

unit uktsrchdlgs;

interface
uses
  SysUtils, Classes,
  uktcomponents,
  uktsrchtypes,
  uktdlgctrls,
  dummy;

type

{ TCustomSDVSearchContainer }

  TCustomSDVSearchContainer = class(TSDVDialogComponent)
  private
    (* Private declarations *)

    FOnHelpClick: TNotifyEvent;
  protected
    (* Protected declarations *)

    FSearchResult: TSDVSearchResult;
    FBookmarks:    TList;
    FSearchHistory: TStrings;

    FSearchText:  string;

    FOptions: TSDVSearchOptions;
    FStatus:  TSDVSearchStatus;

    FDirection: TSDVSearchDirection;
    FScope:     TSDVSearchScope;
    FOrigin:    TSDVSearchOrigin;
  protected
    (* Protected declarations *)

    function getBookmarks: TList; virtual;
    function getDirection: TSDVSearchDirection; virtual;
    function getScope: TSDVSearchScope; virtual;
    function getOrigin: TSDVSearchOrigin; virtual;

    function getHistory: TStrings; virtual;
    function getResult: TSDVSearchResult; virtual;
    function getText: string; virtual;

    function getShowHelp: Boolean; virtual;

    function getShowCaseSensitive: Boolean; virtual;
    function getShowWholeWordsOnly: Boolean; virtual;
    function getShowRegularExpressions: Boolean; virtual;

    function getWantCaseSensitive: Boolean; virtual;
    function getWantWholeWordsOnly: Boolean; virtual;
    function getWantRegularExpressions: Boolean; virtual;

    procedure setBookmarks(Value: TList); virtual;
    procedure setDirection(Value: TSDVSearchDirection); virtual;
    procedure setScope(Value: TSDVSearchScope); virtual;
    procedure setOrigin(Value: TSDVSearchOrigin); virtual;

    procedure setHistory(const Value: TStrings); virtual;
    procedure setResult(Value: TSDVSearchResult); virtual;
    procedure setText(Value: string); virtual;

    procedure setShowHelp(Value: Boolean); virtual;

    procedure setShowCaseSensitive(Value: Boolean); virtual;
    procedure setShowWholeWordsOnly(Value: Boolean); virtual;
    procedure setShowRegularExpressions(Value: Boolean); virtual;

    procedure setWantCaseSensitive(Value: Boolean); virtual;
    procedure setWantWholeWordsOnly(Value: Boolean); virtual;
    procedure setWantRegularExpressions(Value: Boolean); virtual;
  protected
    (* Protected declarations *)

    procedure DelegateOnHelpClick();
  public
    (* Public declarations *)

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    (* Public declarations *)

    { Never Published declarations }

    property Bookmarks: TList
      read getBookmarks write setBookmarks;
    property SearchResult: TSDVSearchResult
      read getResult write setResult;

    { UnPublished declarations }

    property SearchHistory: TStrings
      read getHistory write setHistory;
    property SearchText:  string
      read getText write setText;

    property Direction: TSDVSearchDirection
      read getDirection write setDirection;
    property Scope: TSDVSearchScope
      read getScope write setScope;
    property Origin: TSDVSearchOrigin
      read getOrigin write setOrigin;

    property ShowHelp: Boolean
      read getShowHelp write setShowHelp;
    property ShowCaseSensitive: Boolean
      read getShowCaseSensitive write setShowCaseSensitive;
    property ShowWholeWordsOnly: Boolean
      read getShowWholeWordsOnly write setShowWholeWordsOnly;
    property ShowRegularExpressions: Boolean
      read getShowRegularExpressions write setShowRegularExpressions;

    property WantCaseSensitive: Boolean
      read getWantCaseSensitive write setWantCaseSensitive;
    property WantWholeWordsOnly: Boolean
      read getWantWholeWordsOnly write setWantWholeWordsOnly;
    property WantRegularExpressions: Boolean
      read getWantRegularExpressions write setWantRegularExpressions;

    property OnHelpClick: TNotifyEvent
      read FOnHelpClick write FOnHelpClick; //default nil;
  end;

{ TCustomSDVReplaceContainer }

  TCustomSDVReplaceContainer = class(TCustomSDVSearchContainer)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)

    FReplaceText: string;
    FReplaceHistory: TStrings;

    function getReplaceHistory: TStrings; virtual;
    function getReplaceText: string; virtual;

    function getShowReplaceAll: Boolean; virtual;
    function getShowPromptOnReplace: Boolean; virtual;
    function getShowDeleteOnReplace: Boolean; virtual;
    function getShowKeepCapitalCase: Boolean; virtual;

    function getWantPromptOnReplace: Boolean; virtual;
    function getWantDeleteOnReplace: Boolean; virtual;
    function getWantKeepCapitalCase: Boolean; virtual;

    procedure setReplaceHistory(const Value: TStrings); virtual;
    procedure setReplaceText(Value: string); virtual;

    procedure setShowReplaceAll(Value: Boolean); virtual;
    procedure setShowPromptOnReplace(Value: Boolean); virtual;
    procedure setShowDeleteOnReplace(Value: Boolean); virtual;
    procedure setShowKeepCapitalCase(Value: Boolean); virtual;

    procedure setWantPromptOnReplace(Value: Boolean); virtual;
    procedure setWantDeleteOnReplace(Value: Boolean); virtual;
    procedure setWantKeepCapitalCase(Value: Boolean); virtual;
  public
    (* Public declarations *)

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { UnPublished declarations }

    property ReplaceHistory: TStrings
      read getReplaceHistory write setReplaceHistory;
    property ReplaceText: string
      read getReplaceText write setReplaceText;

    property ShowReplaceAll: Boolean
      read getShowReplaceAll write setShowReplaceAll;
    property ShowPromptOnReplace: Boolean
      read getShowPromptOnReplace write setShowPromptOnReplace;
    property ShowDeleteOnReplace: Boolean
      read getShowDeleteOnReplace write setShowDeleteOnReplace;
    property ShowKeepCapitalCase: Boolean
      read getShowKeepCapitalCase write setShowKeepCapitalCase;

    property WantPromptOnReplace: Boolean
      read getWantPromptOnReplace write setWantPromptOnReplace;
    property WantDeleteOnReplace: Boolean
      read getWantDeleteOnReplace write setWantDeleteOnReplace;
    property WantKeepCapitalCase: Boolean
      read getWantKeepCapitalCase write setWantKeepCapitalCase;
  end;

implementation

{ TCustomSDVSearchContainer }

function TCustomSDVSearchContainer.getBookmarks: TList;
begin
  Result := FBookmarks;
  { Goal: "Bookmarks" property get method .}
  { Objetivo: Metodo lectura propiedad "Bookmarks" .}
end;

function TCustomSDVSearchContainer.getDirection: TSDVSearchDirection;
begin
  Result := FDirection;
  { Goal: "Direction" property get method .}
  { Objetivo: Metodo lectura propiedad "Direction" .}
end;

function TCustomSDVSearchContainer.getScope: TSDVSearchScope;
begin
  Result := FScope;
  { Goal: "Scope" property get method .}
  { Objetivo: Metodo lectura propiedad "Scope" .}
end;

function TCustomSDVSearchContainer.getOrigin: TSDVSearchOrigin;
begin
  Result := FOrigin;
  { Goal: "Origin" property get method .}
  { Objetivo: Metodo lectura propiedad "Origin" .}
end;

function TCustomSDVSearchContainer.getHistory: TStrings;
begin
  Result := FSearchHistory;
  { Goal: "SearchHistory" property get method .}
  { Objetivo: Metodo lectura propiedad "SearchHistory" .}
end;

function TCustomSDVSearchContainer.getResult: TSDVSearchResult;
begin
  Result := FSearchResult;
  { Goal: "SearchResult" property get method .}
  { Objetivo: Metodo lectura propiedad "SearchResult" .}
end;

function TCustomSDVSearchContainer.getText: string;
begin
  Result := FSearchText;
  { Goal: "SearchText" property get method .}
  { Objetivo: Metodo lectura propiedad "SearchText" .}
end;

procedure TCustomSDVSearchContainer.setText(Value: string);
begin
  FSearchText := Value;
  { Goal: "SearchText" property get method .}
  { Objetivo: Metodo lectura propiedad "SearchText" .}
end;


function TCustomSDVSearchContainer.getShowHelp: Boolean;
begin
  Result := (sropShowHelp in FOptions);
  { Goal: "ShowHelp" property get method .}
  { Objetivo: Metodo lectura propiedad "ShowHelp" .}
end;

function TCustomSDVSearchContainer.getShowCaseSensitive: Boolean;
begin
  Result := (sropShowCaseSensitive in FOptions);
  { Goal: "ShowCaseSensitive" property get method .}
  { Objetivo: Metodo lectura propiedad "ShowCaseSensitive" .}
end;

function TCustomSDVSearchContainer.getShowWholeWordsOnly: Boolean;
begin
  Result := (sropShowWholeWordsOnly in FOptions);
  { Goal: "ShowWholeWordsOnly" property get method .}
  { Objetivo: Metodo lectura propiedad "ShowWholeWordsOnly" .}
end;

function TCustomSDVSearchContainer.getShowRegularExpressions: Boolean;
begin
  Result := (sropShowRegularExpressions in FOptions);
  { Goal: "ShowRegularExpressions" property get method .}
  { Objetivo: Metodo lectura propiedad "ShowRegularExpressions" .}
end;

function TCustomSDVSearchContainer.getWantCaseSensitive: Boolean;
begin
  Result := (srstWantCaseSensitive in FStatus);
  { Goal: "WantCaseSensitive" property get method .}
  { Objetivo: Metodo lectura propiedad "WantCaseSensitive" .}
end;

function TCustomSDVSearchContainer.getWantWholeWordsOnly: Boolean;
begin
  Result := (srstWantWholeWordsOnly in FStatus);
  { Goal: "WantWholeWordsOnly" property get method .}
  { Objetivo: Metodo lectura propiedad "WantWholeWordsOnly" .}
end;

function TCustomSDVSearchContainer.getWantRegularExpressions: Boolean;
begin
  Result := (srstWantRegularExpressions in FStatus);
  { Goal: "WantRegularExpressions" property get method .}
  { Objetivo: Metodo lectura propiedad "WantRegularExpressions" .}
end;

procedure TCustomSDVSearchContainer.setBookmarks(Value: TList);
begin
  FBookmarks := Value;
  { Goal: "Bookmarks" property get method .}
  { Objetivo: Metodo lectura propiedad "Bookmarks" .}
end;

procedure TCustomSDVSearchContainer.setDirection(Value: TSDVSearchDirection);
begin
  FDirection := Value;
  { Goal: "Direction" property get method .}
  { Objetivo: Metodo lectura propiedad "Direction" .}
end;

procedure TCustomSDVSearchContainer.setScope(Value: TSDVSearchScope);
begin
  FScope := Value;
  { Goal: "Scope" property get method .}
  { Objetivo: Metodo lectura propiedad "Scope" .}
end;

procedure TCustomSDVSearchContainer.setOrigin(Value: TSDVSearchOrigin);
begin
  FOrigin := Value;
  { Goal: "Origin" property get method .}
  { Objetivo: Metodo lectura propiedad "Origin" .}
end;

procedure TCustomSDVSearchContainer.setHistory(const Value: TStrings);
begin
  FSearchHistory := Value;
  { Goal: "SearchHistory" property get method .}
  { Objetivo: Metodo lectura propiedad "SearchHistory" .}
end;

procedure TCustomSDVSearchContainer.setResult(Value: TSDVSearchResult);
begin
  FSearchResult := Value;
  { Goal: "SearchResult" property get method .}
  { Objetivo: Metodo lectura propiedad "SearchResult" .}
end;

procedure TCustomSDVSearchContainer.setShowHelp(Value: Boolean);
begin
  if (Value)
    then Include(FOptions, sropShowHelp)
    else Exclude(FOptions, sropShowHelp);
  { Goal: "ShowHelp" property get method .}
  { Objetivo: Metodo lectura propiedad "ShowHelp" .}
end;

procedure TCustomSDVSearchContainer.setShowCaseSensitive(Value: Boolean);
begin
  if (Value)
    then Include(FOptions, sropShowCaseSensitive)
    else Exclude(FOptions, sropShowCaseSensitive);
  { Goal: "ShowCaseSensitive" property get method .}
  { Objetivo: Metodo lectura propiedad "ShowCaseSensitive" .}
end;

procedure TCustomSDVSearchContainer.setShowWholeWordsOnly(Value: Boolean);
begin
  if (Value)
    then Include(FOptions, sropShowWholeWordsOnly)
    else Exclude(FOptions, sropShowWholeWordsOnly);
  { Goal: "ShowWholeWordsOnly" property get method .}
  { Objetivo: Metodo lectura propiedad "ShowWholeWordsOnly" .}
end;

procedure TCustomSDVSearchContainer.setShowRegularExpressions(Value: Boolean);
begin
  if (Value)
    then Include(FOptions, sropShowRegularExpressions)
    else Exclude(FOptions, sropShowRegularExpressions);
  { Goal: "ShowRegularExpressions" property get method .}
  { Objetivo: Metodo lectura propiedad "ShowRegularExpressions" .}
end;

procedure TCustomSDVSearchContainer.setWantCaseSensitive(Value: Boolean);
begin
  if (Value)
    then Include(FStatus, srstWantCaseSensitive)
    else Exclude(FStatus, srstWantCaseSensitive);
  { Goal: "WantCaseSensitive" property get method .}
  { Objetivo: Metodo lectura propiedad "WantCaseSensitive" .}
end;

procedure TCustomSDVSearchContainer.setWantWholeWordsOnly(Value: Boolean);
begin
  if (Value)
    then Include(FStatus, srstWantWholeWordsOnly)
    else Exclude(FStatus, srstWantWholeWordsOnly);
  { Goal: "WantWholeWordsOnly" property get method .}
  { Objetivo: Metodo lectura propiedad "WantWholeWordsOnly" .}
end;

procedure TCustomSDVSearchContainer.setWantRegularExpressions(Value: Boolean);
begin
  if (Value)
    then Include(FStatus, srstWantRegularExpressions)
    else Exclude(FStatus, srstWantRegularExpressions);
  { Goal: "WantRegularExpressions" property get method .}
  { Objetivo: Metodo lectura propiedad "WantRegularExpressions" .}
end;

constructor TCustomSDVSearchContainer.Create(AOwner: TComponent);
begin
  inherited;
  // Call TComponent.Create;
  // Llamar TComponent.Create;

  FBookmarks := TList.Create;
  FSearchHistory := TStringList.Create;

  FSearchResult := srrNone;
  FSearchText := '';

  FOptions := [];
  FStatus  := [];

  Include(FOptions, sropShowHelp);
  Include(FOptions, sropShowCaseSensitive);
  Include(FOptions, sropShowWholeWordsOnly);
  Include(FOptions, sropShowRegularExpressions);

  FDirection := srdForward;
  FScope :=     srscpGlobal;
  FOrigin :=    sropFromCursor;
  // Clean the new properties at start
  // Limpiar las nuevas propiedades al inicio

  { Goal: To prepare the component .}
  { Objetivo: Preparar el componente .}
end;

destructor TCustomSDVSearchContainer.Destroy;
begin
  FSearchHistory.Free;
  FBookmarks.Free;
  // Clean the new properties at finish
  // Limpiar las nuevas propiedades al terminar

  inherited Destroy;
  { Goal: To unprepare the component .}
  { Objetivo: Despreparar el componente .}
end;

procedure TCustomSDVSearchContainer.DelegateOnHelpClick();
begin
  if (Assigned(FOnHelpClick))
    then FOnHelpClick(Self);
end;

{ TCustomSDVReplaceContainer }

function TCustomSDVReplaceContainer.getReplaceHistory: TStrings;
begin
  Result := FReplaceHistory;
  { Goal: "ReplaceHistory" property get method .}
  { Objetivo: Metodo lectura propiedad "ReplaceHistory" .}
end;

function TCustomSDVReplaceContainer.getReplaceText: string;
begin
  Result := FReplaceText;
  { Goal: "ReplaceText" property get method .}
  { Objetivo: Metodo lectura propiedad "ReplaceText" .}
end;

function TCustomSDVReplaceContainer.getShowReplaceAll: Boolean;
begin
  Result := (sropShowReplaceAll in FOptions);
  { Goal: "ShowReplaceAll" property get method .}
  { Objetivo: Metodo lectura propiedad "ShowReplaceAll" .}
end;

function TCustomSDVReplaceContainer.getShowPromptOnReplace: Boolean;
begin
  Result := (sropShowPromptOnReplace in FOptions);
  { Goal: "ShowPromptOnReplace" property get method .}
  { Objetivo: Metodo lectura propiedad "ShowPromptOnReplace" .}
end;

function TCustomSDVReplaceContainer.getShowDeleteOnReplace: Boolean;
begin
  Result := (sropShowDeleteOnReplace in FOptions);
  { Goal: "ShowDeleteOnReplace" property get method .}
  { Objetivo: Metodo lectura propiedad "ShowDeleteOnReplace" .}
end;

function TCustomSDVReplaceContainer.getShowKeepCapitalCase: Boolean;
begin
  Result := (sropShowKeepCapitalCase in FOptions);
  { Goal: "ShowKeepCapitalCase" property get method .}
  { Objetivo: Metodo lectura propiedad "ShowKeepCapitalCase" .}
end;

function TCustomSDVReplaceContainer.getWantPromptOnReplace: Boolean;
begin
  Result := (srstWantPromptOnReplace in FStatus);
  { Goal: "WantPromptOnReplace" property get method .}
  { Objetivo: Metodo lectura propiedad "WantPromptOnReplace" .}
end;

function TCustomSDVReplaceContainer.getWantDeleteOnReplace: Boolean;
begin
  Result := (srstWantDeleteOnReplace in FStatus);
  { Goal: "WantDeleteOnReplace" property get method .}
  { Objetivo: Metodo lectura propiedad "WantDeleteOnReplace" .}
end;

function TCustomSDVReplaceContainer.getWantKeepCapitalCase: Boolean;
begin
  Result := (srstWantKeepCapitalCase in FStatus);
  { Goal: "WantKeepCapitalCase" property get method .}
  { Objetivo: Metodo lectura propiedad "WantKeepCapitalCase" .}
end;

procedure TCustomSDVReplaceContainer.setReplaceHistory(const Value: TStrings);
begin
  FReplaceHistory := Value;
  { Goal: "ReplaceHistory" property get method .}
  { Objetivo: Metodo lectura propiedad "ReplaceHistory" .}
end;

procedure TCustomSDVReplaceContainer.setReplaceText(Value: string);
begin
  FReplaceText := Value;
  { Goal: "ReplaceText" property get method .}
  { Objetivo: Metodo lectura propiedad "ReplaceText" .}
end;

procedure TCustomSDVReplaceContainer.setShowReplaceAll(Value: Boolean);
begin
  if (Value)
    then Include(FOptions, sropShowReplaceAll)
    else Exclude(FOptions, sropShowReplaceAll);
  { Goal: "ShowReplaceAll" property get method .}
  { Objetivo: Metodo lectura propiedad "ShowReplaceAll" .}
end;

procedure TCustomSDVReplaceContainer.setShowPromptOnReplace(Value: Boolean);
begin
  if (Value)
    then Include(FOptions, sropShowPromptOnReplace)
    else Exclude(FOptions, sropShowPromptOnReplace);
  { Goal: "ShowPromptOnReplace" property get method .}
  { Objetivo: Metodo lectura propiedad "ShowPromptOnReplace" .}
end;

procedure TCustomSDVReplaceContainer.setShowDeleteOnReplace(Value: Boolean);
begin
  if (Value)
    then Include(FOptions, sropShowDeleteOnReplace)
    else Exclude(FOptions, sropShowDeleteOnReplace);
  { Goal: "ShowDeleteOnReplace" property get method .}
  { Objetivo: Metodo lectura propiedad "ShowDeleteOnReplace" .}
end;

procedure TCustomSDVReplaceContainer.setShowKeepCapitalCase(Value: Boolean);
begin
  if (Value)
    then Include(FOptions, sropShowKeepCapitalCase)
    else Exclude(FOptions, sropShowKeepCapitalCase);
  { Goal: "ShowKeepCapitalCase" property get method .}
  { Objetivo: Metodo lectura propiedad "ShowKeepCapitalCase" .}
end;

procedure TCustomSDVReplaceContainer.setWantPromptOnReplace(Value: Boolean);
begin
  if (Value)
    then Include(FStatus, srstWantPromptOnReplace)
    else Exclude(FStatus, srstWantPromptOnReplace);
  { Goal: "WantPromptOnReplace" property get method .}
  { Objetivo: Metodo lectura propiedad "WantPromptOnReplace" .}
end;

procedure TCustomSDVReplaceContainer.setWantDeleteOnReplace(Value: Boolean);
begin
  if (Value)
    then Include(FStatus, srstWantDeleteOnReplace)
    else Exclude(FStatus, srstWantDeleteOnReplace);
  { Goal: "WantDeleteOnReplace" property get method .}
  { Objetivo: Metodo lectura propiedad "WantDeleteOnReplace" .}
end;

procedure TCustomSDVReplaceContainer.setWantKeepCapitalCase(Value: Boolean);
begin
  if (Value)
    then Include(FStatus, srstWantKeepCapitalCase)
    else Exclude(FStatus, srstWantKeepCapitalCase);
  { Goal: "WantKeepCapitalCase" property get method .}
  { Objetivo: Metodo lectura propiedad "WantKeepCapitalCase" .}
end;

constructor TCustomSDVReplaceContainer.Create(AOwner: TComponent);
begin
  inherited;
  // Call TComponent.Create;
  // Llamar TComponent.Create;

  Include(FOptions, sropShowReplaceAll);

  Include(FOptions, sropShowPromptOnReplace);
  Include(FOptions, sropShowDeleteOnReplace);
  Include(FOptions, sropShowKeepCapitalCase);
  
  FReplaceHistory := TStringList.Create;
  FReplaceText := '';
  // Clean the new properties at start
  // Limpiar las nuevas propiedades al inicio

  { Goal: To prepare the component .}
  { Objetivo: Preparar el componente .}
end;

destructor TCustomSDVReplaceContainer.Destroy;
begin
  FReplaceHistory.Free;
  // Clean the new propierties at finish
  // Limpiar las nuevas propiedades al terminar

  inherited Destroy;
  { Goal: To unprepare the component .}
  { Objetivo: Despreparar el componente .}
end;

end.



