unit ukttreestates;

interface

const
  siNone      = 0;
  siEmpty     = 1;
  siCollapsed = 2;
  siExpanded  = 3;
  siExploring = 4;
  siUnknown   = 5;

type

{ TTreeStates }

  TTreeStates =
    (
     tsEmpty,
     tsCollapsed,
     tsExpanded,
     tsExploring,
     tsUnknown
    );


  PTreeStates = ^TTreeStates;

  function StateToChar(const Value: TTreeStates): char;
  function StateToIndex(const Value: TTreeStates): Integer;

implementation

function StateToChar(const Value: TTreeStates): char;
begin
  case (Value) of
    { [.] } tsEmpty:     Result := '.';
    { [+] } tsCollapsed: Result := '+';
    { [-] } tsExpanded:  Result := '-';
    { [*] } tsExploring: Result := '*';
    { [?] } tsUnknown:   Result := '?';
    else Result := '.';
  end;
end;

function StateToIndex(const Value: TTreeStates): Integer;
begin
  case (Value) of
    tsEmpty:     Result := siEmpty;
    tsCollapsed: Result := siCollapsed;
    tsExpanded:  Result := siExpanded;
    tsExploring: Result := siExploring;
    tsUnknown:   Result := siUnknown;
    else Result := siNone;
  end;
  // Goal: Returns the image index for the given treestate.
  // Objetivo: Regresa el indice de imagen para el estadoarbol dado.
end;

end.
