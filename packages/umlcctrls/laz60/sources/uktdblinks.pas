unit uktdblinks;

interface
uses
  SysUtils, Classes,
{.IFDEF MSWINDOWS}
  Controls,
{.ENDIF}
  DB,
  dummy;

type

{ TSDVFieldDataLink }

  TSDVFieldDataLink = class(TDataLink)
  private
    { Private declarations }  

    FField: TField;
    FFieldName: string;
    FControl: TComponent;
    FEditing: Boolean;
    FModified: Boolean;

    FOnDataChange: TNotifyEvent;
    FOnEditingChange: TNotifyEvent;
    FOnUpdateData: TNotifyEvent;
    FOnActiveChange: TNotifyEvent;

    function getCanModify(): Boolean;

    procedure setEditing(Value: Boolean);
    procedure setField(Value: TField);
    procedure setFieldName(const Value: string);
    
	procedure UpdateField();
//    procedure UpdateRightToLeft();
  protected
    { Protected declarations }
	
    procedure ActiveChanged(); override;
    procedure EditingChanged(); override;
    procedure FocusControl(Field: TFieldRef); override;
    procedure LayoutChanged(); override;
    procedure RecordChanged(Field: TField); override;
    procedure UpdateData(); override;
  public
    { Public declarations }

    constructor Create;

    function Edit(): Boolean;
    
    procedure Modified();
    procedure Reset();

	(* properties *)
	
    property CanModify: Boolean read getCanModify;
    property Control: TComponent read FControl write FControl;
    property Editing: Boolean read FEditing;
    property Field: TField read FField;
    property FieldName: string read FFieldName write setFieldName;

    (* events: *)
  
    property OnDataChange: TNotifyEvent read FOnDataChange write FOnDataChange;
    property OnEditingChange: TNotifyEvent read FOnEditingChange write FOnEditingChange;
    property OnUpdateData: TNotifyEvent read FOnUpdateData write FOnUpdateData;
    property OnActiveChange: TNotifyEvent read FOnActiveChange write FOnActiveChange;
  end;

implementation

{ TSDVFieldDataLink }

function TSDVFieldDataLink.getCanModify(): Boolean;
begin
  Result := (not ReadOnly and (Field <> nil) and Field.CanModify);
end;

procedure TSDVFieldDataLink.setEditing(Value: Boolean);
begin
  if (FEditing <> Value) then
  begin
    FEditing := Value;
    FModified := false;
	
    if Assigned(FOnEditingChange)
       then FOnEditingChange(Self);
  end;
end;

procedure TSDVFieldDataLink.setField(Value: TField);
begin
  if (FField <> Value) then
  begin
    FField := Value;
    EditingChanged();
    RecordChanged(nil);
//    UpdateRightToLeft();
  end;
end;

procedure TSDVFieldDataLink.setFieldName(const Value: string);
begin
  if (FFieldName <> Value) then
  begin
    FFieldName := Value;
    UpdateField();
  end;
end;

procedure TSDVFieldDataLink.UpdateField();
begin
  if (Active and (FFieldName <> '')) then
  begin
    FField := nil;
    if (FControl <> nil) then
      setField(GetFieldProperty(DataSource.DataSet, FControl, FFieldName))
	else
      setField(DataSource.DataSet.FieldByName(FFieldName));
  end else
    setField(nil);
end;

(*
procedure TSDVFieldDataLink.UpdateRightToLeft();
var
  IsRightAligned: Boolean;
  AUseRightToLeftAlignment: Boolean;
begin
  if (Assigned(FControl) and (FControl is TWinControl)) then
  begin
    with FControl as TWinControl do
      if (IsRightToLeft) then
      begin
        IsRightAligned :=
          (GetWindowLong(Handle, GWL_EXSTYLE) and WS_EX_RIGHT) = WS_EX_RIGHT;
        AUseRightToLeftAlignment :=
          DBUseRightToLeftAlignment(TControl(FControl), Field);
        if (IsRightAligned and (not AUseRightToLeftAlignment)) or
           ((not IsRightAligned) and AUseRightToLeftAlignment) then
          Perform(CM_RECREATEWND, 0, 0);
      end;
  end;
end;
*)

procedure TSDVFieldDataLink.ActiveChanged();
begin
  UpdateField();
  if (Assigned(FOnActiveChange))
    then FOnActiveChange(Self);
end;

procedure TSDVFieldDataLink.EditingChanged();
var CanEdit: Boolean;
begin
  CanEdit := (inherited Editing and CanModify);
  setEditing(CanEdit);
end;

procedure TSDVFieldDataLink.FocusControl(Field: TFieldRef);
var AControl: TWinControl;
begin  
  if (FControl is TWinControl) then
  begin
    if ((Field^ <> nil) and (Field^ = FField)) then
    begin
      AControl := (FControl as TWinControl);
      if (AControl.CanFocus()) then
      begin
        Field^ := nil;
        AControl.SetFocus();
      end;
    end;
  end;  
end;

procedure TSDVFieldDataLink.LayoutChanged();
begin
  UpdateField();
end;

procedure TSDVFieldDataLink.RecordChanged(Field: TField);
begin
  if ((Field = nil) or (Field = FField)) then
  begin
    if (Assigned(FOnDataChange))
      then FOnDataChange(Self);
    FModified := false;
  end;
end;

procedure TSDVFieldDataLink.UpdateData();
begin
  if (FModified) then
  begin
    if ((Field <> nil) and Assigned(FOnUpdateData))
      then FOnUpdateData(Self);
    FModified := false;
  end;
end;

constructor TSDVFieldDataLink.Create;
begin
  inherited Create;
  VisualControl := true;
end;

function TSDVFieldDataLink.Edit(): Boolean;
begin
  if (CanModify)
    then inherited Edit();
  Result := FEditing;
end;

procedure TSDVFieldDataLink.Modified();
begin
  FModified := true;
end;

procedure TSDVFieldDataLink.Reset();
begin
  RecordChanged(nil);
end;

end.
