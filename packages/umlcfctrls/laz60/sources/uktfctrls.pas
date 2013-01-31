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
unit uktfctrls;

interface
uses
{.IFDEF MSWINDOWS}
  Controls, Forms,
  Graphics, StdCtrls,
{.ENDIF}
  SysUtils, Classes,
  uktKeyConsts,
  uktcomponents,
  uktguidstrs,
  uktmsgtypes, uktmsgctrls,
  //uktMsgLists,
{.IFDEF MSWINDOWS}
  uktfmngrs,
  uktedits,
  uktfedits,
{.ENDIF}
  dummy;

type

(* TCustomSDVFocusIntegerEdit *)

  TCustomSDVFocusIntegerEdit = class(TCustomSDVFocusEdit)
  private
    (* Private declarations *)

    FAsValue:  Integer;
    FMaxValue: Integer;

    FSigned:  Boolean;
    FHasSign: Boolean;
  protected
    (* Protected declarations *)
  public
    (* Public declarations *)

    constructor Create(AOwner: TComponent); override;
    destructor Destroy();  override;

    procedure KeyPress(var AKey: Char); override;
    function ValidChar(var AKey: Char): Boolean;

    (* Unpublished declarations *)

    property AsValue: Integer read FAsValue write FAsValue;
    property MaxValue: Integer read FMaxValue write FMaxValue default 0;
    property Signed: Boolean read FSigned write FSigned default FALSE;
  end;

{ TCustomSDVFocusFloatEdit }

  TCustomSDVFocusFloatEdit = class(TCustomSDVFocusEdit)
  private
    (* Private declarations *)

    FAsValue: Extended;
    FPrecision: shortInt;

    FDot: Boolean;
    FCount: shortInt;

    FSigned: Boolean;
    FHasSign: Boolean;

    procedure setPrecision(AValue: shortInt);
  protected
    (* Protected declarations *)
  public
    (* Public declarations *)

    constructor Create(AOwner: TComponent); override;
    destructor Destroy();  override;

    procedure KeyPress(var AKey: Char); override;
    function ValidChar(var AKey: Char): Boolean;

    procedure DoExit(); override;

    (* Published declarations *)

    property AsValue: Extended read FAsValue write FAsValue;
    property Presicion: shortInt read FPrecision write setPrecision;
    property Signed: boolean read FSigned write FSigned;    
  end;

(* TSDVFocusIntegerEdit *)

  TSDVFocusIntegerEdit = class(TCustomSDVFocusIntegerEdit)
  published
    (* Published declarations *)

    (* TCustomEdit: *)

 {.IFDEF MSWINDOWS}
    property BiDiMode;

    {$IFDEF DELPHI}
    property Ctl3D;
    property ImeMode;
    property ImeName;
    property OEMConvert;
    {$ENDIF}

    property DragCursor;
    property DragKind;
    property ParentBiDiMode;
    property PasswordChar;

    property OnEndDock;
    property OnStartDock;
{.ENDIF}

//    property AutoSelect;
//    property CharCase;
//    property Color;
//    property Font;
//    property ParentColor;
//    property ParentFont;

    {$IFDEF DELPHI}
    property ParentCtl3D;
    {$ENDIF}

    property Anchors;
    property AutoSize;
    property BorderStyle;
    property Constraints;
    property DragMode;
    property Enabled;
    property HideSelection;
    property MaxLength;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;

    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;

    (* TCustomSDVFocusIntegerEdit: *)

    property Alignment;
    property Manager;

    property AsValue;
    property MaxValue;
    property Signed;
  end;
  { Goal: To change color when focused ;}
  { Objetivo: Cambiar el color al enfocarse ;}

(* TSDVFocusFloatEdit *)

  TSDVFocusFloatEdit = class(TCustomSDVFocusFloatEdit)
  published
    (* Published declarations *)

    (* TCustomEdit: *)

{.IFDEF MSWINDOWS}
    property BiDiMode;

    {$IFDEF DELPHI}
    property Ctl3D;
    property ImeMode;
    property ImeName;
    property OEMConvert;
    {$ENDIF}

    property DragCursor;
    property DragKind;
    property ParentBiDiMode;
    property PasswordChar;

    property OnEndDock;
    property OnStartDock;
{.ENDIF}

//    property AutoSelect;
//    property CharCase;
//    property Color;
//    property Font;
//    property ParentColor;
//    property ParentFont;

    {$IFDEF DELPHI}
    property ParentCtl3D;
    {$ENDIF}

    property Anchors;
    property AutoSize;
    property BorderStyle;
    property Constraints;
    property DragMode;
    property Enabled;
    property HideSelection;
    property MaxLength;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;

    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;

    (* TCustomSDVFocusIntegerEdit: *)

    property Alignment;
    property Manager;

    property AsValue;
    property Presicion;
    property Signed;
  end;
  { Goal: To change color when focused ;}
  { Objetivo: Cambiar el color al enfocarse ;}

implementation

(* TCustomSDVFocusIntegerEdit *)

constructor TCustomSDVFocusIntegerEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMaxValue := 0;
  FSigned   := FALSE;
  FHasSign  := FALSE;
end;

destructor TCustomSDVFocusIntegerEdit.Destroy();
begin
  inherited Destroy();
end;

procedure TCustomSDVFocusIntegerEdit.KeyPress(var AKey: Char);
begin
  if (ValidChar(AKey)) then
  begin
    if ((Word(AKey) <> VK_DELPREVCHAR) and (Word(AKey) <> VK_ENTER)) then
      if ((Text = '') and (AKey = '-'))
        then FAsValue := 0
        else FAsValue := StrToInt(Text + AKey);
      if ((FAsValue > FMaxValue) and (FMaxValue > 0))
        then AKey := char(VK_NONE)
        else inherited KeyPress(AKey);
  end else AKey := char(VK_NONE);
end;

function TCustomSDVFocusIntegerEdit.ValidChar(var AKey: Char): Boolean;
begin
  case (AKey) of
    '0'..'9',
    char(VK_ENTER): Result := TRUE;
    '-':
    begin
      Result := FALSE;
      if (not FHasSign and FSigned) then
        if (Length(Text) = 0) then
        begin
          Result := TRUE;
          FHasSign := TRUE;
        end;
    end;
    char(VK_DELPREVCHAR):
    begin
      if (Length(Text) = 1)
        then FHasSign := FALSE;
      Result := TRUE;
    end;
    else Result := FALSE;
  end;
end;

{ TCustomSDVFocusFloatEdit }

procedure TCustomSDVFocusFloatEdit.setPrecision(AValue: shortInt);
begin
  if (FPrecision <> AValue)
    then FPrecision := AValue;
end;

constructor TCustomSDVFocusFloatEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDot     := FALSE;
  FCount   := 0;
  FSigned  := FALSE;
  FHasSign := FALSE;
end;

destructor TCustomSDVFocusFloatEdit.Destroy();
begin
  inherited Destroy();
end;

procedure TCustomSDVFocusFloatEdit.KeyPress(var AKey: Char);
var Dec: string; I: Byte;
begin
  if ValidChar(AKey) then
  begin
    if (Word(AKey) <> 8) and (Word(AKey) <> 13)then
    begin
      Dec := '0';
      if (FPrecision > 0) then
      begin
        Dec := '.';
        for I := 1 to FPrecision do
          Dec := Dec + '0';
      end;
      if (Text = '') and (AKey = '-') then
        FAsValue := 0
      else
        FAsValue := StrToFloat(FormatFloat(Dec,StrToFloat(Text + AKey)));
    end;
    inherited KeyPress(AKey);
  end else AKey := #0;
end;

function TCustomSDVFocusFloatEdit.ValidChar(var AKey: Char): Boolean;
begin
  case AKey of
    '0'..'9':
    begin
      if (FDot) then
      begin
        Inc(FCount);
        if (FCount <= FPrecision) then
        begin
          Result := TRUE
        end else
        begin
          FCount := FPrecision;
          Result := FALSE;
        end;
      end
      else
        Result :=
          ((MaxLength > 0) and
           (Length(Text + AKey) > (MaxLength - FPrecision - 1)));
    end;
    char(VK_DELPREVCHAR):
    begin
      Result := TRUE;
      Dec(FCount);
      if (FCount < 0) then
      begin
        FDot     := FALSE;
        FHasSign := FALSE;
        FCount   := 0;
      end;
    end;
    '.':
    begin
      if (not FDot) then
      begin
        FDot := FPrecision > 0;
        Result := FDot;
      end
      else
        Result:= FALSE;
    end;
    '-':
    begin
      Result := FALSE;
      if (not FHasSign and FSigned) then
      begin
        if (Length(Text) = 0) then
        begin
          Result := TRUE;
          FHasSign := TRUE;
        end;
      end;
    end;
    char(VK_ENTER): Result := TRUE;
    else Result := FALSE;
  end;
end;

procedure TCustomSDVFocusFloatEdit.DoExit();
var P, I, Last: Byte;
begin
  if (FPrecision > 0) then
  begin
    P := Pos('.',Text);
    if (P > 0) then
    begin
      if (Length(Copy(Text,P+1,FPrecision)) < FPrecision) then
      begin
        Last := (FPrecision - Length(Copy(Text,P+1,FPrecision)));
        for I := 1 to Last do
        begin
          Text := Text + '0';
        end;
      end;
    end else
    begin
      Text := Text + '.';
      for I := 1 to FPrecision do
      begin
        Text := Text + '0';
      end;
    end;
  end;
  inherited DoExit();
end;

procedure Prepare();
begin
  // ...
end;

procedure UnPrepare();
begin
  // ...
end;

initialization
  Prepare();
finalization
  Unprepare();
end.

