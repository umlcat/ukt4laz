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

unit uktcomboctrls;

interface
uses
(*.IFDEF MSWINDOWS*)
  //Windows,
  //Messages,
  Graphics,
  Controls,
  Forms,
  ExtCtrls,
  Buttons,
(*.ENDIF*)
  SysUtils,
  Classes,
  //Types,
  ActnList,
{$IFDEF FPC}
  LResources,
{$ENDIF}
  uktpanels,
  uktspeedbtns,
  dummy;

const
  MainButtonWidth  = 22;
  MainButtonHeight = 22;

  ComboButtonWidth  = 13;
  ComboButtonHeight = 22;

//  TestArea = 5;
  TestArea = 0;

type
  TSDVOrientation = (doCustom, doTop, doBottom, doLeft, doRight);
  TSDVDirection   = (ddDown, ddUp, ddLeft, ddRight);
  TSDVPosition    = (dpTop, dpBottom, dpLeft, dpRight);

(* TCustomSDVComboControl *)

  TSDVComboControlSpeedButton = class;

  TCustomSDVComboControl = class(TCustomSDVPanel)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)

    FOrientation: TSDVOrientation;
    FDirection:   TSDVDirection;
    FPosition:    TSDVPosition;

    FComboButton: TSDVComboControlSpeedButton;
  protected
    (* Protected declarations *)

    function getDirection(): TSDVDirection;
    function getOrientation(): TSDVOrientation;
    function getPosition(): TSDVPosition;

    procedure setDirection(AValue: TSDVDirection);
    procedure setOrientation(AValue: TSDVOrientation);
    procedure setPosition(AValue: TSDVPosition);
  protected
    (* Protected declarations *)

    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;

    procedure DoComboButtonOnClick; virtual; abstract;

    procedure LoadButtonImage(const ResName: string);
    procedure AssignOrientation();

    procedure CreateControls(); virtual; abstract;
    procedure DestroyControls(); virtual; abstract;
    procedure ReCreateControls(); virtual;

    procedure DelegateOnResize();
    procedure Resize(); override;

    procedure ComboButtonDelegateOnClick(Sender: TObject);
  public
    (* Public declarations *)

    constructor Create(AOwner: TComponent); override;
  public
    (* Public declarations *)

    property Direction: TSDVDirection
       read getDirection write setDirection;
    property Orientation: TSDVOrientation
       read getOrientation write setOrientation;
    property Position: TSDVPosition
       read getPosition write setPosition;
  end;

(* TSDVComboControlSpeedButton *)

  TSDVComboControlSpeedButton = class(TCustomSDVSpeedButton)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)

    FComboControl: TCustomSDVComboControl;
  public
    (* Public declarations *)

    property ComboControl: TCustomSDVComboControl
      read FComboControl write FComboControl;
  end;

implementation

{$IFDEF DELPHI}
{$R 'sdvcomboctrlsres.dcr'}
{$ENDIF}

(* TCustomSDVComboControl *)

procedure TCustomSDVComboControl.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin
//
end;

procedure TCustomSDVComboControl.LoadButtonImage(const ResName: string);
begin
  FComboButton.Glyph.LoadFromResourceName(HInstance, ResName);
  FComboButton.NumGlyphs := 1;
  FComboButton.Enabled := false;
  FComboButton.Enabled := true;
end;

procedure TCustomSDVComboControl.AssignOrientation();
begin
  case (FOrientation) of
    doTop:
    begin
      FPosition  := dpRight; // drop button
      FDirection := ddDown;  // drop button*s image
    end;
    doBottom:
    begin
      FPosition  := dpRight; // drop button
      FDirection := ddUp;  // drop button*s image
    end;
    doLeft:
    begin
      FPosition  := dpTop;   // drop button
      FDirection := ddRight; // drop button*s image
    end;
    doRight:
    begin
      FPosition  := dpTop;  // drop button
      FDirection := ddLeft; // drop button*s image
    end;
    // doCustom:
    else (*Nothing*);
  end;
end;

procedure TCustomSDVComboControl.ReCreateControls();
begin
  DestroyControls();
  CreateControls();
end;

procedure TCustomSDVComboControl.DelegateOnResize();
begin
  if (OnResize <> nil) then
  begin
    OnResize(Self);
  end;
end;

procedure TCustomSDVComboControl.Resize();
begin
  DelegateOnResize();
end;

procedure TCustomSDVComboControl.ComboButtonDelegateOnClick(Sender: TObject);
begin
  DoComboButtonOnClick();
end;

function TCustomSDVComboControl.getDirection(): TSDVDirection;
begin
  Result := FDirection;
end;

function TCustomSDVComboControl.getOrientation(): TSDVOrientation;
begin
  Result := FOrientation;
  // Goal: "Orientation" property get method.
  // Objetivo: Metodo lectura para propiedad "Orientation".
end;

function TCustomSDVComboControl.getPosition(): TSDVPosition;
begin
  Result := FPosition;
end;

procedure TCustomSDVComboControl.setDirection(AValue: TSDVDirection);
begin
  if (FOrientation = doCustom) then
  begin
    FDirection := AValue;
    ReCreateControls();
  end;
end;

procedure TCustomSDVComboControl.setOrientation(AValue: TSDVOrientation);
begin
  FOrientation := AValue;
  ReCreateControls();
  // Goal: "Orientation" property set method.
  // Objetivo: Metodo escritura para propiedad "Orientation".
end;

procedure TCustomSDVComboControl.setPosition(AValue: TSDVPosition);
begin
  if (FOrientation = doCustom) then
  begin
    FPosition := AValue;
    ReCreateControls();
  end;
end;

constructor TCustomSDVComboControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ControlStyle :=
    ControlStyle - [csAcceptsControls, csSetCaption] + [csOpaque];
  if (not NewStyleControls)
    then ControlStyle := ControlStyle + [csFramed];

  BevelOuter := bvNone;
  BevelInner := bvNone;

  FOrientation := doTop; // full control orientation
  FDirection   := ddDown;   // drop button*s image
  FPosition    := dpRight;  // drop button

  // Goal: To prepare the control.
end;


initialization
  {$IFDEF FPC}
  {$I 'uktcomboctrlsres.lrs'}
  {$ENDIF}
end.
