unit uktheaderpanels;

interface
uses
  Windows,
  Messages, SysUtils, Classes, Controls,
  Forms, Graphics, Stdctrls, ExtCtrls,
{$IFDEF FPC}
  LResources,
{$ENDIF}
  uktpanels, ukttogglespeedbtns,
  dummy;

type

(* TCustomSDVHeaderPanel *)

  TCustomSDVHeaderPanel = class(TCustomSDVPanel)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)

    FHeaderPanel:   TCustomSDVPanel;
    FContentPanel:  TCustomSDVPanel;
    FHeaderCaption: TStaticText;
    FButton:        TSDVToggledSpeedButton;
  protected
    (* Protected declarations *)

    procedure FButtonDelegateOnClick(Sender: TObject);
  protected
    (* Protected declarations *)

    function getText(): string; override;
    procedure setText(const Value: string); override;
  public
    (* Public declarations *)

    constructor Create(AOwner: TComponent); override;
  public
    (* Public declarations *)

    procedure ActivateFirst(); override;
  end;

(* TSDVHeaderPanel *)

  TSDVHeaderPanel = class(TCustomSDVHeaderPanel)
  published
    (* Published declarations *)

     (* TCustomPanel: *)

//     property Align;
//     property Alignment;
     property Anchors;
     property AutoSize;
     property BevelInner;
     property BevelOuter;
     property BevelWidth;
     property BiDiMode;
     property BorderWidth;
     property BorderStyle;
     property Caption;
//     property Color;
     property Constraints;
     {$IFDEF DELPHI}
     property Ctl3D;
     {$ENDIF}
     property UseDockManager;
     property DockSite;
     property DragCursor;
     property DragKind;
     property DragMode;
     property Enabled;
     property FullRepaint;
     property Font;
     //property Locked;
     property ParentBiDiMode;
//     property ParentColor;
     {$IFDEF DELPHI}
     property ParentCtl3D;
     {$ENDIF}
     property ParentFont;
     property ParentShowHint;
     property PopupMenu;
     property ShowHint;
     property TabOrder;
     property TabStop;
     property Visible;

     //property OnCanResize;
     property OnClick;
     property OnConstrainedResize;
     property OnContextPopup;
     property OnDockDrop;
     property OnDockOver;
     property OnDblClick;
     property OnDragDrop;
     property OnDragOver;
     property OnEndDock;
     property OnEndDrag;
     property OnEnter;
     property OnExit;
     property OnGetSiteInfo;
     property OnMouseDown;
     property OnMouseMove;
     property OnMouseUp;
     property OnResize;
     property OnStartDock;
     property OnStartDrag;
     property OnUnDock;

     (* TCustomSDVPanel: *)

    property ShowGrid;

    property OnChange;

     (* TCustomSDVHeaderPanel: *)

//     property ColorBegin;
//     property ColorEnd;
  end;

implementation

{$IFDEF DELPHI}
{$R 'sdvheaderpanelsres.dcr'}
{$ENDIF}

(* TCustomSDVHeaderPanel *)

procedure TCustomSDVHeaderPanel.ActivateFirst;
begin
  inherited;
  FButton.Activated;
end;

constructor TCustomSDVHeaderPanel.Create(AOwner: TComponent);
begin
  inherited;

  Self.Height :=  97;
  Self.Width  := 187;

  BevelInner := bvNone;
  BevelOuter := bvLowered;

  FHeaderPanel := TCustomSDVPanel.Create(Self);
    FHeaderPanel.Height := 23;
    FHeaderPanel.Align  := alTop;
  Self.InsertControl(FHeaderPanel);

  FHeaderCaption := TStaticText.Create(Self);
    FHeaderCaption.Left := 24;
    FHeaderCaption.Top  := 4;
    FHeaderCaption.Font.Style := [fsBold];
  FHeaderPanel.InsertControl(FHeaderCaption);

  FButton := TSDVToggledSpeedButton.Create(Self);
    FButton.Left := 2;
    FButton.Top  := 3;
    FButton.Width  := 17;
    FButton.Height := 17;

    FButton.OnClick := {$IFNDEF DELPHI}@{$ENDIF}FButtonDelegateOnClick;

    FButton.Toggled := false;
    FButton.CaptionToggled := '';
    FButton.CaptionUnToggled := '';

    {$IFDEF FPC}
    FButton.GlyphToggled.LoadFromLazarusResource('BTNPN_Expand');
    FButton.GlyphUnToggled.LoadFromLazarusResource('BTNPN_Collapse');
    {$ENDIF}
    {$IFDEF DELPHI}
    FButton.GlyphToggled.LoadFromResourceName(HInstance, 'BTNPN_Expand');
    FButton.GlyphUnToggled.LoadFromResourceName(HInstance, 'BTNPN_Collapse');
    {$ENDIF}

    FButton.NumGlyphs := 1;
    FButton.Enabled := false;
    FButton.Enabled := true;
  FHeaderPanel.InsertControl(FButton);

  FContentPanel := TCustomSDVPanel.Create(Self);
    FContentPanel.Height := 23;
    FContentPanel.Align  := alClient;
    FContentPanel.BevelInner := bvNone;
    FContentPanel.BevelOuter := bvNone;
    FContentPanel.Color := clAppWorkSpace;
  Self.InsertControl(FContentPanel);

  Self.Text := 'NoName';
//  Color :=
end;

procedure TCustomSDVHeaderPanel.FButtonDelegateOnClick(Sender: TObject);
var Extra: Integer;
begin
  if (FButton.Toggled) then
  begin
    Extra := FHeaderPanel.BevelWidth * 2;
    Self.Height := FHeaderPanel.Height + Extra;
  end else Self.Height := 97;
end;

function TCustomSDVHeaderPanel.getText(): string;
begin
  Result := InternalText;
end;

procedure TCustomSDVHeaderPanel.setText(const Value: string);
begin
  inherited;
  InternalText := Value;
  FHeaderCaption.Caption := Value;
end;

initialization
{$IFDEF FPC}
{$I 'uktheaderpanelsres.lrs'}
{$ENDIF}

end.
