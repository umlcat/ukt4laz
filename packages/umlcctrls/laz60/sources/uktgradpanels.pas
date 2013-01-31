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

unit uktgradpanels;

interface
uses
  //Windows,
  Graphics,
  SysUtils, Classes, Controls,
  Forms,
  ExtCtrls,
  Math,
  uktpanels,
  dummy;

type

(* TCustomSDVGradientPanel *)

  TCustomSDVGradientPanel = class(TCustomSDVPanel)
  private
    (* private declarations *)

    FColors: array[0..255] of TColor;

    procedure AutoInitialize();
  protected
    (* protected declarations *)

    FColorBegin,
    FColorEnd: TColor;
  protected
    (* protected declarations *)

    function getColorBegin(): TColor;
    function getColorEnd(): TColor;

    procedure setColorBegin(const AValue: TColor);
    procedure setColorEnd(const AValue: TColor);
  protected
    (* protected declarations *)

    FNumberOfColors: Integer;

    procedure GradientFill2
      (ACanvas: TCanvas; AWidth, AHeight: Integer);
    procedure CalculateColor();

    procedure Paint(); override;
  public
    (* public declarations *)

    constructor Create(AOwner: TComponent); override;
  public
    (* public declarations *)

    property ColorBegin: TColor
      read getColorBegin write setColorBegin;
    property ColorEnd: TColor
      read getColorEnd write setColorEnd;
    property UseDockManager
      default TRUE;
  end;

(* TSDVGradientPanel *)

  TSDVGradientPanel = class(TCustomSDVGradientPanel)
  published
    (* published declarations *)

     (* TCustomPanel: *)

     property Align;
     property Alignment;
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
//     property Ctl3D;
     property UseDockManager;
     property DockSite;
     property DragCursor;
     property DragKind;
     property DragMode;
     property Enabled;
     property FullRepaint;
     property Font;
//     property Locked;
     property ParentBiDiMode;
//     property ParentColor;
//     property ParentCtl3D;
     property ParentFont;
     property ParentShowHint;
     property PopupMenu;
     property ShowHint;
     property TabOrder;
     property TabStop;
     property Visible;

//     property OnCanResize;
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

     (* TCustomSDVGradientPanel: *)

     property ColorBegin;
     property ColorEnd;
  end;

  function MulDiv
    (nNumber:longint; nNumerator:longint; nDenominator:longint):longint;
      external 'kernel32' name 'MulDiv';

implementation

(* TCustomSDVGradientPanel *)

function TCustomSDVGradientPanel.getColorBegin: TColor;
begin
  Result := FColorBegin;
end;

function TCustomSDVGradientPanel.getColorEnd: TColor;
begin
  Result := FColorEnd;
end;

procedure TCustomSDVGradientPanel.setColorBegin(const AValue: TColor);
begin
  if (FColorBegin <> AValue) then
  begin
    FColorBegin := AValue;
    Invalidate;
  end;
end;

procedure TCustomSDVGradientPanel.setColorEnd(const AValue: TColor);
begin
  if (FColorEnd <> AValue) then
  begin
    FColorEnd := AValue;
    Invalidate;
  end;
end;

// Method to set variable and property Values and create objects.
procedure TCustomSDVGradientPanel.AutoInitialize();
begin
  FShowGrid := FALSE;
  Height := 121;
  Width  := 377;
  FColorBegin := clBtnShadow;
  FColorEnd := clSilver;
  FNumberOfColors := 255;
end;

procedure TCustomSDVGradientPanel.GradientFill2
  (ACanvas: TCanvas; AWidth, AHeight: Integer);
var Rc: TRect; I: Integer; TempBitmap: TBitmap;
begin
  CalculateColor;
  TempBitmap := TBitmap.Create;
  TempBitmap.Width := AWidth;
  TempBitmap.Height := AHeight;

  Rc.Top := 0;
  Rc.Bottom := AHeight;

  with TempBitmap do
  for I := 0 to FNumberOfColors do
  begin
    Rc.Left  := MulDiv(I, Width, FNumberOfColors);
    Rc.Right := MulDiv(I + 1, Width, FNumberOfColors);
    ACanvas.Brush.Color := FColors[i];
    ACanvas.FillRect(Rc);
  end;
  Canvas.Draw(0, 0, TempBitmap);
  TempBitmap.Free;
end;

procedure TCustomSDVGradientPanel.CalculateColor();
var
  beginRGB: array[0..2] of Byte;
  RGBDifference: array[0..2] of Integer;
  R: Byte;
  G: Byte;
  B: Byte;
  I: Byte;
begin
  beginRGB[0] := Red(ColorToRGB(FColorBegin));
  beginRGB[1] := Green(ColorToRGB(FColorBegin));
  beginRGB[2] := Blue(ColorToRGB(FColorBegin));

  RGBDifference[0] := Red(ColorToRGB(FColorEnd)) - beginRGB[0];
  RGBDifference[1] := Green(ColorToRGB(FColorEnd)) - beginRGB[1];
  RGBDifference[2] := Blue(ColorToRGB(FColorEnd)) - beginRGB[2];

  for i := 0 to 255 do
  begin
    R := beginRGB[0] + MulDiv(I, RGBDifference[0], FNumberOfColors - 1);
    G := beginRGB[1] + MulDiv(I, RGBDifference[1], FNumberOfColors - 1);
    B := beginRGB[2] + MulDiv(I, RGBDifference[2], FNumberOfColors - 1);
    FColors[i] := RGBToColor(R, G, B);
  end;

  (*
  beginRGB[0] := GetRValue(ColorToRGB (FColorBegin));
  beginRGB[1] := GetGValue(ColorToRGB (FColorBegin));
  beginRGB[2] := GetBValue(ColorToRGB (FColorBegin));

  RGBDifference[0] := GetRValue(ColorToRGB (FColorEnd)) - beginRGB[0];
  RGBDifference[1] := GetGValue(ColorToRGB (FColorEnd)) - beginRGB[1];
  RGBDifference[2] := GetBValue(ColorToRGB (FColorEnd)) - beginRGB[2];

  for i := 0 to 255 do
  begin
    R := beginRGB[0] + MulDiv(I, RGBDifference[0], FNumberOfColors - 1);
    G := beginRGB[1] + MulDiv(I, RGBDifference[1], FNumberOfColors - 1);
    B := beginRGB[2] + MulDiv(I, RGBDifference[2], FNumberOfColors - 1);
    FColors[i] := RGB(R, G, B);
  end;
  *)
end;

procedure TCustomSDVGradientPanel.Paint();
var FontHeight: Integer;
   x, y: Integer;
begin
  GradientFill2(Canvas, Width, Height);
  FontHeight := Canvas.TextHeight('W');
  Canvas.Brush.Style := bsClear;
  Canvas.Font := Self.Font;
  x := 4;
  y := (Height - FontHeight) shr 1;
  Canvas.TextOut(X, Y, Caption);
end;

constructor TCustomSDVGradientPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AutoInitialize;
  // Code to perform other tasks when the component is created.
end;

end.
