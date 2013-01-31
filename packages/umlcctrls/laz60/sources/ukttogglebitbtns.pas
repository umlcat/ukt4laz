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

unit ukttogglebitbtns;

interface
uses
(*.IFDEF MSWINDOWS*)
  //Windows,
  //Messages,
  Graphics,
  Controls, 
  //StdCtrls,
  Forms, Buttons,
(*.ENDIF*)
  SysUtils, 
  Classes,
  ukttogglebuttons,
  uktbitbtns;

type

(* TCustomSDVToggledBitBtn *)

  TCustomSDVToggledBitBtn = class(TCustomSDVBitBtn, ISDVToggledButton)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)

    FOnClick: TNotifyEvent;

    FToggled: Boolean;

    FCaptions: array[Boolean] of string;
    FGlyphs:   array[Boolean] of TBitmap;

    function getCaptionToggled: string; virtual;
    function getCaptionUnToggled: string; virtual;

    function getGlyphToggled: TBitmap; virtual;
    function getGlyphUnToggled: TBitmap; virtual;

    function getToggled: Boolean; virtual;

    procedure setCaptionToggled(const Value: string); virtual;
    procedure setCaptionUnToggled(const Value: string); virtual;

    procedure setGlyphToggled(const Value: TBitmap); virtual;
    procedure setGlyphUnToggled(const Value: TBitmap); virtual;

    procedure setToggled(const Value: Boolean); virtual;

    procedure DelegateOnClick;
    procedure RefreshButton();
  public
    (* Public declarations *)

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Activated;
    procedure Click; override;

    property Toggled: Boolean
      read getToggled write setToggled;

    property GlyphToggled: TBitmap
      read getGlyphToggled write setGlyphToggled;
    property GlyphUnToggled: TBitmap
      read getGlyphUnToggled write setGlyphUnToggled;

    property CaptionToggled: string
      read getCaptionToggled write setCaptionToggled;
    property CaptionUnToggled: string
      read getCaptionUnToggled write setCaptionUnToggled;

    property OnClick: TNotifyEvent
      read FOnClick write FOnClick;
  end;

(* TSDVToggledBitBtn *)

  TSDVToggledBitBtn = class(TCustomSDVToggledBitBtn)
  published
    (* Published declarations *)

    (* TCustomSDVToggledBitBtn: *)

    property Toggled;

    property GlyphToggled;
    property GlyphUnToggled;

    property CaptionToggled;
    property CaptionUnToggled;

    property OnClick;
  end;

implementation

(* TCustomSDVToggledBitBtn *)

function TCustomSDVToggledBitBtn.getToggled: Boolean;
begin
  Result := FToggled;
end;

function TCustomSDVToggledBitBtn.getGlyphToggled: TBitmap;
begin
  Result := FGlyphs[true];
end;

function TCustomSDVToggledBitBtn.getGlyphUnToggled: TBitmap;
begin
  Result := FGlyphs[false];
end;

function TCustomSDVToggledBitBtn.getCaptionToggled: string;
begin
  Result := FCaptions[true];
end;

function TCustomSDVToggledBitBtn.getCaptionUnToggled: string;
begin
  Result := FCaptions[false];
end;

procedure TCustomSDVToggledBitBtn.setToggled(const Value: Boolean);
begin
  if (FToggled <> Value) then
  begin
    FToggled := Value;
    RefreshButton();
  end;
end;

procedure TCustomSDVToggledBitBtn.DelegateOnClick;
begin
  if (Assigned(FOnClick))
    then FOnClick(Self);
end;

procedure TCustomSDVToggledBitBtn.RefreshButton();
begin
  Glyph.Assign(FGlyphs[FToggled]);
  Caption := FCaptions[FToggled];
end;

procedure TCustomSDVToggledBitBtn.setCaptionToggled(const Value: string);
begin
  if (FCaptions[true] <> Value) then
  begin
    FCaptions[true] := Value;
    RefreshButton();
  end;
end;

procedure TCustomSDVToggledBitBtn.setCaptionUnToggled(const Value: string);
begin
  if (FCaptions[false] <> Value) then
  begin
    FCaptions[false] := Value;
    RefreshButton();
  end;
end;

procedure TCustomSDVToggledBitBtn.setGlyphToggled(const Value: TBitmap);
begin
  if (FGlyphs[true] <> Value) then
  begin
    FGlyphs[true].Assign(Value);
    RefreshButton();
  end;
end;

procedure TCustomSDVToggledBitBtn.setGlyphUnToggled(const Value: TBitmap);
begin
  if (FGlyphs[false] <> Value) then
  begin
    FGlyphs[false].Assign(Value);
    RefreshButton();
  end;
end;

procedure TCustomSDVToggledBitBtn.Click;
begin
  Toggled := not Toggled;
  DelegateOnClick;
end;

constructor TCustomSDVToggledBitBtn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FToggled := false;

  FGlyphs[false] := TBitmap.Create;
  FGlyphs[true]  := TBitmap.Create;

  FCaptions[false] := 'UnToggled';
  FCaptions[true]  := 'Toggled';
end;

destructor TCustomSDVToggledBitBtn.Destroy;
begin
  FCaptions[true]  := '';
  FCaptions[false] := '';

  inherited Destroy;
end;

procedure TCustomSDVToggledBitBtn.Activated;
begin
  RefreshButton();
end;

end.
