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

unit ukttogglespeedbtns;

interface
uses
  SysUtils,
  Classes,
(*.IFDEF MSWINDOWS*)
  //Windows,
  //Messages,
  Graphics,
  Controls, 
  //StdCtrls,
  Forms,
  Buttons,
(*.ENDIF*)
  ukttogglebuttons,
  uktspeedbtns,
  dummy;

type

(* TCustomSDVToggledSpeedButton *)

  TCustomSDVToggledSpeedButton = class(TCustomSDVSpeedButton, ISDVToggledButton)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)

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
  end;

(* TSDVToggledSpeedButton *)

  TSDVToggledSpeedButton = class(TCustomSDVToggledSpeedButton)
  published
    (* Published declarations *)

    (* TCustomSDVToggledSpeedButton: *)

    property Toggled;

    property GlyphToggled;
    property GlyphUnToggled;

    property CaptionToggled;
    property CaptionUnToggled;
  end;

implementation

(* TCustomSDVToggledSpeedButton *)

function TCustomSDVToggledSpeedButton.getToggled: Boolean;
begin
  Result := FToggled;
end;

function TCustomSDVToggledSpeedButton.getGlyphToggled: TBitmap;
begin
  Result := FGlyphs[true];
end;

function TCustomSDVToggledSpeedButton.getGlyphUnToggled: TBitmap;
begin
  Result := FGlyphs[false];
end;

function TCustomSDVToggledSpeedButton.getCaptionToggled: string;
begin
  Result := FCaptions[true];
end;

function TCustomSDVToggledSpeedButton.getCaptionUnToggled: string;
begin
  Result := FCaptions[false];
end;

procedure TCustomSDVToggledSpeedButton.setToggled(const Value: Boolean);
begin
  if (FToggled <> Value) then
  begin
    FToggled := Value;
    RefreshButton();
  end;
end;

procedure TCustomSDVToggledSpeedButton.RefreshButton();
begin
  Glyph.Assign(FGlyphs[FToggled]);
  Caption := FCaptions[FToggled];
end;

procedure TCustomSDVToggledSpeedButton.setCaptionToggled(const Value: string);
begin
  if (FCaptions[true] <> Value) then
  begin
    FCaptions[true] := Value;
    RefreshButton();
  end;
end;

procedure TCustomSDVToggledSpeedButton.setCaptionUnToggled(const Value: string);
begin
  if (FCaptions[false] <> Value) then
  begin
    FCaptions[false] := Value;
    RefreshButton();
  end;
end;

procedure TCustomSDVToggledSpeedButton.setGlyphToggled(const Value: TBitmap);
begin
  if (FGlyphs[true] <> Value) then
  begin
    FGlyphs[true].Assign(Value);
    RefreshButton();
  end;
end;

procedure TCustomSDVToggledSpeedButton.setGlyphUnToggled(const Value: TBitmap);
begin
  if (FGlyphs[false] <> Value) then
  begin
    FGlyphs[false].Assign(Value);
    RefreshButton();
  end;
end;

procedure TCustomSDVToggledSpeedButton.Click;
begin
  Toggled := not Toggled;
  inherited Click;
end;

constructor TCustomSDVToggledSpeedButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FToggled := false;

  FGlyphs[false] := TBitmap.Create;
  FGlyphs[true]  := TBitmap.Create;

  FCaptions[false] := 'UnToggled';
  FCaptions[true]  := 'Toggled';
end;

destructor TCustomSDVToggledSpeedButton.Destroy;
begin
  FCaptions[true]  := '';
  FCaptions[false] := '';

  inherited Destroy;
end;

procedure TCustomSDVToggledSpeedButton.Activated;
begin
  RefreshButton();
end;

end.
