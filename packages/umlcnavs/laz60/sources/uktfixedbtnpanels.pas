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

unit uktfixedbtnpanels;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Graphics,
  Controls,
  uktpanels,
  uktnormpanels,
  uktctrlenums,
  ukteventargs,
  uktevargbtns,
  dummy;

const
  defaultButtonSeparator = 4;

type
  TSDVOnCalculateButtonTextDelegateType =
    (* ^ *) function(sender: TObject; e: pointer): string;
  TSDVOnCalculateButtonImageDelegateType =
    (* ^ *) function(sender: TObject; e: pointer): TBitmap;
  TSDVOnChangeButtonFontDelegateType =
    (* ^ *) function(sender: TObject; e: pointer): TFont;

(* TSDVInternalFixedPanelButton *)

  TSDVCustomFixedButtonPanel = class;


  TSDVInternalFixedPanelButton = class(TCustomSDVEventButton)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)

    FButtonMenuPanel: TSDVCustomFixedButtonPanel;
  public
    (* Public declarations *)

    /// <summary>
    /// Stores a quick reference to the container,
    /// may required in some circumstances.
    /// </summary>
    property ButtonMenuPanel: TSDVCustomFixedButtonPanel
      read FButtonMenuPanel write FButtonMenuPanel;
  end;

(* TSDVCustomFixedButtonPanel *)

  TSDVCustomFixedButtonPanel = class(TCustomSDVNormalizedPanel)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)

    FInternalItemsPanel: TCustomSDVNormalizedPanel;
  protected
    (* Protected declarations *)

    FShowButtonsText:    Boolean;
    FShowButtonsImages:  Boolean;
    FTransposeImages:    Boolean;
    FApplyColor:         Boolean;
  protected
    (* Protected declarations *)

    function getShowButtonsText(): Boolean;
    function getShowButtonsImages(): Boolean;
    function getTransposeImages(): Boolean;
    function getApplyColor(): Boolean;

    procedure setShowButtonsText(const AValue: Boolean);
    procedure setShowButtonsImages(const AValue: Boolean);
    procedure setTransposeImages(const AValue: Boolean);
    procedure setApplyColor(const AValue: Boolean);
  protected
    (* Protected declarations *)

    /// <summary>
    /// Almacena un delegado para el evento de asignar el texto,
    /// de el titulo de un boton.
    /// La propiedad <code>ShowButtonsText</code> debe estar habilitada.
    /// </summary>
    FOnCalculateButtonText: TSDVOnCalculateButtonTextDelegateType;

    /// <summary>
    /// Almacena un delegado para el evento de asignar la imagen de fondo de un boton,
    /// cuando el boton NO esta seleccionado.
    /// La propiedad <code>ShowButtonsImage</code> debe estar habilitada.
    /// </summary>
    FOnCalculateButtonImage: TSDVOnCalculateButtonImageDelegateType;

    /// <summary>
    /// Almacena un delegado para el evento de asignar la imagen de fondo de un boton,
    /// cuando el boton SI esta seleccionado.
    /// de un boton. La propiedad <code>ShowButtonsImage</code> debe estar habilitada.
    /// </summary>
    FOnCalculateSelectedButtonImage: TSDVOnCalculateButtonImageDelegateType;

    /// <summary>
    /// Almacena un delegado para el evento de asignar el tipo-fuente para el texto,
    /// de el titulo de un boton.
    /// La propiedad <code>ShowButtonsText</code> debe estar habilitada.
    /// </summary>
    FOnChangeButtonFont: TSDVOnChangeButtonFontDelegateType;
  protected
    (* Protected declarations *)

    function delegateOnCalculateButtonText
      (Sender: TObject; e: pointer): String;

    function delegateOnCalculateButtonImage
      (Sender: TObject; e: pointer): TBitmap;

    function delegateOnCalculateSelectedButtonImage
      (Sender: TObject; e: pointer): TBitmap;

    function delegateOnChangeButtonFont
      (Sender: TObject; e: pointer): TFont;
  protected
    (* Protected declarations *)

    function isAssignedOnCalculateButtonText(): Boolean;
    function isAssignedOnCalculateButtonImage(): Boolean;
    function isAssignedOnCalculateSelectedButtonImage(): Boolean;
    function isAssignedOnChangeButtonFont(): Boolean;

    function selectDefaultButtonText
      (Args: TObject): string; virtual;

    function selectDefaultButtonImage
      (Args: TObject): TBitmap; virtual;

    function selectDefaultSelectedButtonImage
      (Args: TObject): TBitmap; virtual;

    function calculateButtonText
      (Args: TObject): string; virtual;

    function calculateButtonImage
      (Args: TObject): TBitmap; virtual;

    function calculateButtonSelectedImage
      (Args: TObject): TBitmap; virtual;

    function changeButtonFont
      (Args: TObject): TFont; virtual;

    function readDefaultButtonWidth(): Integer; virtual;
    function readDefaultButtonHeight(): Integer; virtual;

    function canRebuiltControl(): Boolean; virtual;
    procedure createContainerControls(); virtual;

    procedure createButtons(); virtual;
    procedure rebuiltControl(); // nonvirtual;

    procedure Resize();
  public
    (* Public declarations *)

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    (* Public declarations *)

    /// <summary>
    /// Regresa con que direccion se ha construido el control,
    /// algunas clases dependientes, dependen de esta propiedad.
    /// Esta propiedad debe actualizarse en cada clase.
    /// </summary>
    /// <returns>Direccion del control</returns>
    function readOrientation(): TSDVControlOrientation; virtual;
  public
    (* Public declarations *)

    property ShowButtonsText: Boolean
      read getShowButtonsText write setShowButtonsText;
    property ShowButtonsImages: Boolean
      read getShowButtonsImages write setShowButtonsImages;
    property TransposeImages: Boolean
      read getTransposeImages write setTransposeImages;
    property ApplyColor: Boolean
      read getApplyColor write setApplyColor;
  end;

implementation

(* TSDVCustomFixedButtonPanel *)

function TSDVCustomFixedButtonPanel.getShowButtonsText: Boolean;
begin
  Result := FShowButtonsText;
end;

function TSDVCustomFixedButtonPanel.getShowButtonsImages: Boolean;
begin
  Result := FShowButtonsImages;
end;

function TSDVCustomFixedButtonPanel.getTransposeImages: Boolean;
begin
  Result := FTransposeImages;
end;

function TSDVCustomFixedButtonPanel.getApplyColor: Boolean;
begin
  Result := FApplyColor;
end;

procedure TSDVCustomFixedButtonPanel.setShowButtonsText
  (const AValue: Boolean);
begin
  FShowButtonsText := AValue;
end;

procedure TSDVCustomFixedButtonPanel.setShowButtonsImages
  (const AValue: Boolean);
begin
  FShowButtonsImages := AValue;
end;

procedure TSDVCustomFixedButtonPanel.setTransposeImages
  (const AValue: Boolean);
begin
  FTransposeImages := AValue;
end;

procedure TSDVCustomFixedButtonPanel.setApplyColor
  (const AValue: Boolean);
begin
  FApplyColor := AValue;
end;

function TSDVCustomFixedButtonPanel.delegateOnCalculateButtonText
  (Sender: TObject; e: pointer): String;
begin
  Result := FOnCalculateButtonText(Self, e);
end;

function TSDVCustomFixedButtonPanel.delegateOnCalculateButtonImage
  (Sender: TObject; e: pointer): TBitmap;
begin
  Result := FOnCalculateButtonImage(Self, e);
end;

function TSDVCustomFixedButtonPanel.delegateOnCalculateSelectedButtonImage
  (Sender: TObject; e: pointer): TBitmap;
begin
  Result := FOnCalculateSelectedButtonImage(Self, e);
end;

function TSDVCustomFixedButtonPanel.delegateOnChangeButtonFont
  (Sender: TObject; e: pointer): TFont;
begin
  Result := FOnChangeButtonFont(Self, e);
end;

function TSDVCustomFixedButtonPanel.isAssignedOnCalculateButtonText(): Boolean;
begin
  Result := (FOnCalculateButtonText <> nil);
end;

function TSDVCustomFixedButtonPanel.isAssignedOnCalculateButtonImage(): Boolean;
begin
  Result := (FOnCalculateButtonImage <> nil);
end;

function TSDVCustomFixedButtonPanel.isAssignedOnCalculateSelectedButtonImage(): Boolean;
begin
  Result := (FOnCalculateSelectedButtonImage <> nil);
end;

function TSDVCustomFixedButtonPanel.isAssignedOnChangeButtonFont(): Boolean;
begin
  Result := (FOnChangeButtonFont <> nil);
end;

/// <summary>
/// Return the default caption for each button,
/// if the control requires a caption.
/// </summary>
/// <param name="a">Arguments of the button</param>
/// <returns>Default caption of the given button</returns>
function TSDVCustomFixedButtonPanel.selectDefaultButtonText
  (Args: TObject): string;
begin
  Result := '';
  Self.DoNothing();
end;

/// <summary>
/// Returns the default embedded image for a button,
/// when is NOT selected, and the <code>ShowButtonsImages</code> property,
/// is enabled.
/// </summary>
/// <param name="a"></param>
/// <returns>Default image of the given non selected button</returns>
function TSDVCustomFixedButtonPanel.selectDefaultButtonImage
  (Args: TObject): TBitmap;
begin
  Result := nil;
  Self.DoNothing();
end;

/// <summary>
/// Returns the default embedded image for a button,
/// when is selected, and the <code>ShowButtonsImages</code> property,
/// is enabled.
/// </summary>
/// <param name="a"></param>
/// <returns>Default image of the given selected button</returns>
function TSDVCustomFixedButtonPanel.selectDefaultSelectedButtonImage
  (Args: TObject): TBitmap;
begin
  Result := nil;
  Self.DoNothing();
end;

/// <summary>
/// Returns the text used for the caption of the button.
/// </summary>
/// <param name="FixedButtonPanelEventArgs">Arguments of a button</param>
/// <returns>Caption of the given button</returns>
function TSDVCustomFixedButtonPanel.calculateButtonText
  (Args: TObject): string;
begin
  Result := '';

  if (Self.ShowButtonsText) then
  begin
    if (isAssignedOnCalculateButtonText()) then
    begin
      Result := delegateOnCalculateButtonText(Self, Args);
    end else
    begin
      Result := selectDefaultButtonText(Args);
    end;
  end; // if (Self.ShowButtonsText)
end;

/// <summary>
/// Returns the bitmap used for the image of the button.
/// When the button is NOT selected.
/// </summary>
/// <param name="FixedButtonPanelEventArgs">Arguments of a button</param>
/// <returns>Image of the given button</returns>
function TSDVCustomFixedButtonPanel.calculateButtonImage
  (Args: TObject): TBitmap;
begin
  Result := nil;

  if (Self.ShowButtonsImages) then
  begin
    if (isAssignedOnCalculateButtonImage()) then
    begin
      Result := delegateOnCalculateButtonImage(Self, Args);
    end else
    begin
       Result := selectDefaultButtonImage(Args);
    end;
  end; // if (this.ShowButtonsImages)
end; // Bitmap calculateButtonImage(...)

/// <summary>
/// Returns the bitmap used for the image of the button.
/// When the button is NOT selected.
/// </summary>
/// <param name="FixedButtonPanelEventArgs">Arguments of a button</param>
/// <returns>Image of the given button</returns>
function TSDVCustomFixedButtonPanel.calculateButtonSelectedImage
  (Args: TObject): TBitmap;
begin
  Result := nil;

  if (Self.ShowButtonsImages) then
  begin
    if (isAssignedOnCalculateSelectedButtonImage()) then
    begin
      Result := delegateOnCalculateSelectedButtonImage(Self, Args);
    end else
    begin
      Result := selectDefaultSelectedButtonImage(Args);
    end;
  end; // if (this.ShowButtonsImages)
end; // Bitmap calculateSelectedButtonImage(...)

/// <summary>
/// Returns the font used for the text of the button.
/// </summary>
/// <param name="FixedButtonPanelEventArgs">Arguments of a button</param>
/// <returns>Font of the text of the given button</returns>
function TSDVCustomFixedButtonPanel.changeButtonFont
  (Args: TObject): TFont;
begin
  Result := nil;

  if (isAssignedOnChangeButtonFont()) then
  begin
     Result := delegateOnChangeButtonFont(Self, Args);
  end;
end; // Font changeButtonFont(...)

/// <summary>
/// Ancho por default de los botones.
/// </summary>
/// <returns>Ancho sugerido</returns>
function TSDVCustomFixedButtonPanel.readDefaultButtonWidth(): Integer;
begin
  Result := 0;
end;

/// <summary>
/// Altura por default de los botones.
/// </summary>
/// <returns>Altura sugerida</returns>
function TSDVCustomFixedButtonPanel.readDefaultButtonHeight(): Integer;
begin
  Result := 0;
end;

/// <summary>
/// Returns if the operation can be performed.
/// </summary>
/// <returns>Can rebuilt</returns>
function TSDVCustomFixedButtonPanel.canRebuiltControl(): Boolean;
begin
  Result := ((Self.Height > 0) and (Self.Width > 0));
end;

/// <summary>
/// Create the panel that contain the buttons.
/// </summary>
procedure TSDVCustomFixedButtonPanel.createContainerControls();
var ButtonsPanelLocation: TPoint;
begin
  ButtonsPanelLocation.X := 0;
  ButtonsPanelLocation.Y := 0;

  // --> agregar panel que contendra a los botones para opciones
  Self.FInternalItemsPanel := TCustomSDVNormalizedPanel.Create(Self);
    FInternalItemsPanel.Color := Graphics.clDkGray;

    FInternalItemsPanel.Left  := ButtonsPanelLocation.X;
    FInternalItemsPanel.Top   := ButtonsPanelLocation.Y;

    FInternalItemsPanel.Width  := Self.Width;
    FInternalItemsPanel.Height := Self.Height;

    FInternalItemsPanel.Align  := alClient;
  Self.InsertControl(FInternalItemsPanel);
end;

procedure TSDVCustomFixedButtonPanel.createButtons();
begin
  // --> Intended to be overriden in subclasses
  DoNothing();
end; // procedure createButtons(...)

procedure TSDVCustomFixedButtonPanel.rebuiltControl();
begin
  if (canRebuiltControl()) then
  begin
    // --> suspender despliegue
    //Self.SuspendLayout();

    // --> eliminar panel interior con botones
    if (FInternalItemsPanel <> nil) then
    begin
      FInternalItemsPanel.ClearControls();
      Self.RemoveControl(FInternalItemsPanel);
      FInternalItemsPanel := nil;
    end;

    createContainerControls();
    createButtons();
    // --> continuar despliegue
    //Self.ResumeLayout();
  end; // if (canRebuiltControl())
end; // void rebuiltControl(...)

procedure TSDVCustomFixedButtonPanel.Resize();
begin
  // --> ejecutar base, siempre
  inherited Resize();

  rebuiltControl();
end; // procedure OnResize(...)

constructor TSDVCustomFixedButtonPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TSDVCustomFixedButtonPanel.Destroy;
begin
  inherited Destroy;
end;

function TSDVCustomFixedButtonPanel.readOrientation(): TSDVControlOrientation;
begin
  Result := sdvctrlenums.sdvcorNone;
end;

end.

