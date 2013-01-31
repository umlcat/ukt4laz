(*****************************************************************************
 *                                                                           *
 *  This file is part of the UMLCat Component Library.                       *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
 **)

unit uktxmlfileansiscanners;

interface
uses
  SysUtils, Classes,
  uktshortansistrs,
  //uktstreams,
  uktansitextstreams,
  //uktscanners,
  ukttagscanners,
  uktscannerstates,
  uktscanneroptions,
  uktxmlfiletokens,
  uktxmlfilegroups,
  uktxmlfileansisymbols,
  dummy;

type

// abstract

(* TCustomXMLFileANSIScanner *)

  TCustomXMLFileANSIScanner = class(TCustomSDVTagScanner)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)

    FInternalCurrentSymbol: TXMLFileANSISymbol;
    // simbolo detectado actualmente
    // current detected symbol

    FWaitSymbol: TXMLFileANSISymbol;
    // simbolo utilizado en preanalisis
    // preanalysis used symbol

    UseWaitToken: Boolean;
    // ficha utilizada mientras se espera siguiente
    // token used while next is expected
  protected
    (* Protected declarations *)

    function getInternalCurrentSymbol: TXMLFileANSISymbol; virtual;
    function getStream: TCustomSDVANSITextBasedStream; virtual;

    procedure setInternalCurrentSymbol(const Value: TXMLFileANSISymbol); virtual;
    procedure setStream(const Value: TCustomSDVANSITextBasedStream); virtual;
  protected
    (* Protected declarations *)

    function InternalNext(): Boolean; virtual;
  protected
    (* Protected declarations *)

    (* property declarations *)

    property InternalCurrentSymbol: TXMLFileANSISymbol
      read getInternalCurrentSymbol write setInternalCurrentSymbol;
  public
    (* Public declarations *)

    function Start(): Boolean; override;
    function Next(): Boolean; override;
    function Finish(): Boolean; override;

    function Read(): Boolean; virtual;
    function MoveNext(): Boolean;  virtual;

    procedure ReadCurrentSymbol
      (var ASymbol: TXMLFileANSISymbol);
  public
    (* Public declarations *)

    (* property declarations *)

    property Stream: TCustomSDVANSITextBasedStream
      read getStream write setStream;
  end;

(* TCustomMinimalXMLFileANSIScanner *)

  TCustomMinimalXMLFileANSIScanner= class(TCustomXMLFileANSIScanner)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)
  public
    (* Public declarations *)

    function Read(): Boolean; override;
    function MoveNext(): Boolean;  override;
  end;

(* TCustomExtendedXMLFileANSIScanner *)

  TCustomExtendedXMLFileANSIScanner= class(TCustomXMLFileANSIScanner)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)

    FBackupSymbol:  TXMLFileANSISymbol;
    // simbolo utilizado mientras se concatena simbolos de texto
    // symbol used while concatenning text symbols
  public
    (* Public declarations *)

    function Read(): Boolean; override;
    function MoveNext(): Boolean;  override;
  end;

// concrete

(* TCustomXMLFileANSIScanner2 *)

  TCustomXMLFileANSIScanner2 = class(TCustomXMLFileANSIScanner)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)

    FBackupSymbol:  TXMLFileANSISymbol;
    // simbolo utilizado mientras se concatena simbolos de texto
    // symbol used while concatenning text symbols

    UseBackupToken: Boolean;
    // ficha utilizada cuando hay respaldo
    // token used while there*s a backup
  protected
    (* Protected declarations *)

    function ScanNext(): Boolean;
  public
    (* Public declarations *)

    function Start(): Boolean; override;
    function Next(): Boolean; override;
    function Finish(): Boolean; override;

    function Read(): Boolean; override;
    function MoveNext(): Boolean; override;
  end;

(* TXMLLiteFileANSIScanner *)

  TXMLFileANSIScanner= class(TCustomXMLFileANSIScanner)
  published
    (* published declarations *)

    (* tcustomsdscanner: *)

//    property Options;
    property Stream;

    (* TCustomXMLFileANSIScanner: *)
  end;

implementation

{$include 'uktxmlfilematrix.incpas'}

(* TCustomXMLFileANSIScanner2 *)

function TCustomXMLFileANSIScanner2.ScanNext(): Boolean;
var BackupText, PrevText, KeepScanning: Boolean;

  procedure ReturnToken;
  begin
    if (PrevText) then
    begin
      UseBackupToken := not
        ((Options.Spaces = scnopRemoveTabs) and
         IsStringOfChar(FBackupSymbol.Text, #32));
      // check if there*s tabulator spaces before another element
      // revisar si hay espacios de tabulacion antes de otro elemento

      if (UseBackupToken) then
      begin
        uktxmlfileansisymbols.Exchange(FInternalCurrentSymbol, FBackupSymbol);
        // intercambiar simbolo de texto respaldado con nuevo simbolo no-texto,
        // dejando pendiente el simbolo no-texto para la proxima vez

        // exchange backup text symbol with new non-text symbol,
        // keeping the non-text symbol for next time
      end else
      begin
        uktxmlfileansisymbols.Clear(FBackupSymbol);
        // remove leading tabulator spaces,
        // spaces among text will be returned

        // eliminar espacios de tabulacion iniciales,
        // espacios entre texto se regresaran
      end;
    end;
  end;

  procedure TextToken;
  begin
    uktxmlfileansisymbols.Concat(FBackupSymbol, FInternalCurrentSymbol);
    // respaldar este caracter con el fin de unir con los siguientes
    // caracteres como un solo simbolo de texto

    // backup this character in order to concat it with next characters
    // as a single text symbol

    PrevText := TRUE;
    KeepScanning := TRUE;
  end;

  procedure SpaceToken;
  begin
    BackupText := (Options.Spaces in [scnopReturnAsText, scnopRemoveTabs]);
    if (BackupText) then
    begin
      uktxmlfileansisymbols.Concat(FBackupSymbol, FInternalCurrentSymbol);
      // respaldar este caracter con el fin de unir con los siguientes
      // caracteres como un solo simbolo de texto

      // backup this character in order to concat it with next characters
      // as a single text symbol

      PrevText := TRUE;
      KeepScanning := TRUE;
    end else KeepScanning := (Options.Spaces = scnopIgnoreTag);
    // ignorar caracteres de "espacios", ir por el siguiente simbolo
    // ignore "space" characters, go for next token
  end;

  procedure EoFToken;
  begin
    KeepScanning := (Options.EoF = scnopIgnoreTag);
    // ignorar caracteres de "Fin de Archivo", ir por el siguiente simbolo
    // ignore "End of File" characters, go for next symbol
  end;
  
  procedure EoPgToken;
  begin
    KeepScanning := (Options.EoPg = scnopIgnoreTag);
    // ignorar caracteres de "Salto de Pagina", ir por el siguiente simbolo
    // ignore "Page Break" characters, go for next symbol
  end;

  procedure EoLnToken;
  begin
    BackupText := (Options.EoLn = scnopReturnAsText);
    // ignorar caracteres de "Salto de Linea", ir por el siguiente simbolo
    // ignore "Line Break" characters, go for next symbol

    if (BackupText) then
    begin
      uktxmlfileansisymbols.Concat(FBackupSymbol, FInternalCurrentSymbol);
      // respaldar este caracter con el fin de unirlo con los siguientes
      // caracteres como un solo simbolo de texto

      // backup this character in order to concat it with next characters
      // as a single text symbol

      PrevText := TRUE;
      KeepScanning := TRUE;
    end else
    begin
      KeepScanning := (Options.EoLn = scnopIgnoreTag);
      if (not KeepScanning)
        then ReturnToken;
    end;
  end;

  procedure EntityToken;
  begin
    BackupText := (Options.Specials = scnopReturnAsText);
    // ignorar caracteres especiales, ir por el siguiente simbolo
    // ignore special characters, go for next symbol

    if (BackupText) then
    begin
      FInternalCurrentSymbol.Text := EntityToCharEx(FInternalCurrentSymbol.Text);

      uktxmlfileansisymbols.Concat(FBackupSymbol, FInternalCurrentSymbol);
      // respaldar este caracter con el fin de unirlo con los siguientes
      // caracteres como un solo simbolo de texto

      // backup this character in order to concat it with next characters
      // as a single text symbol

      PrevText := TRUE;
      KeepScanning := TRUE;
    end else
    begin
      KeepScanning := (Options.Specials = scnopIgnoreTag);
      if (not KeepScanning)
        then ReturnToken;
    end;
  end;

  procedure ElseToken;
  begin
    ReturnToken;
    KeepScanning := FALSE;
  end;

begin
  PrevText := FALSE;
  // no hay texto previamente guardado
  // there*s not any text stored

  uktxmlfileansisymbols.Clear(FBackupSymbol);
  // preparar almacenaje para respaldo
  // prepare backup storage

  KeepScanning := FALSE;
  repeat
    Result := InternalNext();
    // obtener "EoF", "EoPg", "EoLn", "espacio" como simbolos independientes
    // obtain "EoF", "EoPg", "EoLn", "space" as independent symbols

    case (FInternalCurrentSymbol.Token) of
      xmlfiletkEntity: EntityToken;
      xmlfiletkText:   TextToken;
      xmlfiletkSpace:  SpaceToken;
      xmlfiletkEoF:    EoFToken;
      xmlfiletkEoPg:   EoPgToken;
      xmlfiletkEoLn:   EoLnToken;
      else             ElseToken;
    end;
  until ((not KeepScanning) or (not Result))

  { Objetivo: El procedimiento "ScanNext" obtiene varios simbolos de }
  { "InternalNext" y regresa un solo simbolo porque el objetivo de este }
  { procedimiento es tratar cada caracter de espacios, tabulacion ,}
  { salto de linea y salto de pagina como un solo caracter individual }
  { o como parte de una secuencia de texto segun las opciones del }
  { analizador .}

  { Goal: "ScanNext" procedure obtains several symbols from  }
  { "InternalNext" and a single symbol, beacuse the goal of this procedure }
  { is to treat each space, tabulator, line break and page break characters }
  { as a single character or as part of a text sequence upon scanner*s }
  { options .}
end;

function TCustomXMLFileANSIScanner2.Start(): Boolean;
begin
  FCurrentRow := 1;
  FCurrentCol := 1;

  UseWaitToken  := FALSE;
  // ficha utilizada mientras se espera siguiente
  // token used while next is expected

  UseBackupToken  := FALSE;
  // ficha utilizada cuando hay respaldo
  // token used while there*s a backup

  Result := FStream.Connect();
end;

function TCustomXMLFileANSIScanner2.Next(): Boolean;
begin
  Result := Read();
  MoveNext();
end;

function TCustomXMLFileANSIScanner2.Finish(): Boolean;
begin
  UseBackupToken  := FALSE;
  // ficha utilizada cuando hay respaldo
  // token used while there*s a backup

  UseWaitToken  := FALSE;
  // ficha utilizada mientras se espera siguiente
  // token used while next is expected

  Result := FStream.Disconnect();
end;

function TCustomXMLFileANSIScanner2.Read(): Boolean;
begin
  Result := TRUE;
  if (not UseWaitToken) then
  begin
    if (not UseBackupToken) then
    begin
      Result := ScanNext();
      // obtener un simbolo del archivo
      // obtain a symbol from the file

      if (Result) then
      begin
        FWaitSymbol := FInternalCurrentSymbol;
        UseWaitToken := TRUE;
        // dejarlo pendiente en caso de usarlo otra vez
        // leave it so we may use it again
      end;
    end else
    begin
      FInternalCurrentSymbol := FBackupSymbol;
      UseBackupToken := FALSE;
    end;
    // sino, regresar el simbolo de texto respaldado
    // else, return the backup text symbol

  end else FInternalCurrentSymbol := FWaitSymbol;
  // sino, regresar el simbolo en espera
  // else, return the symbol in delay
end;

function TCustomXMLFileANSIScanner2.MoveNext(): Boolean;
begin
  UseWaitToken := FALSE;
  Result := TRUE;
end;

(* TCustomXMLFileANSIScanner *)

function TCustomXMLFileANSIScanner.InternalNext(): Boolean;
var C: ANSIchar; G: TXMLFileGroup; CurrentState: TSDVState;
begin
  uktxmlfileansisymbols.Clear(FInternalCurrentSymbol);

  FInternalCurrentSymbol.StartRow := FCurrentRow;
  FInternalCurrentSymbol.StartCol := FCurrentCol;

  CurrentState := stStart;
  repeat
    C := #0;
    Stream.GetChar(C);
    G := CharToGroup(C);
    CurrentState := NextState(CurrentState, G);

    if (G = xmlfilegrEoLn) then
    begin
      FCurrentCol := 1;
      System.Inc(FCurrentRow);
    end else
    begin
      System.Inc(FCurrentCol);
    end;
    // actualizar posicion de caracteres
    // update characters location

    if (CanSave(CurrentState))
      then uktshortansistrs.Concat(FInternalCurrentSymbol.Text, C);
    if (CanMove(CurrentState))
      then Stream.NextChar();
  until (IsTerminal(CurrentState));

  FInternalCurrentSymbol.FinishRow := FCurrentRow;
  FInternalCurrentSymbol.FinishCol := FCurrentCol;
  FInternalCurrentSymbol.Token := StateToToken(CurrentState);

  Result := not (CurrentState < stStart);

  { Objetivo: El procedimiento "InternalNext" analiza una secuencia }
  { de caracteres, construyendo y regresando un solo simbolo ,}

  { Advertencia: Este procedimiento considera cada espacio, caracter de }
  { tabulacion, salto de linea y salto de pagina como simbolos }
  { independientes !!!}

  { Goal: "InternalNext" procedure scans a sequence of characters }
  { and constructs and returns a single symbol ,}

  { Warning: This procedures considers each space, tabulator character ,}
  { line break and page break as independent symbols !!!}
end;

function TCustomXMLFileANSIScanner.getInternalCurrentSymbol: TXMLFileANSISymbol;
begin
  Result := FInternalCurrentSymbol;
end;

function TCustomXMLFileANSIScanner.getStream: TCustomSDVANSITextBasedStream;
begin
  Result := (FStream as TCustomSDVANSITextBasedStream);
end;

procedure TCustomXMLFileANSIScanner.setInternalCurrentSymbol
  (const Value: TXMLFileANSISymbol);
begin
  FInternalCurrentSymbol := Value;
end;

procedure TCustomXMLFileANSIScanner.setStream(const Value: TCustomSDVANSITextBasedStream);
begin
  if (FStream <> Value) then
  begin
    FStream := Value;
  end;
end;

function TCustomXMLFileANSIScanner.Start(): Boolean;
begin
  FCurrentRow := 1;
  FCurrentCol := 1;

  uktxmlfileansisymbols.Clear(FInternalCurrentSymbol);
  uktxmlfileansisymbols.Clear(FWaitSymbol);

  Result := FStream.Connect();
end;

function TCustomXMLFileANSIScanner.Next(): Boolean;
begin
  Result := Read();
  MoveNext();
end;

function TCustomXMLFileANSIScanner.Finish(): Boolean;
begin
  Result := FStream.Disconnect();

  uktxmlfileansisymbols.Clear(FInternalCurrentSymbol);
  uktxmlfileansisymbols.Clear(FWaitSymbol);

  FCurrentRow := 0;
  FCurrentCol := 0;
end;

function TCustomXMLFileANSIScanner.Read(): Boolean;
begin
  Result := (not UseWaitToken);
  if (Result) then
  begin
    Result := InternalNext();
    // obtener un simbolo del archivo
    // obtain a symbol from the file

    if (Result) then
    begin
      FWaitSymbol  := FInternalCurrentSymbol;
      UseWaitToken := TRUE;
      // dejarlo pendiente en caso de usarlo otra vez
      // leave it so we may use it again
    end;
  end else FInternalCurrentSymbol := FWaitSymbol;
  // sino, regresar el simbolo en espera
  // else, return the symbol in delay
end;

function TCustomXMLFileANSIScanner.MoveNext(): Boolean;
begin
  UseWaitToken := FALSE;
  Result := TRUE;
end;

procedure TCustomXMLFileANSIScanner.ReadCurrentSymbol
  (var ASymbol: TXMLFileANSISymbol);
begin
  ASymbol := FInternalCurrentSymbol;
end;

(* TCustomMinimalXMLFileANSIScanner *)

function TCustomMinimalXMLFileANSIScanner.Read(): Boolean;
begin
  Result := TRUE;
end;

function TCustomMinimalXMLFileANSIScanner.MoveNext(): Boolean;
begin
//  UseWaitToken := FALSE;
  Result := TRUE;
end;

(* TCustomExtendedXMLFileANSIScanner *)

function TCustomExtendedXMLFileANSIScanner.Read(): Boolean;
begin
  Result := TRUE;
end;

function TCustomExtendedXMLFileANSIScanner.MoveNext(): Boolean;
begin
//  UseWaitToken := FALSE;
  Result := TRUE;
end;

end.
