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

unit uktaboutdlgs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Forms, Dialogs,
  uktcomponents,
  uktdlgctrls,
  lazuktfrmabout,
  dummy;

type

(* TCustomSDVAboutDialog *)

  TCustomSDVAboutDialog = class(TSDVDialogComponent)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)

    FSummary:     TStrings;
    FCompany:     string;
    FProductName: string;
    FVersion:     string;
    FCopyright:   string;
    FComments:    string;
  public
    (* Public declarations *)

    function CreateDialog(): TCustomForm; override;
  public
    (* Public declarations *)

    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
  public
    (* Public declarations *)

    (* UnPublished declarations *)

    property Summary: TStrings
      read FSummary write FSummary;
    property ProductName: string
      read FProductName write FProductName;
    property Company: string
      read FCompany write FCompany;
    property Version: string
      read FVersion write FVersion;
    property Copyright: string
      read FCopyright write FCopyright;
    property Comments: string
      read FComments write FComments;
  end;

  (* TSDVAboutDialog *)

  TSDVAboutDialog = class(TCustomSDVAboutDialog)
  published
    (* Published declarations *)

    (* TDialogComponent: *)

    property HelpContext;
    property Title;

    property BeforeExecute;
    property AfterExecute;

    (* TCustomSDVAboutDialog: *)

    property Summary;
    property Company;
  end;

implementation

(* TCustomSDVAboutDialog *)

function TCustomSDVAboutDialog.CreateDialog(): TCustomForm;
var F: Tfrmabout;
begin
  Application.CreateForm(Tfrmabout, FForm);
  Application.ProcessMessages;

  // --> assign some initial properties to the form,
  // --> before showing
  F := (FForm as Tfrmabout);
  F.Container := Self;

  Application.ProcessMessages;
  F.Caption    := FTitle;

  F.SummaryMemo.Lines.Clear();
  F.SummaryMemo.Lines.Assign(FSummary);

  F.ProductNameLabel.Caption := FProductName;
  F.CompanyLabel.Caption     := FCompany;
  F.VersionLabel.Caption     := FVersion;
  F.CopyrightLabel.Caption   := FCopyright;
  F.CommentsLabel.Caption    := FComments;

  // ...

  Result := FForm;
end;

constructor TCustomSDVAboutDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSummary := TStringList.Create();
end;

destructor TCustomSDVAboutDialog.Destroy();
begin
  FSummary.Free();
  inherited Destroy();
end;

end.

