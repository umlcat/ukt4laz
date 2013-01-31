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

unit lazuktfrmmsgmemo;

interface

uses
  SysUtils, Types, Classes,
  Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls,
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  uktmsgdlgtypes,
  uktmsgdlgstrs,
  uktmsgdlgarrays,
  dummy;

type
  TuktfrmMsgMemo = class(TForm)
    btnCancel: TButton;
    btnOK: TButton;
    btnYes: TButton;
    btnNo: TButton;
    btnAbort: TButton;
    btnIgnore: TButton;
    btnReTry: TButton;
    btnYesToAll: TButton;
    imgError: TImage;
    imgInformation: TImage;
    imgWarning: TImage;
    imgConfirmation: TImage;
    btnNoToAll: TButton;
    mmMemo: TMemo;
    imgAbout: TImage;
    imgHelp: TImage;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure btnYesClick(Sender: TObject);
    procedure btnNoClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnAbortClick(Sender: TObject);
    procedure btnReTryClick(Sender: TObject);
    procedure btnIgnoreClick(Sender: TObject);
    procedure btnYesToAllClick(Sender: TObject);
    procedure btnNoToAllClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }

    ButtonPressed:  TMsgDlgButton;
    Buttons: TButtonsArray;
    Images:  TImagesArray;
  end;

implementation

{$IFDEF delphi}
{$R *.dfm}
{$ENDIF}
{$IFDEF FPC}
{$R *.lfm}
{$ENDIF}

procedure TuktfrmMsgMemo.FormCreate(Sender: TObject);
var I: TMsgDlgButton; J: TMsgDlgType;
begin
  Buttons[mbOK]       := btnOK;
  Buttons[mbCancel]   := btnCancel;
  Buttons[mbAbort]    := btnAbort;
  Buttons[mbReTry]    := btnReTry;
  Buttons[mbIgnore]   := btnIgnore;
  Buttons[mbYes]      := btnYes;
  Buttons[mbNo]       := btnNo;
  Buttons[mbYesToAll] := btnYesToAll;
  Buttons[mbNoToAll]  := btnNoToAll;
  // create an array of buttons for control

  Images[mtWarning]      := imgWarning;
  Images[mtError]        := imgError;
  Images[mtInformation]  := imgInformation;
  Images[mtConfirmation] := imgConfirmation;
  Images[mtHelp]         := imgHelp;
  Images[mtAbout]        := imgAbout;
  Images[mtCustomized]   := nil;
  // create an array of images for control

  for I := Low(TMsgDlgButton) to High(TMsgDlgButton) do
  begin
    Buttons[I].Caption := ButtonsCaptionsArray[I];
  end;

  // "customized" type doesn't have an image:
  for J := Low(TMsgDlgType) to Pred(High(TMsgDlgType)) do
  begin
    Images[J].Left := 10;
    Images[J].Top  := 10;
  end;
end;

procedure TuktfrmMsgMemo.FormClose
(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  // "forma de usese y tirese"
  // "use and dispose form"
end;

procedure TuktfrmMsgMemo.btnYesClick(Sender: TObject);
begin
  ButtonPressed := mbYes;
  Self.Close;
  // no se puede utilizar "ModalResult", algunos valores no existen en delphi 4
  // cannot use "ModalResult", some values doesn*t exist in delphi 4
end;

procedure TuktfrmMsgMemo.btnNoClick(Sender: TObject);
begin
  ButtonPressed := mbNo;
  Self.Close;
  // no se puede utilizar "ModalResult", algunos valores no existen en delphi 4
  // cannot use "ModalResult", some values doesn*t exist in delphi 4
end;

procedure TuktfrmMsgMemo.btnOKClick(Sender: TObject);
begin
  ButtonPressed := mbOK;
  Self.Close;
  // no se puede utilizar "ModalResult", algunos valores no existen en delphi 4
  // cannot use "ModalResult", some values doesn*t exist in delphi 4
end;

procedure TuktfrmMsgMemo.btnCancelClick(Sender: TObject);
begin
  ButtonPressed := mbCancel;
  Self.Close;
  // no se puede utilizar "ModalResult", algunos valores no existen en delphi 4
  // cannot use "ModalResult", some values doesn*t exist in delphi 4
end;

procedure TuktfrmMsgMemo.btnAbortClick(Sender: TObject);
begin
  ButtonPressed := mbCancel;
  Self.Close;
  // no se puede utilizar "ModalResult", algunos valores no existen en delphi 4
  // cannot use "ModalResult", some values doesn*t exist in delphi 4
end;

procedure TuktfrmMsgMemo.btnReTryClick(Sender: TObject);
begin
  ButtonPressed := mbReTry;
  Self.Close;
  // no se puede utilizar "ModalResult", algunos valores no existen en delphi 4
  // cannot use "ModalResult", some values doesn*t exist in delphi 4
end;

procedure TuktfrmMsgMemo.btnIgnoreClick(Sender: TObject);
begin
  ButtonPressed := mbIgnore;
  Self.Close;
  // no se puede utilizar "ModalResult", algunos valores no existen en delphi 4
  // cannot use "ModalResult", some values doesn*t exist in delphi 4
end;

procedure TuktfrmMsgMemo.btnYesToAllClick(Sender: TObject);
begin
  ButtonPressed := mbYesToAll;
  Self.Close;
  // no se puede utilizar "ModalResult", algunos valores no existen en delphi 4
  // cannot use "ModalResult", some values doesn*t exist in delphi 4
end;

procedure TuktfrmMsgMemo.btnNoToAllClick(Sender: TObject);
begin
  ButtonPressed := mbNoToAll;
  Self.Close;
  // no se puede utilizar "ModalResult", algunos valores no existen en delphi 4
  // cannot use "ModalResult", some values doesn*t exist in delphi 4
end;

end.
