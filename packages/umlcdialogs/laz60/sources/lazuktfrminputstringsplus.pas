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

unit lazuktfrminputstringsplus;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, CheckLst, Buttons,
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  {$IFDEF DELPHI}
  Mask,
  {$ELSE}
  MaskEdit,
  {$ENDIF}
  uktmsgdlgtypes,
  uktmsgdlgstrs,
  dummy;

type
  TuktfrmInputStringsPlus = class(TForm)
    lblLabel1: TLabel;
    btnOK: TButton;
    btnCancel: TButton;
    sbStatusBar: TStatusBar;
    mmAnswer: TCheckListBox;
    lblLabel2: TLabel;
    edAdditional: TEdit;
    btnAdd: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }

   Option:  TMsgDlgButton;
  end;

implementation

{$IFDEF delphi}
{$R *.dfm}
{$ENDIF}

procedure TuktfrmInputStringsPlus.FormCreate(Sender: TObject);
begin
  btnOK.Caption     := ButtonsCaptionsArray[mbOK];
  btnCancel.Caption := ButtonsCaptionsArray[mbCancel];
end;

procedure TuktfrmInputStringsPlus.btnOKClick(Sender: TObject);
begin
  Option := mbOK;
end;

procedure TuktfrmInputStringsPlus.btnCancelClick(Sender: TObject);
begin
  Option := mbCancel;
end;

procedure TuktfrmInputStringsPlus.btnAddClick(Sender: TObject);
begin
  if (edAdditional.Text <> '') then
  begin
    mmAnswer.Items.Add(edAdditional.Text);
  end;
end;

initialization
  {$IFDEF FPC}
  {$I 'lazuktfrminputstringsplus.lrs'}
  {$ENDIF}

end.
