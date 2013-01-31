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

unit lazuktfrmoptiondlg;

interface

uses
  Windows, Messages, Variants,
  SysUtils, Types, Classes,
  Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  uktmsgdlgtypes,
  uktmsgdlgstrs,
  dummy;

type
  TfrmOptionDlg = class(TForm)
    lbMessage: TLabel;
    btnOK: TButton;
    btnCancel: TButton;
    lbOptions: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
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

procedure TfrmOptionDlg.FormCreate(Sender: TObject);
begin
  btnOK.Caption     := ButtonsCaptionsArray[mbOK];
  btnCancel.Caption := ButtonsCaptionsArray[mbCancel];
end;

procedure TfrmOptionDlg.btnOKClick(Sender: TObject);
begin
  Option := mbOK;
end;

procedure TfrmOptionDlg.btnCancelClick(Sender: TObject);
begin
  Option := mbCancel;
end;

initialization
  {$IFDEF FPC}
  {$I 'lazuktfrmoptiondlg.lrs'}
  {$ENDIF}

end.
