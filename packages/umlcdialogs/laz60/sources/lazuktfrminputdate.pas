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

unit lazuktfrminputdate;

interface

uses
{$IFDEF MSWINDOWS}
  Windows, Messages,
  Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls,
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  {$IFDEF DELPHI}
  Mask,
  {$ELSE}
  MaskEdit,
  {$ENDIF}
{$ENDIF}
  Classes,
  uktmsgdlgtypes,
  uktmsgdlgstrs,
  dummy;

type
  TuktfrmInputDate = class(TForm)
    lbMessage: TLabel;
    btnOK: TButton;
    btnCancel: TButton;
    edYear: TMaskEdit;
    edMonth: TMaskEdit;
    edDay: TMaskEdit;
    lbYear: TLabel;
    lbMonth: TLabel;
    lbDay: TLabel;
    procedure btnOKClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
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

procedure TuktfrmInputDate.btnOKClick(Sender: TObject);
begin
  Option := mbOK;
end;

procedure TuktfrmInputDate.btnCancelClick(Sender: TObject);
begin
  Option := mbCancel;
end;

procedure TuktfrmInputDate.FormCreate(Sender: TObject);
begin
  lbYear.Caption  := reslblYear_Caption;
  lbMonth.Caption := reslblMonth_Caption;
  lbDay.Caption   := reslblDay_Caption;

  btnOK.Caption     := ButtonsCaptionsArray[mbOK];
  btnCancel.Caption := ButtonsCaptionsArray[mbCancel];
end;

initialization
  {$IFDEF FPC}
  {$I 'lazuktfrminputdate.lrs'}
  {$ENDIF}

end.
