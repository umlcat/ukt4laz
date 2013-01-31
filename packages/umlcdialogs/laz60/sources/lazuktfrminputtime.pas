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

unit lazuktfrminputtime;

interface

uses
{$IFDEF MSWINDOWS}
  Windows, Messages,
  Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls,
{$ENDIF}
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  {$IFDEF DELPHI}
  Mask,
  {$ELSE}
  MaskEdit,
  {$ENDIF}
  SysUtils, Types, Variants, Classes,
  uktmsgdlgtypes,
  uktmsgdlgstrs,
  dummy;

type
  TuktfrmInputTime = class(TForm)
    lbMessage: TLabel;
    btnOK: TButton;
    btnCancel: TButton;
    edHour: TMaskEdit;
    edMin: TMaskEdit;
    edSec: TMaskEdit;
    lbHour: TLabel;
    lbMin: TLabel;
    lbSec: TLabel;
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

procedure TuktfrmInputTime.btnOKClick(Sender: TObject);
begin
  Option := mbOK;
end;

procedure TuktfrmInputTime.btnCancelClick(Sender: TObject);
begin
  Option := mbCancel;
end;

procedure TuktfrmInputTime.FormCreate(Sender: TObject);
begin
  lbHour.Caption := reslblHour_Caption;
  lbMin.Caption  := reslblMin_Caption;
  lbSec.Caption  := reslblSec_Caption;

  btnOK.Caption     := ButtonsCaptionsArray[mbOK];
  btnCancel.Caption := ButtonsCaptionsArray[mbCancel];
end;

initialization
  {$IFDEF FPC}
  {$I 'lazuktfrminputtime.lrs'}
  {$ENDIF}

end.
