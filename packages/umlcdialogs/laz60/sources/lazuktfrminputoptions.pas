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

unit lazuktfrminputoptions;

interface

uses
{$IFDEF MSWINDOWS}
  Windows, Messages,
  Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls,
{$ENDIF}
  SysUtils, Types, Classes, Variants,
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  uktmsgdlgtypes,
  uktmsgdlgstrs,
  dummy;

type
  TuktfrmInputOptions = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    rgOptions: TRadioGroup;
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

procedure TuktfrmInputOptions.btnOKClick(Sender: TObject);
begin
  Option := mbOK;
end;

procedure TuktfrmInputOptions.btnCancelClick(Sender: TObject);
begin
  Option := mbCancel;
end;

procedure TuktfrmInputOptions.FormCreate(Sender: TObject);
begin
  btnOK.Caption     := ButtonsCaptionsArray[mbOK];
  btnCancel.Caption := ButtonsCaptionsArray[mbCancel];
end;

initialization
  {$IFDEF FPC}
  {$I 'lazuktfrminputoptions.lrs'}
  {$ENDIF}

end.
