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

unit lazuktfrmtransstr;

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
  uktmsgdlgarrays,
  dummy;

type
  TuktfrmTransStr = class(TForm)
    lblSource: TLabel;
    btnOK: TButton;
    btnCancel: TButton;
    edSource: TEdit;
    lblDest: TLabel;
    edDest: TEdit;
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

procedure TuktfrmTransStr.btnOKClick(Sender: TObject);
begin
  Option := mbOK;
end;

procedure TuktfrmTransStr.btnCancelClick(Sender: TObject);
begin
  Option := mbCancel;
end;

procedure TuktfrmTransStr.FormCreate(Sender: TObject);
begin
  btnOK.Caption     := ButtonsCaptionsArray[mbOK];
  btnCancel.Caption := ButtonsCaptionsArray[mbCancel];
end;

initialization
  {$IFDEF FPC}
  {$I 'lazuktfrmtransstr.lrs'}
  {$ENDIF}

end.
