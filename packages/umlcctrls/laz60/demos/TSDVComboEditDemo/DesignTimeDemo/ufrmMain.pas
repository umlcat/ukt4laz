unit ufrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Buttons,
  vclsdvComboEdits, vclsdvPanels, vclsdvComboCtrls;

type
  TfrmMain = class(TForm)
    pnTop: TPanel;
    btnExit: TBitBtn;
    btnTest: TButton;
    Control: TSDVComboEdit;
    procedure btnTestClick(Sender: TObject);
    procedure ControlClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }

    procedure ControlOnClick(Sender: TObject);
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.btnTestClick(Sender: TObject);
begin
//
end;

procedure TfrmMain.ControlOnClick(Sender: TObject);
begin
  ShowMessage('Hello World !!!');
end;

procedure TfrmMain.ControlClick(Sender: TObject);
begin
  ShowMessage('Hello World !!!');
end;

end.
