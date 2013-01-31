unit ufrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons,
  vclsdvInputDlgs;

type
  TfrmMain = class(TForm)
    edMessage: TEdit;
    lbMessage: TLabel;
    btnInputStr: TButton;
    btnExit: TBitBtn;
    btnInputInt: TButton;
    btnInputPassword: TButton;
    btnInputDate: TButton;
    btnInputTime: TButton;
    procedure btnInputStrClick(Sender: TObject);
    procedure btnInputIntClick(Sender: TObject);
    procedure btnInputPasswordClick(Sender: TObject);
    procedure btnInputDateClick(Sender: TObject);
    procedure btnInputTimeClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.btnInputStrClick(Sender: TObject);
var Answer: string;
begin
  if (InputString('Informacion Personal', 'Dame tu nombre:', Answer))
    then edMessage.Text := Answer;
end;

procedure TfrmMain.btnInputIntClick(Sender: TObject);
var Answer: Integer;
begin
  if (InputInteger('Informacion Personal', 'Dame tu edad:', Answer))
    then edMessage.Text := IntToStr(Answer);
end;

procedure TfrmMain.btnInputPasswordClick(Sender: TObject);
//var Answer: string;
begin
//  if (InputPassword('Informacion Personal', 'Dame tu contraseña:', 14, Answer))
//    then edMessage.Text := Answer;
end;

procedure TfrmMain.btnInputDateClick(Sender: TObject);
var Answer: TDateTime;
begin
  if (InputDate
    ('Informacion Personal', 'Dame tu fecha de nacimiento:', Answer))
    then edMessage.Text := DateToStr(Answer);
end;

procedure TfrmMain.btnInputTimeClick(Sender: TObject);
var Answer: TDateTime;
begin
  if (InputTime
    ('Informacion Personal', 'Dame tu hora de nacimiento:', Answer))
    then edMessage.Text := TimeToStr(Answer);
end;

end.
