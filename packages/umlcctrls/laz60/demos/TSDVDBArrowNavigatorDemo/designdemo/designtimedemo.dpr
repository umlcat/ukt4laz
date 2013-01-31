program designtimedemo;

uses
  Forms,
  ufrmMain in '..\runtimedemo\ufrmMain.pas' {frmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
