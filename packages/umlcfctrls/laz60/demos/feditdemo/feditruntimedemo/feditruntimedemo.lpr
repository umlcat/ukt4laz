program feditruntimedemo;

{$MODE Delphi}

uses
  Forms, Interfaces,
  ufrmmain in 'ufrmMain.pas' {frmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
