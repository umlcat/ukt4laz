program RuntimeDemo;

{$MODE Delphi}

uses
  Forms, Interfaces,
  UFMain in 'UFMain.pas' {FMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFMain, FMain);
  Application.Run;
end.
