program DesignTimeDemo;

uses
  Forms,
  ufrmMain in 'ufrmMain.pas' {frmMain},
  ufrmPopUpDialog in 'ufrmPopUpDialog.pas' {frmPopUpDialog},
  udmDataModule in 'udmDataModule.pas' {dmDataModule: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TdmDataModule, dmDataModule);
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
