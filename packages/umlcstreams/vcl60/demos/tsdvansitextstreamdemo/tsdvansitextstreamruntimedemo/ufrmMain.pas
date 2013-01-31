unit ufrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ComCtrls, StdCtrls, Buttons,
  sdshortansistrs, sdtextmarkers,
  sdvrecstreams, sdvfilestreams, sdvstatestreams,
  sdvansicharstreams, sdvansitextstreams;

const
  extTextFile = 'txt';
  dotTextFile = '.txt';
  wldTextFile = '*.txt';

resourcestring
  resTfrMain_Caption = 'tcustomsdansitextstream runtime demo';

  sbtnNew_Caption    = 'New';
  sbtnOpen_Caption   = 'Open';
  sbtnSave_Caption   = 'Save';
  sbtnExit_Caption   = 'Exit';

  fltTextFile = 'Text files (*.txt)|*.txt';
  msgConstruction    = 'Under construction...';

type
  TfrmMain = class(TForm)
    sbStatusBar: TStatusBar;
    pnTop: TPanel;
    btnExit: TBitBtn;
    mmMain: TMemo;
    btnSave: TBitBtn;
    dlgOpenDialog: TOpenDialog;
    btnOpen: TBitBtn;
    dlgSaveDialog: TSaveDialog;
    btnNew: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnNewClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }

    BaseStream: tcustomsdfilestream;
    Stream:     tcustomsdansitextstream;

    procedure LoadResourceStrings;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
var Path: string;
begin
  LoadResourceStrings;

//  Path := ParamStr(0);
  Path := 'c:\temp\';
  // obtener ruta completa de programa

  Path := SysUtils.ExtractFilePath(Path);
  // remover nombrearchivo y extension

  dlgOpenDialog.InitialDir := Path;
  dlgSaveDialog.InitialDir := Path;

  BaseStream := tcustomsdfilestream.Create(nil);
  Stream := tcustomsdansitextstream.Create(nil);
  Stream.Reference := BaseStream;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  Stream.Free;
  BaseStream.Free;
end;

procedure TfrmMain.btnOpenClick(Sender: TObject);
var Buffer: shortansistring;
begin
  mmMain.Lines.Clear;
  if dlgOpenDialog.Execute then
  begin
    BaseStream.Path  := dlgOpenDialog.FileName;
    BaseStream.State := ssReset;

    Stream.Connect;

    while not Stream.IsEoF do
    begin
      Stream.ReadLn(Buffer);
      mmMain.Lines.Add(Buffer);
    end;

    Stream.Disconnect;
  end;
end;

procedure TfrmMain.btnSaveClick(Sender: TObject);
var Buffer: shortansistring; I: Integer;
begin
  if dlgSaveDialog.Execute then
  begin
    BaseStream.Path := dlgSaveDialog.FileName;
    BaseStream.State := ssRewrite;

    Stream.Connect;

    for I := 0 to Pred(mmMain.Lines.Count) do
    begin
      System.FillChar(Buffer, System.SizeOf(Buffer), 0);
      Buffer := mmMain.Lines[I];
      Stream.WriteLn(Buffer);
    end;

    Stream.Disconnect;
  end;
end;

procedure TfrmMain.btnNewClick(Sender: TObject);
begin
  mmMain.Lines.Clear;
end;

procedure TfrmMain.LoadResourceStrings;
begin
  Self.Caption := resTfrMain_Caption;

  btnNew.Caption  := sbtnNew_Caption;
  btnOpen.Caption := sbtnOpen_Caption;
  btnSave.Caption := sbtnSave_Caption;
  btnExit.Caption := sbtnExit_Caption;

  dlgOpenDialog.Filter := fltTextFile;
  dlgSaveDialog.Filter := fltTextFile;

  dlgOpenDialog.DefaultExt := extTextFile;
  dlgSaveDialog.DefaultExt := extTextFile;
end;

end.
