unit ufrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ComCtrls, StdCtrls, Buttons,
  sdvrecstreams, sdvfilestreams, sdvstatestreams;
  
type
  TfrmMain = class(TForm)
    sbStatusBar: TStatusBar;
    pnTop: TPanel;
    btnExit: TBitBtn;
    mmMain: TMemo;
    btnSave: TBitBtn;
    dlgOpenDialog: TOpenDialog;
    btnLoad: TBitBtn;
    dlgSaveDialog: TSaveDialog;
    btnNew: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnNewClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }

    Stream: TCustomSDFileStream;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
var Path: string;
begin
//  Path := ParamStr(0);
  Path := 'c:\temp\';
  // obtener ruta completa de programa

  Path := SysUtils.ExtractFilePath(Path);
  // remover nombrearchivo y extension

  dlgOpenDialog.InitialDir := Path;
  dlgSaveDialog.InitialDir := Path;

  Stream := TCustomSDFileStream.Create(nil);
  Stream.AssignRecordSize(SizeOf(shortstring));
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  Stream.Free;
end;

procedure TfrmMain.btnLoadClick(Sender: TObject);
var Buffer: shortstring;
begin
  mmMain.Lines.Clear;
  if dlgOpenDialog.Execute then
  begin
    Stream.Path := dlgOpenDialog.FileName;
    Stream.State := ssReset;
    Stream.Connect;

    while not Stream.IsEoF do
    begin
      Stream.ReadRecord(@Buffer);
      mmMain.Lines.Add(Buffer);
    end;
  end;
end;

procedure TfrmMain.btnSaveClick(Sender: TObject);
var Buffer: shortstring; I: Integer;
begin
  if dlgSaveDialog.Execute then
  begin
    Stream.Path := dlgSaveDialog.FileName;
    Stream.State := ssRewrite;
    Stream.Connect;

    for I := 0 to Pred(mmMain.Lines.Count) do
    begin
      Stream.ClearRecord(@Buffer);
      Buffer := mmMain.Lines[I];
      Stream.WriteRecord(@Buffer);
    end;

    Stream.Disconnect;
  end;
end;

procedure TfrmMain.btnNewClick(Sender: TObject);
begin
  mmMain.Lines.Clear;
end;

end.
