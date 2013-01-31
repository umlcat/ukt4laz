unit ufrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ComCtrls, StdCtrls, Buttons,
  sdtextmarkers,
  sdvrecstreams, sdvfilestreams, sdvstatestreams, sdvansicharstreams,
  sdvstrfilestreams;

resourcestring
  resTfrMain_Caption = 'tcustomsdstrfilestream runtime demo';

  sbtnNew_Caption    = 'New';
  sbtnTest_Caption   = 'Test';
  sbtnExit_Caption   = 'Exit';

  msgConstruction    = 'Under construction...';

type
  TfrmMain = class(TForm)
    sbStatusBar: TStatusBar;
    pnTop: TPanel;
    btnExit: TBitBtn;
    mmMain: TMemo;
    btnNew: TBitBtn;
    btnTest: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnNewClick(Sender: TObject);
    procedure btnTestClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }

    BaseStream: tcustomsdstrfilestream;
    Stream:     TCustomSDansicharStream;

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

  BaseStream := tcustomsdstrfilestream.Create(nil);
  BaseStream.Text := 'Hello World';

  Stream := tcustomsdansicharstream.Create(nil);
  Stream.Reference := BaseStream;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  Stream.Free;
  BaseStream.Free;
end;

procedure TfrmMain.btnNewClick(Sender: TObject);
begin
  mmMain.Lines.Clear;
  BaseStream.Text := 'Hello World';
end;

procedure TfrmMain.LoadResourceStrings;
begin
  Self.Caption := resTfrMain_Caption;

  btnNew.Caption  := sbtnNew_Caption;
  btnTest.Caption := sbtnTest_Caption;  
  btnExit.Caption := sbtnExit_Caption;
end;

procedure TfrmMain.btnTestClick(Sender: TObject);
var Buffer: ansichar; S: ansistring;
begin
  mmMain.Lines.Clear;
  Stream.Connect;

  repeat
    Stream.Read(Buffer);
    case Buffer of
      SpaceMarker: S := '<space>';
      LineMarker:  S := '<EoLn>';
      PageMarker:  S := '<EoPg>';
      FileMarker:  S := '<EoF>';
      else S := Buffer;
    end;
    mmMain.Lines.Add(S);
  until Stream.IsEoF;

  Stream.Disconnect;
end;

end.
