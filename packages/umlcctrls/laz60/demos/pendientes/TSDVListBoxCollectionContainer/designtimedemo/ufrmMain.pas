unit ufrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, sdvComponents, vclsdvDlgCtrls, vclsdvCollectionListBoxes,
  ComCtrls, StdCtrls, Buttons, ExtCtrls;

type
  TfrmMain = class(TForm)
    sbStatusBar: TStatusBar;
    Container: TSDVListBoxCollectionContainer;
    pnTop: TPanel;
    btnExit: TBitBtn;
    lbListBox: TListBox;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

end.
