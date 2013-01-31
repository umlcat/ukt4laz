unit UFMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, ComCtrls, ImgList, sdvStateTreeViews;

type
  TFMain = class(TForm)
    sbStatusBar: TStatusBar;
    pnTop: TPanel;
    btnExit: TBitBtn;
    imStates: TImageList;
    tvDemo: TSDVStateTreeView;
    procedure tvDemoClickStateImage(Sender: TObject; Node: TTreeNode);
    procedure tvDemoClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FMain: TFMain;

implementation

{$R *.DFM}

procedure TFMain.tvDemoClickStateImage(Sender: TObject; Node: TTreeNode);
begin
  ShowMessage(#34 + Node.Text + #34' state image clicked.');

  if Node.StateIndex = 3
    then Node.StateIndex := 1
    else Node.StateIndex := Succ(Node.StateIndex);
end;

procedure TFMain.tvDemoClick(Sender: TObject);
begin
  ShowMessage(#34 + tvDemo.Selected.Text + #34' text clicked.');
end;

end.
