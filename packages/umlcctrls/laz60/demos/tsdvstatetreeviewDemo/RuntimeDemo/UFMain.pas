unit UFMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, ComCtrls, ImgList,
  sdvStateTreeViews;

type
  TFMain = class(TForm)
    sbStatusBar: TStatusBar;
    pnTop: TPanel;
    btnExit: TBitBtn;
    imStates: TImageList;
    procedure tvDemoClickStateImage(Sender: TObject; Node: TTreeNode);
    procedure tvDemoClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }

    IsActivated: Boolean;

    tvDemo: TSDVStateTreeView;
  end;

var
  FMain: TFMain;

implementation

{$R *.LFM}

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

procedure TFMain.FormCreate(Sender: TObject);
begin
  IsActivated := FALSE;
end;

procedure TFMain.FormActivate(Sender: TObject);
var ParentNode, Node: TTreeNode;
begin
  if (not IsActivated) then
  begin
    IsActivated := TRUE;

    tvDemo := TSDVStateTreeView.Create(Self);

    tvDemo.Top  := 41;
    tvDemo.Left := 0;
    tvDemo.Align := alClient;
    tvDemo.StateImages := imStates;
    tvDemo.HideSelection := FALSE;

    tvDemo.OnClick := {@}tvDemoClick;
    tvDemo.OnClickStateImage := {@}tvDemoClickStateImage;

    Self.InsertControl(tvDemo);

    ParentNode := tvDemo.Items.AddChild(nil, 'My PC');
    ParentNode.StateIndex := 1;

    Node := tvDemo.Items.AddChild(ParentNode, 'A:');
    Node.StateIndex := 1;

    Node := tvDemo.Items.AddChild(ParentNode, 'C:');
    Node.StateIndex := 1;

    Node := tvDemo.Items.AddChild(ParentNode, 'D:');
    Node.StateIndex := 1;

    ParentNode.Expand(TRUE);
    tvDemo.SetFocus;
    Application.ProcessMessages;
  end;
end;

end.
