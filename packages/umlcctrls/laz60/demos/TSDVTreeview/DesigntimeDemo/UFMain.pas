unit UFMain;

interface
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ImgList, Buttons, ComCtrls,
  sdvComponents, sdvTreeStates, vclsdvTreenodes, vclsdvTreeviews,
  vclsdvPanels;

type
  TFMain = class(TForm)
    pnTop: TPanel;
    btnExit: TBitBtn;
    btnItemEnabled: TButton;
    btnItemCaption: TButton;
    btnEnabled: TButton;
    sbStatusBar: TStatusBar;
    imlsImages: TImageList;
    tvDemo: TSDVTreeView;
    btnClear: TButton;
    procedure FormShow(Sender: TObject);
    procedure btnItemCaptionClick(Sender: TObject);
    procedure btnItemEnabledClick(Sender: TObject);
    procedure btnEnabledClick(Sender: TObject);
    procedure tvDemoExplore(const Node: TSDVTreeNode);
    procedure tvDemoClick(Sender: TObject);
    procedure tvDemoBeforeCollapse(const Node: TSDVTreeNode);
    procedure tvDemoBeforeExpand(const Node: TSDVTreeNode);
    procedure tvDemoAfterCollapse(const Node: TSDVTreeNode);
    procedure tvDemoAfterExpand(const Node: TSDVTreeNode);
    procedure FormCreate(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure tvDemoBeforeEmpty(const Node: TSDVTreeNode);
    procedure tvDemoAfterEmpty(const Node: TSDVTreeNode);
  private
    { Private declarations }
  public
    { Public declarations }

    procedure ExtractFullPath(const Node: TSDVTreeNode; const Param: pointer);
    procedure Agregar
     (const Node: TSDVTreeNode; const ACaption: string; const AIsContainer: Boolean);
  end;

var
  FMain: TFMain;

implementation

{$R *.DFM}

procedure TFMain.FormShow(Sender: TObject);
var RootNode: TSDVTreeViewNode;
begin
  tvDemo.Activated := TRUE;

  RootNode := (tvDemo.Items.Root as TSDVTreeViewNode);

  RootNode.Caption := 'My PC';
  RootNode.IsContainer := TRUE;
  RootNode.DisabledIndex := 5;
  RootNode.ImageIndex    := 6;
  RootNode.KeepIndex     := 7;
  RootNode.SelectedIndex := 8;
  Application.ProcessMessages;

  tvDemo.Items.Root.Explore;
  Application.ProcessMessages;
end;

procedure TFMain.btnItemCaptionClick(Sender: TObject);
var Node: TSDVTreeViewNode;
begin
  Node := tvDemo.Selected;
  if (Assigned(Node)) then
  begin
    ShowMessage(Node.Caption);
  end;
end;

procedure TFMain.btnItemEnabledClick(Sender: TObject);
var Node: TSDVTreeViewNode;
begin
  Node := tvDemo.Selected;
  if (Assigned(Node)) then
  begin
    Node.Enabled := not Node.Enabled;
  end;
//  tvDemo.Selected := tvDemo.Items.Root as TSDVTreeViewNode;
  if (tvDemo.ShowRoot)
    then tvDemo.Selected := tvDemo.Items.Root as TSDVTreeViewNode;
end;

procedure TFMain.btnEnabledClick(Sender: TObject);
begin
  tvDemo.Enabled := not tvDemo.Enabled;
end;

procedure TFMain.tvDemoExplore(const Node: TSDVTreeNode);
var NewNode: TSDVTreeViewNode;
    Path: string; Attr, ErrorCode: Integer; F: TSearchRec;
begin
  if (Node = tvDemo.Items.Root) then
  begin
    NewNode := (Node.Insert as TSDVTreeViewNode);
    NewNode.Caption := 'A:';
    NewNode.IsContainer := TRUE;
    NewNode.DisabledIndex := 5;
    NewNode.ImageIndex    := 6;
    NewNode.KeepIndex     := 7;
    NewNode.SelectedIndex := 8;

    NewNode := (Node.Insert as TSDVTreeViewNode);
    NewNode.Caption := 'C:';
    NewNode.IsContainer := TRUE;
    NewNode.DisabledIndex := 5;
    NewNode.ImageIndex    := 6;
    NewNode.KeepIndex     := 7;
    NewNode.SelectedIndex := 8;

    NewNode := (Node.Insert as TSDVTreeViewNode);
    NewNode.Caption := 'D:';
    NewNode.IsContainer := TRUE;
    NewNode.DisabledIndex := 5;
    NewNode.ImageIndex    := 6;
    NewNode.KeepIndex     := 7;
    NewNode.SelectedIndex := 8;
  end else if (Node.Caption = 'A:') or (Node.Caption = 'D:') then
  begin
    ShowMessage('Error: Diskette or CD not found.');
  end else if (Node.IsContainer) then
  begin
    Path := '';
    Node.ForUp(ExtractFullPath, @Path);
    Path := Path + '\*.*';
    Attr := (faAnyFile - faVolumeID - faHidden);

    ErrorCode := FindFirst(Path, Attr, F);
    if (ErrorCode = 0) then
    begin
      Agregar(Node, F.Name, (F.Attr and faDirectory) <> 0);

      while (FindNext(F) = 0) do
        Agregar(Node, F.Name, (F.Attr and faDirectory) <> 0)
    end;
    FindClose(F);
  end else ShowMessage('No Items');
  Application.ProcessMessages;
end;

procedure TFMain.tvDemoClick(Sender: TObject);
var Node: TSDVTreeViewNode absolute Sender;
begin
  ShowMessage(Node.Caption);
end;

procedure TFMain.tvDemoBeforeCollapse(const Node: TSDVTreeNode);
begin
  ShowMessage('BeforeCollapse');
end;

procedure TFMain.tvDemoBeforeExpand(const Node: TSDVTreeNode);
begin
  ShowMessage('BeforeExpand');
end;

procedure TFMain.tvDemoAfterCollapse(const Node: TSDVTreeNode);
begin
  ShowMessage('AfterCollapse');
end;

procedure TFMain.tvDemoAfterExpand(const Node: TSDVTreeNode);
begin
  ShowMessage('AfterExpand');
end;

procedure TFMain.ExtractFullPath(const Node: TSDVTreeNode; const Param: pointer);
var Path: ^string absolute Param;
begin
  if (not Node.IsRoot) then
  begin
    if (Path^ = '')
      then Path^ := (Node as TSDVTreeViewNode).Caption
      else Path^ := (Node as TSDVTreeViewNode).Caption + '\' + Path^;
  end;
end;

procedure TFMain.Agregar
  (const Node: TSDVTreeNode; const ACaption: string; const AIsContainer: Boolean);
var NewNode: TSDVTreeViewNode;
begin
  if ((ACaption <> '.') and (ACaption <> '..')) then
  begin
    NewNode := (Node.Insert as TSDVTreeViewNode);
    NewNode.Caption := ACaption;
    NewNode.IsContainer := AIsContainer;

    if (NewNode.IsContainer) then
    begin
      NewNode.DisabledIndex := 5;
      NewNode.ImageIndex    := 6;
      NewNode.KeepIndex     := 7;
      NewNode.SelectedIndex := 8;
      NewNode.Enabled       := TRUE;
    end else
    begin
      NewNode.DisabledIndex := 1;
      NewNode.ImageIndex    := 2;
      NewNode.KeepIndex     := 3;
      NewNode.SelectedIndex := 4;
      NewNode.Enabled       := TRUE;

      NewNode.State := tsEmpty;
      NewNode.Treenode.StateIndex := siEmpty;
    end;
  end;
end;

procedure TFMain.FormCreate(Sender: TObject);
begin
  tvDemo.ShowRoot := TRUE;
//  tvDemo.ShowRoot := FALSE;
end;

procedure TFMain.btnClearClick(Sender: TObject);
var Node: TSDVTreeViewNode;
begin
  Node := tvDemo.Selected;
  if (Assigned(Node)) then
  begin
    Node.Empty;
  end;
end;

procedure TFMain.tvDemoBeforeEmpty(const Node: TSDVTreeNode);
begin
  ShowMessage('BeforeClear');
end;

procedure TFMain.tvDemoAfterEmpty(const Node: TSDVTreeNode);
begin
  ShowMessage('AfterClear');
end;

end.
