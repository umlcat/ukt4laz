unit UFMain;

interface
uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ComCtrls, StdCtrls, Buttons, ImgList,
  TreeStates, sdvTreenodes, sdvTreeviews, sdvObjectTreeviews;

type
  TFMain = class(TForm)
    sbStatusBar: TStatusBar;
    pnTop: TPanel;
    btnExit: TBitBtn;
    imlsImages: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }

    tvDemo: TCustomSDVObjectTreeView;

    procedure tvDemoExplore(const Node: TSDVTreeNode);

    procedure ExtractFullPath(const Node: TSDVTreeNode; const Param: pointer);
    procedure Agregar
     (const Node: TSDVTreeNode; const ACaption: string; const AIsContainer: Boolean);
  end;

var
  FMain: TFMain;

implementation

{$R *.dfm}

procedure TFMain.FormCreate(Sender: TObject);
var RootNode: TSDVTreeViewNode;
begin
  tvDemo := TCustomSDVObjectTreeView.Create(Self);
  tvDemo.Align     := alClient;
  tvDemo.Images    := imlsImages;
  tvDemo.ShowRoot  := TRUE;
  tvDemo.OnExplore := {@}tvDemoExplore;

  RootNode := (tvDemo.Items.Root as TSDVTreeViewNode);
  RootNode.Caption := 'My PC';
  RootNode.IsContainer := TRUE;
  RootNode.DisabledIndex := 5;
  RootNode.ImageIndex    := 6;
  RootNode.KeepIndex     := 7;
  RootNode.SelectedIndex := 8;

  Self.InsertControl(tvDemo);
end;

procedure TFMain.FormShow(Sender: TObject);
begin
  tvDemo.Activated := TRUE;
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
  end else if ((Node.Caption = 'A:') or (Node.Caption = 'D:')) then
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

end.
