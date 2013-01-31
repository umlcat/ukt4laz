unit ufrmPopUpDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ImgList;

type
  TfrmPopUpDialog = class(TForm)
    tvItems: TTreeView;
    procedure tvItemsDblClick(Sender: TObject);
    procedure tvItemsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure tvItemsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }

    Option: Integer;

    procedure PopUpExit;
  end;

  function Execute(const X, Y, W, H: Integer; var AOption: Integer): Boolean;

implementation

uses udmDataModule;

{$R *.dfm}

function Execute(const X, Y, W, H: Integer; var AOption: Integer): Boolean;
var Form: TfrmPopUpDialog;
begin
  Application.CreateForm(TfrmPopUpDialog, Form);
    Form.Top := Y;
    Form.Left := X;
    Form.Width := W;
    Form.Height := H;
    Form.Option := AOption;
    Result := (Form.ShowModal = mrOK);
    if (Result)
      then AOption := Form.Option;
  Form.Release; Form.Free; Form := nil;
end;

procedure TfrmPopUpDialog.tvItemsDblClick(Sender: TObject);
begin
  PopUpExit;
end;

procedure TfrmPopUpDialog.tvItemsClick(Sender: TObject);
begin
  PopUpExit;
end;

procedure TfrmPopUpDialog.FormCreate(Sender: TObject);
begin
  tvItems.Selected := tvItems.Items[Option];
end;

procedure TfrmPopUpDialog.tvItemsKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_RETURN)
    then PopUpExit;
end;

procedure TfrmPopUpDialog.PopUpExit;
begin
  Option := tvItems.Selected.Index;
  ModalResult := mrOK;
end;

end.
