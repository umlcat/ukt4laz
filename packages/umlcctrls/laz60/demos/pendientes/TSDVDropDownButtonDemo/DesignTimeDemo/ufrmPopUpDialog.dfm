object frmPopUpDialog: TfrmPopUpDialog
  Left = 245
  Top = 166
  BorderStyle = bsNone
  Caption = 'frmPopUpDialog'
  ClientHeight = 154
  ClientWidth = 288
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefault
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object tvItems: TTreeView
    Left = 0
    Top = 0
    Width = 288
    Height = 154
    Align = alClient
    Images = dmDataModule.imlsItems
    Indent = 19
    ReadOnly = True
    RowSelect = True
    ShowButtons = False
    ShowLines = False
    ShowRoot = False
    StateImages = dmDataModule.imlsItems
    TabOrder = 0
    OnClick = tvItemsClick
    OnDblClick = tvItemsDblClick
    OnKeyDown = tvItemsKeyDown
    Items.Data = {
      030000001C000000020000000200000001000000FFFFFFFF0000000000000000
      034375741D000000030000000300000001000000FFFFFFFF0000000000000000
      04436F70791E000000040000000400000001000000FFFFFFFF00000000000000
      00055061737465}
  end
end
