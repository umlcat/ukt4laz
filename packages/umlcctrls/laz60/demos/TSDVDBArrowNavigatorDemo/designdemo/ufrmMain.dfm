object frmMain: TfrmMain
  Left = 270
  Top = 235
  Width = 696
  Height = 480
  Caption = 'frmMain'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object pnTop: TPanel
    Left = 0
    Top = 0
    Width = 688
    Height = 41
    Align = alTop
    TabOrder = 0
    object btnExit: TButton
      Left = 608
      Top = 8
      Width = 75
      Height = 25
      Caption = 'btnExit'
      ModalResult = 2
      TabOrder = 0
      OnClick = btnExitClick
    end
  end
  object sbStatusBar: TStatusBar
    Left = 0
    Top = 427
    Width = 688
    Height = 19
    Panels = <>
    SimplePanel = False
  end
  object dbgrMain: TDBGrid
    Left = 0
    Top = 41
    Width = 688
    Height = 336
    Align = alTop
    DataSource = dsAnimals
    TabOrder = 2
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
  end
  object dsAnimals: TDataSource
    DataSet = tbAnimals
    Left = 48
    Top = 384
  end
  object tbAnimals: TTable
    DatabaseName = 'DBDEMOS'
    TableName = 'animals.dbf'
    Left = 16
    Top = 384
  end
end
