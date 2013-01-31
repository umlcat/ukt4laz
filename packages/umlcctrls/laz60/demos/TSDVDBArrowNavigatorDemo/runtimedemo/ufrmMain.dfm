object frmMain: TfrmMain
  Left = 262
  Top = 221
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
  OnActivate = FormActivate
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
    object btnOpen: TButton
      Left = 8
      Top = 8
      Width = 75
      Height = 25
      Caption = 'btnOpen'
      TabOrder = 1
      OnClick = btnOpenClick
    end
    object btnClose: TButton
      Left = 88
      Top = 8
      Width = 75
      Height = 25
      Caption = 'btnClose'
      TabOrder = 2
      OnClick = btnCloseClick
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
  object DBGrid1: TDBGrid
    Left = 0
    Top = 41
    Width = 688
    Height = 361
    Align = alClient
    DataSource = dsAnimals
    Options = [dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgRowSelect, dgAlwaysShowSelection, dgConfirmDelete, dgCancelOnExit]
    TabOrder = 2
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
  end
  object Navigator: TSDVDBNavigator
    Left = 0
    Top = 402
    Width = 688
    Height = 25
    Align = alBottom
    Enabled = True
    TabOrder = 3
    ButtonsVisible = [btnopFirst, btnopFastPrior, btnopPrior, btnopNext, btnopFastNext, btnopLast]
    ButtonsEnabled = [btnopFirst, btnopFastPrior, btnopPrior, btnopSearch, btnopNext, btnopFastNext, btnopLast]
    Delta = 10
    DataSource = dsAnimals
  end
  object dsAnimals: TDataSource
    DataSet = tbAnimals
    Left = 40
    Top = 360
  end
  object tbAnimals: TTable
    DatabaseName = 'DBDEMOS'
    TableName = 'animals.dbf'
    Left = 8
    Top = 360
  end
end
