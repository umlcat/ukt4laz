object frmMain: TfrmMain
  Left = 269
  Top = 234
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
  object SDVNavigator1: TSDVNavigator
    Left = 0
    Top = 402
    Width = 688
    Height = 25
    Align = alBottom
    Enabled = True
    TabOrder = 2
    ButtonsVisible = [btnopFirst, btnopFastPrior, btnopPrior, btnopSearch, btnopNext, btnopFastNext, btnopLast]
    ButtonsEnabled = [btnopFirst, btnopFastPrior, btnopPrior, btnopSearch, btnopNext, btnopFastNext, btnopLast]
    Delta = 10
  end
end
