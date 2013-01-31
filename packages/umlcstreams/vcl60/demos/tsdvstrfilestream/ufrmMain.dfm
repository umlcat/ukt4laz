object frmMain: TfrmMain
  Left = 286
  Top = 213
  BorderStyle = bsSingle
  Caption = 'frmMain'
  ClientHeight = 453
  ClientWidth = 688
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object sbStatusBar: TStatusBar
    Left = 0
    Top = 434
    Width = 688
    Height = 19
    Panels = <>
    SimplePanel = False
  end
  object pnTop: TPanel
    Left = 0
    Top = 0
    Width = 688
    Height = 41
    Align = alTop
    TabOrder = 1
    object btnExit: TBitBtn
      Left = 608
      Top = 8
      Width = 75
      Height = 25
      Caption = 'btnExit'
      TabOrder = 0
      Kind = bkClose
    end
    object btnNew: TBitBtn
      Left = 8
      Top = 8
      Width = 75
      Height = 25
      Caption = 'btnNew'
      TabOrder = 1
      OnClick = btnNewClick
    end
    object btnTest: TButton
      Left = 88
      Top = 8
      Width = 75
      Height = 25
      Caption = 'btnTest'
      TabOrder = 2
      OnClick = btnTestClick
    end
  end
  object mmMain: TMemo
    Left = 0
    Top = 41
    Width = 688
    Height = 393
    Align = alClient
    ScrollBars = ssBoth
    TabOrder = 2
  end
end
