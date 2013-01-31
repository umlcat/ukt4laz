object frmMain: TfrmMain
  Left = 246
  Top = 184
  Width = 696
  Height = 480
  Caption = 'TSDVHeaderPanel Demo'
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
      Caption = '&Exit'
      TabOrder = 0
      Kind = bkClose
    end
  end
  object Control: TSDVHeaderPanel
    Left = 8
    Top = 56
    Width = 187
    Height = 97
    BevelOuter = bvLowered
    Caption = 'Hello World'
    Enabled = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    TabOrder = 2
    ShowGrid = False
  end
end
