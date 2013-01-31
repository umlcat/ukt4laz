object frmMain: TfrmMain
  Left = 257
  Top = 166
  BorderStyle = bsSingle
  Caption = 'TSDVObjectComboBox RunTime Demo'
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
    object btnExit: TBitBtn
      Left = 608
      Top = 8
      Width = 75
      Height = 25
      Caption = '&Exit'
      TabOrder = 0
      Kind = bkClose
    end
    object btnTest: TButton
      Left = 8
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Test'
      TabOrder = 1
      OnClick = btnTestClick
    end
  end
  object sbStatusBar: TStatusBar
    Left = 0
    Top = 434
    Width = 688
    Height = 19
    Panels = <>
    SimplePanel = False
  end
end
