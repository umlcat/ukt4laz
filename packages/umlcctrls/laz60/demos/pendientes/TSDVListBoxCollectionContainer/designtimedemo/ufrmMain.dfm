object frmMain: TfrmMain
  Left = 257
  Top = 169
  BorderStyle = bsSingle
  Caption = 'TSDVListBoxCollectionContainer DesignTime Demo'
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
  PixelsPerInch = 96
  TextHeight = 13
  object sbStatusBar: TStatusBar
    Left = 0
    Top = 434
    Width = 688
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object pnTop: TPanel
    Left = 0
    Top = 0
    Width = 688
    Height = 41
    Align = alTop
    TabOrder = 1
    object btnExit: TBitBtn
      Left = 600
      Top = 8
      Width = 75
      Height = 25
      Caption = '&Exit'
      TabOrder = 0
      Kind = bkClose
    end
  end
  object lbListBox: TListBox
    Left = 96
    Top = 64
    Width = 121
    Height = 97
    ItemHeight = 13
    TabOrder = 2
  end
  object Container: TSDVListBoxCollectionContainer
    Left = 32
    Top = 72
  end
end
