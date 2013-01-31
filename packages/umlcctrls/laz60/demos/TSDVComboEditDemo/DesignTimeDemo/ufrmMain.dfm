object frmMain: TfrmMain
  Left = 256
  Top = 184
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
      Caption = 'btnExit'
      TabOrder = 0
      Kind = bkClose
    end
    object btnTest: TButton
      Left = 8
      Top = 8
      Width = 75
      Height = 25
      Caption = 'btnTest'
      TabOrder = 1
      OnClick = btnTestClick
    end
  end
  object Control: TSDVComboEdit
    Left = 8
    Top = 48
    Width = 121
    Height = 21
    BevelOuter = bvNone
    Enabled = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ReadOnly = False
    TabOrder = 1
    OnClick = ControlClick
    Glyph.Data = {
      F6000000424DF600000000000000760000002800000010000000100000000100
      0400000000008000000000000000000000001000000000000000000000000000
      8000008000000080800080000000800080008080000080808000C0C0C0000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      333333333444444433333333444444444333333444444444443333444FFFFFF4
      44433444448FFF4444433444448FFF4444443444448FFF44444434444FFFFF44
      444434444444444444443444448FF8444443334444FFFF4444433334448FF844
      4433333344444444433333333344444333333333333333333333}
  end
end
