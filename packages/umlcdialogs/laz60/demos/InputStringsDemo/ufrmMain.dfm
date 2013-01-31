object frmMain: TfrmMain
  Left = 366
  Top = 300
  BorderStyle = bsSingle
  Caption = 'InputStringsDemo'
  ClientHeight = 261
  ClientWidth = 344
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  DesignSize = (
    344
    261)
  PixelsPerInch = 96
  TextHeight = 13
  object lblMessage: TLabel
    Left = 7
    Top = 10
    Width = 59
    Height = 13
    Caption = 'Contact List:'
  end
  object btnExit: TBitBtn
    Left = 262
    Top = 208
    Width = 75
    Height = 25
    Caption = '&Exit'
    TabOrder = 0
    Kind = bkClose
  end
  object btnInputStrings: TButton
    Left = 182
    Top = 208
    Width = 75
    Height = 25
    Caption = 'Input Strings'
    TabOrder = 1
    OnClick = btnInputStringsClick
  end
  object sbStatusBar: TStatusBar
    Left = 0
    Top = 242
    Width = 344
    Height = 19
    Panels = <>
    SimplePanel = False
  end
  object lbMessage: TListBox
    Left = 8
    Top = 32
    Width = 329
    Height = 161
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    Items.Strings = (
      'John Doe'
      'Sara Mitchel'
      'Anne Douglas'
      'Mike Murray'
      'Peter O'#39'Hara')
    TabOrder = 2
  end
end
