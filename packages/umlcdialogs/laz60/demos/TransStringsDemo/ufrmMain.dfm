object frmMain: TfrmMain
  Left = 361
  Top = 332
  ActiveControl = edMessage
  BorderStyle = bsSingle
  Caption = 'InputStrDemo'
  ClientHeight = 113
  ClientWidth = 318
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
  object lbMessage: TLabel
    Left = 8
    Top = 16
    Width = 46
    Height = 13
    Caption = 'Message:'
  end
  object edMessage: TEdit
    Left = 72
    Top = 16
    Width = 233
    Height = 21
    TabOrder = 0
  end
  object btnInputStr: TButton
    Left = 152
    Top = 48
    Width = 75
    Height = 25
    Caption = 'Input String'
    TabOrder = 1
    OnClick = btnInputStrClick
  end
  object btnExit: TBitBtn
    Left = 232
    Top = 80
    Width = 75
    Height = 25
    Caption = '&Exit'
    TabOrder = 2
    Kind = bkClose
  end
  object btnInputStrings: TButton
    Left = 232
    Top = 48
    Width = 75
    Height = 25
    Caption = 'InputStrings'
    TabOrder = 3
    OnClick = btnInputStringsClick
  end
end
