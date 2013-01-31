object frmMain: TfrmMain
  Left = 274
  Top = 216
  ActiveControl = edMessage
  BorderStyle = bsSingle
  Caption = 'InputStrDemo'
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
    Width = 121
    Height = 21
    TabOrder = 0
  end
  object btnInputStr: TButton
    Left = 8
    Top = 48
    Width = 75
    Height = 25
    Caption = 'Input String'
    TabOrder = 1
    OnClick = btnInputStrClick
  end
  object btnExit: TBitBtn
    Left = 200
    Top = 16
    Width = 75
    Height = 25
    Caption = '&Exit'
    TabOrder = 4
    Kind = bkClose
  end
  object btnInputInt: TButton
    Left = 8
    Top = 80
    Width = 75
    Height = 25
    Caption = 'Input Integer'
    TabOrder = 3
    OnClick = btnInputIntClick
  end
  object btnInputPassword: TButton
    Left = 88
    Top = 48
    Width = 75
    Height = 25
    Caption = 'Input Passw.'
    TabOrder = 2
    OnClick = btnInputPasswordClick
  end
  object btnInputDate: TButton
    Left = 8
    Top = 112
    Width = 75
    Height = 25
    Caption = 'Input Date'
    TabOrder = 5
    OnClick = btnInputDateClick
  end
  object btnInputTime: TButton
    Left = 88
    Top = 112
    Width = 75
    Height = 25
    Caption = 'Input Time'
    TabOrder = 6
    OnClick = btnInputTimeClick
  end
end
