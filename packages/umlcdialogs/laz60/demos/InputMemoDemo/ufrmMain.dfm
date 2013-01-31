object frmMain: TfrmMain
  Left = 366
  Top = 300
  BorderStyle = bsSingle
  Caption = 'InputMemoDemo'
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
  object lbMessage: TLabel
    Left = 7
    Top = 10
    Width = 46
    Height = 13
    Caption = 'Message:'
    FocusControl = mmMessage
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
  object btnInputMemo: TButton
    Left = 182
    Top = 208
    Width = 75
    Height = 25
    Caption = 'Input Memo'
    TabOrder = 1
    OnClick = btnInputMemoClick
  end
  object mmMessage: TMemo
    Left = 8
    Top = 32
    Width = 329
    Height = 161
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssBoth
    TabOrder = 2
  end
  object sbStatusBar: TStatusBar
    Left = 0
    Top = 242
    Width = 344
    Height = 19
    Panels = <>
    SimplePanel = False
  end
end
