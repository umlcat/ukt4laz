object frmMain: TfrmMain
  Left = 258
  Top = 187
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
  object sbStatusBar: TStatusBar
    Left = 0
    Top = 431
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
    object btnConnect: TButton
      Left = 8
      Top = 8
      Width = 75
      Height = 25
      Caption = 'btnConnect'
      TabOrder = 1
      OnClick = btnConnectClick
    end
    object btnDisconnect: TButton
      Left = 88
      Top = 8
      Width = 75
      Height = 25
      Caption = 'btnDisconnect'
      TabOrder = 2
      OnClick = btnDisconnectClick
    end
  end
  object Manager: TSDVFocusMngr
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    FocusedFont.Charset = DEFAULT_CHARSET
    FocusedFont.Color = clLime
    FocusedFont.Height = -11
    FocusedFont.Name = 'MS Sans Serif'
    FocusedFont.Style = []
    DisabledColor = clSilver
    DisabledFont.Charset = DEFAULT_CHARSET
    DisabledFont.Color = clBlack
    DisabledFont.Height = -11
    DisabledFont.Name = 'MS Sans Serif'
    DisabledFont.Style = []
    ReadOnlyColor = clSilver
    ReadOnlyFont.Charset = DEFAULT_CHARSET
    ReadOnlyFont.Color = clBlack
    ReadOnlyFont.Height = -11
    ReadOnlyFont.Name = 'MS Sans Serif'
    ReadOnlyFont.Style = []
    Left = 8
    Top = 48
  end
end
