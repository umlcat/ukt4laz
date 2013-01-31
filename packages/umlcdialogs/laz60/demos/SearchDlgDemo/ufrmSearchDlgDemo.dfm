object frmSearchDlgDemo: TfrmSearchDlgDemo
  Left = 247
  Top = 187
  Width = 696
  Height = 480
  Caption = 'frmSearchDlgDemo'
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
  object sbStatusBar: TStatusBar
    Left = 0
    Top = 427
    Width = 688
    Height = 19
    Panels = <>
    SimplePanel = False
  end
  object SearchDlg: TSDVSearchTextDialog
    HelpContext = 0
    Direction = srdForward
    Scope = srscpGlobal
    Origin = sropFromCursor
    ShowHelp = True
    ShowCaseSensitive = True
    ShowWholeWordsOnly = True
    ShowRegularExpressions = True
    WantCaseSensitive = False
    WantWholeWordsOnly = False
    WantRegularExpressions = False
    Left = 16
    Top = 392
  end
end
