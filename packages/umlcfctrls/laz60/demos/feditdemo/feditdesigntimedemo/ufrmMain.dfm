object frmMain: TfrmMain
  Left = 316
  Top = 185
  Width = 696
  Height = 480
  ActiveControl = SDVFocusEdit1
  Caption = 'frmMain'
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
      Caption = 'btnExit'
      TabOrder = 0
      Kind = bkClose
    end
  end
  object SDVFocusEdit1: TSDVFocusEdit
    Left = 8
    Top = 88
    Width = 121
    Height = 21
    Enabled = True
    ParentFont = False
    ReadOnly = False
    TabOrder = 2
    Manager = Manager
  end
  object SDVFocusEdit2: TSDVFocusEdit
    Left = 8
    Top = 120
    Width = 121
    Height = 21
    Enabled = True
    ParentFont = False
    ReadOnly = False
    TabOrder = 3
    Manager = Manager
  end
  object SDVFocusEdit3: TSDVFocusEdit
    Left = 8
    Top = 152
    Width = 121
    Height = 21
    Enabled = True
    ParentFont = False
    ReadOnly = False
    TabOrder = 4
    Manager = Manager
  end
  object SDVFocusEdit4: TSDVFocusEdit
    Left = 8
    Top = 184
    Width = 121
    Height = 21
    Enabled = True
    ParentFont = False
    ReadOnly = True
    TabOrder = 5
    Text = 'SDVFocusEdit4'
    Manager = Manager
  end
  object SDVFocusMemo1: TSDVFocusMemo
    Left = 8
    Top = 216
    Width = 185
    Height = 89
    AutoSize = False
    Enabled = True
    Lines.Strings = (
      'SDVFocusMemo1')
    ReadOnly = False
    TabOrder = 6
    Text = 'SDVFocusMemo1'
    Manager = Manager
  end
  object SDVFocusMemo2: TSDVFocusMemo
    Left = 8
    Top = 312
    Width = 185
    Height = 89
    AutoSize = False
    Enabled = True
    Lines.Strings = (
      'SDVFocusMemo2')
    ReadOnly = True
    TabOrder = 7
    Text = 'SDVFocusMemo2'
    Manager = Manager
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
