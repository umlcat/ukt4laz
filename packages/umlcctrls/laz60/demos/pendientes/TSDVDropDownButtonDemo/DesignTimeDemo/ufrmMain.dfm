object frmMain: TfrmMain
  Left = 256
  Top = 184
  Width = 696
  Height = 480
  Caption = 'frmMain'
  Color = clAppWorkSpace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
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
    Top = 434
    Width = 688
    Height = 19
    Panels = <>
    SimplePanel = False
  end
  object DropDownButton1: TSDVDropDownButton
    Left = 32
    Top = 72
    Width = 35
    Height = 22
    Align = alNone
    BevelOuter = bvNone
    Enabled = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    TabOrder = 2
    OnClick = DropDownButton1Click
    ShowCaption = False
    ShowGlyph = True
    Orientation = doTop
    Direction = ddDown
    Position = dpRight
    OnDropDown = DropDownButton1DropDown
  end
  object DropDownButton2: TSDVDropDownButton
    Left = 32
    Top = 128
    Width = 35
    Height = 22
    Align = alNone
    BevelOuter = bvNone
    Enabled = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    TabOrder = 3
    OnClick = DropDownButton2Click
    ShowCaption = False
    ShowGlyph = True
    Orientation = doTop
    Direction = ddDown
    Position = dpRight
    OnDropDown = DropDownButton2DropDown
    OnPopUpControlWidth = DropDownButton2PopUpControlWidth
    OnPopUpControlHeight = DropDownButton2PopUpControlHeight
  end
end
