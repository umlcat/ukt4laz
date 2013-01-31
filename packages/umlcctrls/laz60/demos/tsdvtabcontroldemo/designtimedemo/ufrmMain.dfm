object frmMain: TfrmMain
  Left = 249
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
  OnActivate = FormActivate
  OnCreate = FormCreate
  OnDestroy = FormDestroy
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
  end
  object sbStatusBar: TStatusBar
    Left = 0
    Top = 434
    Width = 688
    Height = 19
    Panels = <>
    SimplePanel = False
  end
  object pcMain: TPageControl
    Left = 0
    Top = 41
    Width = 688
    Height = 393
    ActivePage = tbshFive
    Align = alClient
    TabOrder = 2
    object tbshOne: TTabSheet
      Caption = 'tbshOne'
      TabVisible = False
      object mmOne: TMemo
        Left = 0
        Top = 0
        Width = 680
        Height = 383
        Align = alClient
        Lines.Strings = (
          'This is page One')
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
    object tbshTwo: TTabSheet
      Caption = 'tbshTwo'
      ImageIndex = 1
      TabVisible = False
      object mmTwo: TMemo
        Left = 0
        Top = 0
        Width = 680
        Height = 383
        Align = alClient
        Lines.Strings = (
          'This is page Two')
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
    object tbshThree: TTabSheet
      Caption = 'tbshThree'
      ImageIndex = 2
      TabVisible = False
      object mmThree: TMemo
        Left = 0
        Top = 0
        Width = 680
        Height = 383
        Align = alClient
        Lines.Strings = (
          'This is page Three')
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
    object tbshFour: TTabSheet
      Caption = 'tbshFour'
      ImageIndex = 3
      TabVisible = False
      object mmFour: TMemo
        Left = 0
        Top = 0
        Width = 680
        Height = 383
        Align = alClient
        Lines.Strings = (
          'This is page Four')
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
    object tbshFive: TTabSheet
      Caption = 'tbshFive'
      ImageIndex = 4
      TabVisible = False
      object mmFive: TMemo
        Left = 0
        Top = 0
        Width = 680
        Height = 383
        Align = alClient
        Lines.Strings = (
          'This is page Five')
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
  end
end
