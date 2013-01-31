object frmMain: TfrmMain
  Left = 268
  Top = 172
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
  object Control: TSDVDropDownMenuButton
    Left = 64
    Top = 56
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
    ShowCaption = False
    ShowGlyph = True
    Actions = <
      item
        Action = acEditCopy
      end
      item
        Action = acEditCut
      end
      item
      end
      item
        Action = acEditPaste
      end>
    Images = imlsImages
    Orientation = doTop
    Direction = ddDown
    Position = dpRight
  end
  object pmHello: TPopupMenu
    Images = imlsImages
    Left = 16
    Top = 56
    object miEditCut: TMenuItem
      Action = acEditCut
    end
    object miEditCopy: TMenuItem
      Action = acEditCopy
    end
    object miEditPaste: TMenuItem
      Action = acEditPaste
    end
  end
  object imlsImages: TImageList
    Left = 16
    Top = 120
    Bitmap = {
      494C010104000900040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000003000000001001000000000000018
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000104210421042104210421042
      1042104210421042104210421042104200000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000104218631863186318631863
      1863186318631863186318631863104200000000000000000000100010000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000001000
      10001000100010001000100010001000100000001042FF7FFF7FFF7FFF7FFF7F
      FF7FFF7FFF7FFF7FFF7FFF7FFF7F104200000000000000001000000000001000
      0000000010001000000000000000000000000000000000000000000000000000
      1000100010001000100010001000100010000000000000000000000000001000
      FF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7F100000001042FF7F0000000000000000
      000000000000000000000000FF7F104200000000000000001000000000001000
      0000100000000000100000000000000000000000000000000000000000000000
      1000FF7FFF7FFF7FFF7FFF7FFF7FFF7F10000000104200421042004210421000
      FF7F000000000000000000000000FF7F100000001042FF7F1042FF7FFF7FFF7F
      FF7FFF7FFF7FFF7FFF7F0000FF7F104200000000000000001000000000001000
      0000100000000000100000000000000000000000000000000000000000000000
      1000FF7F00000000000000000000FF7F10000000004210420042104200421000
      FF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7F100000001042FF7F1042FF7FFF7F0040
      FF7FFF7F00020002FF7F0000FF7F104200000000000000000000100010001000
      0000100000000000100000000000000000000000000000000000000000000000
      1000FF7FFF7FFF7FFF7FFF7FFF7FFF7F10000000104200421042004210421000
      FF7F000000000000FF7F100010001000100000001042FF7F1042FF7F0040007C
      FF7F1F00FF031F00FF7F0000FF7F104200000000000000000000000000001000
      00001000100010000000000000000000000000000000FF7FFF7FFF7FFF7FFF7F
      1000FF7F00000000000000000000FF7F10000000004210420042104200421000
      FF7FFF7FFF7FFF7FFF7F1000FF7F1000000000001042FF7F1042FF7FE07F0040
      FF7FFF7F1F00FF7FFF7F0000FF7F104200000000000000000000000000001000
      00001000000000000000000000000000000000000000FF7F0000000000000000
      1000FF7FFF7FFF7FFF7FFF7FFF7FFF7F10000000104200421042004210421000
      FF7FFF7FFF7FFF7FFF7F100010000000000000001042FF7F1042FF7FFF7FFF7F
      FF7FFF7FFF7FFF7FFF7F0000FF7F104200000000000000000000000000000000
      00000000000000000000000000000000000000000000FF7FFF7FFF7FFF7FFF7F
      1000FF7F00000000FF7F10001000100010000000004210420042104200421000
      10001000100010001000100000000000000000001042FF7F1042100010001000
      100010001000100010000000FF7F104200000000000000000000000000000000
      00000000000000000000000000000000000000000000FF7F0000000000000000
      1000FF7FFF7FFF7FFF7F1000FF7F100000000000104200421042004210420042
      10420042104200421042004200000000000000001042FF7F10421000E07F1000
      100010001000100010000000FF7F104200000000000000000000000000000000
      00000000000000000000000000000000000000000000FF7FFF7FFF7FFF7FFF7F
      1000FF7FFF7FFF7FFF7F10001000000000000000004210420000000000000000
      00000000000000001042104200000000000000001042FF7F1042104210421042
      1042104210421042104200001863104200000000000000000000000000000000
      00000000000000000000000000000000000000000000FF7F00000000FF7F0000
      1000100010001000100010000000000000000000104210420000186318631863
      18631863186300001042004200000000000000001042FF7FFF7FFF7FFF7FFF7F
      FF7FFF7FFF7FFF7F104210421042104200000000000000000000000000000000
      00000000000000000000000000000000000000000000FF7FFF7FFF7FFF7F0000
      FF7F0000000000000000000000000000000000000042104200420000E07F0000
      0000E07F000010420042104200000000000000001042FF7FFF7FFF7FFF7FFF7F
      FF7FFF7FFF7FFF7F1042FF7F1042000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FF7FFF7FFF7FFF7F0000
      000000000000000000000000000000000000000000000000000000000000E07F
      E07F0000000000000000000000000000000000001042FF7FFF7FFF7FFF7FFF7F
      FF7FFF7FFF7FFF7F104210420000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000104210421042104210421042
      1042104210421042104200000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000300000000100010000000000800100000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000008001FFFFFFFFFFFF8001F3FFFFFFFC00
      8001ED9FFE0080008001ED6FFE0000008001ED6FFE0000008001F16F80000000
      8001FD1F800000018001FC7F800000038001FEFF800000038001FC7F80010003
      8001FD7F800300038001F93F800700038001FBBF807F00038003FBBF80FF8007
      8007FBBF81FFF87F800FFFFFFFFFFFFF00000000000000000000000000000000
      000000000000}
  end
  object aclsActions: TActionList
    Images = imlsImages
    Left = 16
    Top = 88
    object acEditCut: TAction
      Caption = 'acEditCut'
      ImageIndex = 1
      OnExecute = acEditCutExecute
    end
    object acEditCopy: TAction
      Caption = 'acEditCopy'
      ImageIndex = 2
      OnExecute = acEditCopyExecute
    end
    object acEditPaste: TAction
      Caption = 'acEditPaste'
      ImageIndex = 3
      OnExecute = acEditPasteExecute
    end
  end
end
