object FMain: TFMain
  Left = 264
  Top = 210
  BorderStyle = bsSingle
  Caption = 'TSDVObjectTreeview Runtime Demo'
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
  OnCreate = FormCreate
  OnShow = FormShow
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
      Caption = '&Exit'
      TabOrder = 0
      Kind = bkClose
    end
  end
  object imlsImages: TImageList
    Left = 88
    Top = 10
    Bitmap = {
      494C010109000A00040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000003000000001001000000000000018
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000007C0040007C0040007C0000000000000000000000000000000000000000
      0000100010000000000000001000100000000000000000000042E07F0042E07F
      FF7F0000FF7FE07F0042E07F0042000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      007C007C007C007C007C007C007C000000000000000000000000000000000000
      00001000100000000000000010001000000000000000E07F0042E07FFF7F0000
      0000000000000000FF7FE07F0042E07F00000000000010421042104210421042
      104210421042104210421042104200000000000000000000000000000000FF7F
      007C007C007C007C007C007C007C007C00000000000000000000000000000000
      00001000100000000000000010001000000000000000E07FFF7F000000000000
      000000000000000000000000FF7FE07F0000000000001042FF7FE07F1863E07F
      1863E07F1863E07F1863E07F104200000000000000000000000000000000FF7F
      007C1863FF7F1863FF7F1863007C004000000000000000000000000000000000
      0000000010001000000010001000000000000000000000000000000000000000
      10001000100000000000000000000000000000001042FF7FE07F1863E07F1863
      E07F1863E07F1863E07F1863000010420000000000000000000000000000FF7F
      007C007C007C007C007C007C007C007C00000000000000000000000000000000
      0000000000000000000000000000000000000000104200000000000010001000
      00000000000010001000000000000000104200001042FF7F1863E07F1863E07F
      1863E07F1863E07F18631042000010420000000000000000000000000000FF7F
      007C1863FF7F1863FF7F1863007C004000000000000000000000000000000000
      0000000000000000000000000000000000000000104200001000100000000000
      1042104210420000000010001000000010421042FF7F1863E07F1863E07F1863
      E07F1863E07F1863E07F0000104210420000000000000000000000000000FF7F
      007C007C007C007C007C007C007C007C00000000000000000000100010000000
      10001000000000000000000000000000000000000000104200000000FF03FF03
      104210421042FF03FF0300000000104200001042FF7FFF7FFF7FFF7FFF7FFF7F
      FF7FFF7FFF7FFF7F104200001863104200000000000000000000000000000000
      FF7F007C007C007C007C007C007C000000000000000000001000100000000000
      000010001000000000000000000000000000000000001042FF7FFF7F00000000
      FF03FF03FF0300000000FF7FFF7F104200001042104210421042104210421042
      104210421042104210421042E07F104200000000000000000000000000000000
      0000FF7FFF7FFF7FFF7FFF7F0000000000000000000000001000100000000000
      0000100010000000000000000000000000000000000000001042FF7FFF7FFF7F
      000000000000FF7FFF7FFF7F10420000000000001042FF7F1863E07F1863E07F
      1863E07F1863E07F1863E07F1863104200000000000000000000000000000000
      0000000000000000000000000000000000000000000000001000100000000000
      0000100010000000000000000000000000000000000000001042FF7FFF7FFF7F
      FF7FFF7FFF7FFF7FFF7FFF7F10420000000000001042FF7FE07F1863E07F1863
      E07F1863FF7FFF7FFF7FFF7FFF7F10420000EF01EF01EF01EF01EF01EF01EF01
      EF01EF01EF01EF01EF01EF01EF01EF01EF01EF01EF01EF01EF01EF01EF01EF01
      EF01EF01EF01EF01EF01EF01EF01EF01EF01EF01EF01EF01EF01EF01EF01EF01
      EF01FF7FFF7FFF7FFF7F104200000000000000001042FF7F1863E07F1863E07F
      1863FF7F1042104210421042104210420000FF7FFF7FFF7FFF7FFF7FFF7FFF7F
      FF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7F7B6FFF7F
      F75EF75EEF3D0000F75EFF7FF75EFF7FF75EF75EEF3D0000EF01EF01EF01EF01
      EF01EF01EF01EF01EF01EF01EF01EF01EF01000000001042FF7FFF7FFF7FFF7F
      FF7F10420000000000000000000000000000EF01EF01EF01EF01EF01EF01EF01
      EF01EF01EF01EF01EF01EF01EF01EF01EF01EF01EF01EF01EF01EF01EF01EF01
      EF01EF01EF01EF01EF01EF01EF01EF01EF01EF01EF01EF01EF01EF01EF01EF01
      EF01FF7FFF7FFF7F104200000000000000000000000000001042104210421042
      104200000000000000000000000000000000FF7FFF7FFF7FFF7FFF7FFF7FFF7F
      FF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7F7B6FFF7F
      F75EF75EEF3D0000FF7FF75EF75EFF7FF75EF75EEF3D0000EF01EF01EF01EF01
      EF01EF01EF01EF01EF01EF01EF01EF01EF010000000000000000000000000000
      000000000000000000000000000000000000EF01EF01EF01EF01EF01EF01EF01
      EF01EF01EF01EF01EF01EF01EF01EF01EF01EF01EF01EF01EF01EF01EF01EF01
      EF01EF01EF01EF01EF01EF01EF01EF01EF01EF01EF01EF01EF01EF01EF01EF01
      EF01FF7FFF7F00001F7C1F7C1F7C1F7C1F7C0000000000000000000000000000
      000000000000000000000000000000000000FF7FFF7FFF7FFF7FFF7FFF7FFF7F
      FF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7F7B6FFF7F
      F75EF75EEF3D0000F75EFF7FF75EFF7FF75EF75EEF3D0000EF01EF01EF01EF01
      EF01EF01EF01EF01EF01EF01EF01EF01EF010000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000EF3DF75EF75EF75EF75EF75E
      F75EF75EF75EF75EF75EF75E0000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000EF3DEF3DEF3DEF3D
      EF3DEF3DEF3DEF3DEF3DEF3DEF3DEF3DEF3D0000EF3DFF7FFF7FFF7FFF7FFF7F
      FF7FFF7FFF7FFF7FFF7FF75E0000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000F75EF75EF75EF75EF75E
      F75EF75EF75EF75EF75EF75EF75EEF3DEF3D0000EF3DFF7FFF7FFF7FFF7FFF7F
      FF7FFF7FFF7FFF7FFF7FF75E0000000000000000EF3DEF3DEF3DEF3DEF3DEF3D
      EF3DEF3DEF3DEF3DEF3DEF3DEF3DEF3D00000000000000000000000000000000
      00000000000000000000000000000000000000000000F75EFF7FFF7FF75EFF7F
      F75EFF7FF75EFF7FF75EFF7FF75EEF3DEF3D0000EF3DFF7FFF7F000000000000
      000000000000FF7FFF7FF75E000000000000F75EF75EF75EF75EF75EF75EF75E
      F75EF75EF75EF75EF75EF75EF75EEF3D00001042104210421042104210421042
      1042104210421042104210421042000000000000F75EFF7FFF7FF75EFF7FF75E
      FF7FF75EFF7FF75EFF7FF75EEF3DF75EEF3D0000EF3DFF7FFF7FFF7FFF7FFF7F
      FF7FFF7FFF7FFF7FFF7FF75E000000000000F75EFF7FFF7FF75EFF7FF75EFF7F
      F75EFF7FF75EFF7FF75EFF7FF75EEF3D00001042FF7FE07F1863E07F1863E07F
      1863E07F1863E07F1863E07F1042000000000000F75EFF7FF75EFF7FF75EFF7F
      F75EFF7FF75EFF7FF75EFF7FEF3DF75EEF3D0000EF3DFF7FFF7F000000000000
      000000000000FF7FFF7FF75E000000000000F75EFF7FF75EFF7FF75EFF7FF75E
      FF7FF75EFF7FF75EFF7FF75EF75EEF3D00001042FF7F1863E07F1863E07F1863
      E07F1863E07F1863E07F1863104200000000F75EFF7FF75EFF7FF75EFF7FF75E
      FF7FF75EFF7FF75EFF7FEF3DF75EF75EEF3D0000EF3DFF7FFF7FFF7FFF7FFF7F
      FF7FFF7FFF7FFF7FFF7FF75E000000000000F75EFF7FFF7FF75EFF7FF75EFF7F
      F75EFF7FF75EFF7FF75EFF7FF75EEF3D00001042FF7FE07F1863E07F1863E07F
      1863E07F1863E07F1863E07F104200000000F75EFF7FFF7FFF7FFF7FFF7FFF7F
      FF7FFF7FFF7FFF7FF75EEF3DF75EF75EEF3D0000EF3DFF7FFF7F000000000000
      000000000000FF7FFF7FF75E000000000000F75EFF7FF75EFF7FF75EFF7FF75E
      FF7FF75EFF7FF75EFF7FF75EF75EEF3D00001042FF7F1863E07F1863E07F1863
      E07F1863E07F1863E07F1863104200000000F75EF75EF75EF75EF75EF75EF75E
      F75EF75EF75EF75EF75EF75EFF7FF75EEF3D0000EF3DFF7FFF7FFF7FFF7FFF7F
      FF7FFF7FFF7FFF7FFF7FF75E000000000000F75EFF7FFF7FF75EFF7FF75EFF7F
      F75EFF7FF75EFF7FF75EFF7FF75EEF3D00001042FF7FE07F1863E07F1863E07F
      1863E07F1863E07F1863E07F1042000000000000F75EFF7FF75EFF7FF75EFF7F
      F75EFF7FF75EFF7FF75EFF7FF75EF75EEF3D0000EF3DFF7FFF7F000000000000
      FF7F00000000FF7FFF7FF75E000000000000F75EFF7FF75EFF7FF75EFF7FF75E
      FF7FF75EFF7FF75EFF7FF75EF75EEF3D00001042FF7F1863E07F1863E07F1863
      E07F1863E07F1863E07F18631042000000000000F75EFF7FFF7FF75EFF7FF75E
      FF7FF75EFF7FFF7FFF7FFF7FFF7FF75EEF3D0000EF3DFF7FFF7FFF7FFF7FFF7F
      FF7FFF7FFF7FFF7FFF7FF75E000000000000F75EFF7FFF7FF75EFF7FF75EFF7F
      F75EFF7FF75EFF7FF75EFF7FF75EEF3D00001042FF7FE07F1863E07F1863E07F
      1863E07F1863E07F1863E07F1042000000000000F75EFF7FF75EFF7FF75EFF7F
      F75EFF7FF75EF75EF75EF75EF75EF75E00000000EF3DFF7FFF7FFF7FFF7FFF7F
      FF7FFF7FFF7FFF7FFF7FF75E000000000000F75EFF7FFF7FFF7FFF7FFF7FFF7F
      FF7FFF7FFF7FFF7FFF7FFF7FF75EEF3D00001042FF7FFF7FFF7FFF7FFF7FFF7F
      FF7FFF7FFF7FFF7FFF7FFF7F10420000000000000000F75EFF7FFF7FFF7FFF7F
      FF7FF75E00000000000000000000000000000000EF3DFF7F0000FF7F0000FF7F
      0000FF7F0000FF7F0000FF7F000000000000F75EF75EFF7FF75EFF7FF75EFF7F
      F75EF75EF75EF75EF75EF75EF75E0000000010421863E07F1863E07F1863E07F
      186310421042104210421042104200000000000000000000F75EF75EF75EF75E
      F75E00000000000000000000000000000000000000000000FF7FEF3DFF7FEF3D
      FF7FEF3DFF7FEF3DFF7F00000000000000000000F75EF75EFF7FF75EFF7FF75E
      F75E00000000000000000000000000000000000010421863E07F1863E07F1863
      1042000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FF7F0000FF7F
      0000FF7F0000FF7F0000000000000000000000000000F75EF75EF75EF75EF75E
      0000000000000000000000000000000000000000000010421042104210421042
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000100010000000000000000000F75EF75EF75EF75EF75E
      F75EF75EF75EF75EF75EF75E00000000000000000000F75EF75EF75EF75EF75E
      F75EF75EF75EF75EF75EF75E00000000000000000000EF3DEF3DEF3DEF3DEF3D
      EF3DEF3DEF3DEF3DEF3DEF3D0000000000000000000000000000000000000000
      0000000000000000000010001000000000000000EF3DEF3DEF3DEF3DEF3DEF3D
      EF3DEF3DEF3DEF3DEF3DEF3DF75E000000000000000000000000000000000000
      00000000000000000000F75EF75E000000000000F75EF75EF75EF75EF75EF75E
      F75EF75EF75EF75EF75EF75EEF3D000000000000000000000000000000000000
      0000000000000000000000000000000000000000EF3DFF7FF75EFF7FF75EFF7F
      F75EFF7FF75EFF7FF75EEF3DF75E0000000000000000F75EE07FF75EE07FF75E
      E07FF75EE07FF75EE07F0000F75E000000000000F75EFF7FFF7FFF7FFF7FFF7F
      FF7FFF7FFF7FFF7FFF7FF75EEF3D000000000000000000000000000000000000
      0000000000000000000010001000000000000000EF3DF75EFF7FF75EFF7FF75E
      FF7FF75EFF7FF75EFF7FEF3DF75E0000000000000000E07FF75EE07FF75EE07F
      F75EE07FF75EE07FF75E0000F75E000000000000F75EFF7FFF7FFF7FFF7FFF7F
      FF7FFF7FFF7FFF7FFF7FF75EEF3D000000000000000000000000000000000000
      0000000000000000000010001000000000000000EF3DFF7FF75EFF7FF75EFF7F
      F75EFF7FF75EFF7FF75EEF3DF75E0000000000000000F75EE07FF75EE07FF75E
      E07FF75EE07FF75EE07F0000F75E000000000000F75EFF7FFF7FEF3DEF3DEF3D
      EF3DEF3DEF3DFF7FFF7FF75EEF3D000000000000000000000000000000000000
      0000000000000000000010001000100000000000EF3DF75EFF7FF75EFF7FF75E
      FF7FF75EFF7FF75EFF7FEF3DF75E0000000000000000E07FF75EE07FF75EE07F
      F75EE07FF75EE07FF75E0000F75E000000000000F75EFF7FFF7FFF7FFF7FFF7F
      FF7FFF7FFF7FFF7FFF7FF75EEF3D000000000000000000000000000000000000
      0000000000000000000000001000100010000000EF3DFF7FF75EFF7FF75EFF7F
      F75EFF7FF75EFF7FF75EEF3DF75E0000000000000000F75EE07FF75EE07FF75E
      E07FF75EE07FF75EE07F0000F75E000000000000F75EFF7FFF7FEF3DEF3DEF3D
      EF3DEF3DEF3DFF7FFF7FF75EEF3D0000000000001F0000000000000000000000
      E07F000000000000000000000000100010000000EF3DF75EFF7FF75EFF7FF75E
      FF7FF75EFF7FF75EFF7FEF3DF75E0000000000000000E07FF75EE07FF75EE07F
      F75EE07FF75EE07FF75E0000F75E000000000000F75EFF7FFF7FFF7FFF7FFF7F
      FF7FFF7FFF7FFF7FFF7FF75EEF3D0000000000001F001F001F000000E07FE07F
      E07F000000001000100000000000100010000000EF3DFF7FF75EFF7FF75EFF7F
      F75EFF7FF75EFF7FF75EEF3DF75E0000000000000000F75EE07FF75EE07FF75E
      E07FF75EE07FF75EE07F0000F75E000000000000F75EFF7FFF7FEF3DEF3DEF3D
      EF3DEF3DEF3DFF7FFF7FF75EEF3D0000000000001F001F001F000000E07FE07F
      E07F000000001000100010001000100010000000EF3DF75EFF7FFF7FFF7FFF7F
      FF7FFF7FFF7FF75EFF7FEF3DF75E0000000000000000E07FF75EFF7FFF7FFF7F
      FF7FFF7FFF7FE07FF75E0000F75E000000000000F75EFF7FFF7FFF7FFF7FFF7F
      FF7FFF7FFF7FFF7FFF7FF75EEF3D00000000000000001F001F000000E07FE07F
      0000000000000000100010001000100000000000EF3DFF7FF75EFF7FFF7FFF7F
      FF7FFF7FFF7FFF7FF75EEF3DF75E0000000000000000F75EE07FFF7FFF7FFF7F
      FF7FFF7FFF7FF75EE07F0000F75E000000000000F75EFF7FFF7FEF3DEF3DEF3D
      FF7FEF3DEF3DFF7FFF7FF75EEF3D000000000000007C00000000000000000000
      0002000000000000000000000000000000000000EF3DF75EFF7FFF7FFF7FFF7F
      FF7FFF7FFF7FF75EFF7FEF3DF75E0000000000000000E07FF75EFF7FFF7FFF7F
      FF7FFF7FFF7FE07FF75E0000F75E000000000000F75EFF7FFF7FFF7FFF7FFF7F
      FF7FFF7FFF7FFF7FFF7FF75EEF3D000000000000007C007C007C000000020002
      0002000000000000000000000000000000000000EF3DFF7FF75EFF7FF75EFF7F
      F75EFF7FF75EFF7FF75EEF3DF75E0000000000000000F75EE07FF75EE07FF75E
      E07FF75EE07FF75EE07F0000F75E000000000000F75EFF7FFF7FFF7FFF7FFF7F
      FF7FFF7FFF7FFF7FFF7FF75EEF3D000000000000007C007C007C000000020002
      0002000000000000000000000000000000000000EF3DF75EEF3DF75EEF3DF75E
      EF3DF75EEF3DF75EEF3DFF7FF75E0000000000000000E07F0000E07F0000E07F
      0000E07F0000E07F0000FF7FF75E000000000000F75EFF7FEF3DFF7FEF3DFF7F
      EF3DFF7FEF3DFF7FEF3DFF7FEF3D0000000000000000007C007C000000020002
      00000000000000000000000000000000000000000000EF3DFF7FEF3DFF7FEF3D
      FF7FEF3DFF7FEF3DFF7FEF3D000000000000000000000000FF7FEF3DFF7FEF3D
      FF7FEF3DFF7FEF3DFF7F000000000000000000000000EF3DFF7FF75EFF7FF75E
      FF7FF75EFF7FF75EFF7FEF3D0000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000EF3DFF7FEF3DFF7F
      EF3DFF7FEF3DFF7FEF3D00000000000000000000000000000000FF7F0000FF7F
      0000FF7F0000FF7F00000000000000000000000000000000EF3DFF7FEF3DFF7F
      EF3DFF7FEF3DFF7FEF3D0000000000000000424D3E000000000000003E000000
      2800000040000000300000000100010000000000800100000000000000000000
      000000000000000000000000FFFFFF00FFFF343136433643E000363337304646
      C000364337333734C00034373635464680003030303038358000364637324530
      0000202020203635000037463639364400003030304230318000303137343431
      80003734363534368001303037343434C07F343130303030E0FF303036443631
      FFFF343637323635FFFF0D0A20202020C007FFFFFFFFFFFF8003FFFFFFFFE000
      8003FFFFFFFFC000800380018001C00080030001000180008003000100018000
      8003000100010000800300010001000080030001000100008003000100018000
      80030001000180008003000100018001800300010001C07F800300030003E0FF
      C00780FF80FFFFFFE00FC1FFC1FFFFFFFFF3C007C007C007FFF38003C0038003
      FFFF800380038003FFF3800380038003FFF3800380038003FFF1800380038003
      3E78800380038003007C800380038003004C8003800380030040800380038003
      0061800380038003007F800380038003007F800380038003007F800380038003
      007FC007C007C007C1FFE00FE00FE00F00000000000000000000000000000000
      000000000000}
  end
end
