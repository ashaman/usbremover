object MainFrm: TMainFrm
  Left = 372
  Top = 230
  Width = 716
  Height = 523
  Caption = 'USBRemover'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 708
    Height = 57
    Align = alTop
    TabOrder = 0
    object Label1: TLabel
      Left = 16
      Top = 8
      Width = 124
      Height = 13
      Caption = 'Select the drive to remove'
    end
    object ComboBox1: TComboBox
      Left = 16
      Top = 32
      Width = 145
      Height = 21
      ItemHeight = 13
      TabOrder = 0
      OnChange = ComboBox1Change
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 57
    Width = 708
    Height = 432
    Align = alClient
    Caption = 'Panel2'
    TabOrder = 1
    object TreeView1: TTreeView
      Left = 1
      Top = 1
      Width = 706
      Height = 430
      Align = alClient
      Indent = 19
      TabOrder = 0
    end
  end
end
