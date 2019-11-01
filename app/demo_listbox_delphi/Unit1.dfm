object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 281
  ClientWidth = 418
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object L: TATListbox
    Left = 0
    Top = 0
    Width = 418
    Height = 240
    Align = alClient
    ParentFont = False
    ExplicitHeight = 169
  end
  object Panel1: TPanel
    Left = 0
    Top = 240
    Width = 418
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object chkThemed: TCheckBox
      Left = 1
      Top = 0
      Width = 105
      Height = 17
      Caption = 'themed scrollbars'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = chkThemedClick
    end
    object chkHorzBar: TCheckBox
      Left = 1
      Top = 16
      Width = 97
      Height = 17
      Caption = 'horz scrollbar'
      TabOrder = 1
      OnClick = chkHorzBarClick
    end
  end
end
