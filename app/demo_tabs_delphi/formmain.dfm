object Form1: TForm1
  Left = 195
  Top = 260
  Caption = 'ATTabs demo'
  ClientHeight = 430
  ClientWidth = 789
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 14
  object Label1: TLabel
    Left = 208
    Top = 256
    Width = 46
    Height = 14
    Caption = 'position:'
  end
  object Label2: TLabel
    Left = 208
    Top = 288
    Width = 40
    Height = 14
    Caption = 'theme:'
  end
  object chkFlat: TCheckBox
    Left = 208
    Top = 144
    Width = 97
    Height = 17
    Caption = 'flat tabs'
    TabOrder = 1
    OnClick = chkFlatClick
  end
  object chkShowPlus: TCheckBox
    Left = 208
    Top = 120
    Width = 153
    Height = 17
    Caption = 'show plus pseudo tab'
    Checked = True
    State = cbChecked
    TabOrder = 0
    OnClick = chkShowPlusClick
  end
  object chkAngled: TCheckBox
    Left = 208
    Top = 168
    Width = 97
    Height = 17
    Caption = 'angled tabs'
    TabOrder = 2
    OnClick = chkAngledClick
  end
  object chkGap: TCheckBox
    Left = 208
    Top = 192
    Width = 153
    Height = 17
    Caption = 'gap between tabs'
    TabOrder = 3
    OnClick = chkGapClick
  end
  object chkVarWidth: TCheckBox
    Left = 416
    Top = 120
    Width = 97
    Height = 17
    Caption = 'var width'
    TabOrder = 4
    OnClick = chkVarWidthClick
  end
  object chkMultiline: TCheckBox
    Left = 416
    Top = 144
    Width = 97
    Height = 17
    Caption = 'multi-line'
    TabOrder = 5
    OnClick = chkMultilineClick
  end
  object chkPosTop: TRadioButton
    Left = 264
    Top = 256
    Width = 57
    Height = 17
    Caption = 'top'
    Checked = True
    TabOrder = 6
    TabStop = True
    OnClick = chkPosTopClick
  end
  object chkPosBtm: TRadioButton
    Left = 320
    Top = 256
    Width = 73
    Height = 17
    Caption = 'bottom'
    TabOrder = 7
    OnClick = chkPosBtmClick
  end
  object chkPosLeft: TRadioButton
    Left = 392
    Top = 256
    Width = 65
    Height = 17
    Caption = 'left'
    TabOrder = 8
    OnClick = chkPosLeftClick
  end
  object chkPosRight: TRadioButton
    Left = 456
    Top = 256
    Width = 65
    Height = 17
    Caption = 'right'
    TabOrder = 9
    OnClick = chkPosRightClick
  end
  object chkCenterCaption: TCheckBox
    Left = 208
    Top = 216
    Width = 161
    Height = 17
    Caption = 'center captions'
    TabOrder = 10
    OnClick = chkCenterCaptionClick
  end
  object cbThemeList: TComboBox
    Left = 264
    Top = 286
    Width = 146
    Height = 22
    Style = csDropDownList
    TabOrder = 11
    OnClick = cbThemeListClick
  end
  object XPManifest1: TXPManifest
    Left = 712
    Top = 104
  end
  object PopupMenu1: TPopupMenu
    Left = 608
    Top = 168
    object item11: TMenuItem
      Caption = 'item 1'
    end
    object item21: TMenuItem
      Caption = 'item 2'
    end
  end
end
