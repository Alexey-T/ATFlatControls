object Form1: TForm1
  Left = 303
  Top = 282
  AutoScroll = False
  Caption = 'ATStatus demo'
  ClientHeight = 381
  ClientWidth = 716
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
  object Label1: TLabel
    Left = 200
    Top = 256
    Width = 21
    Height = 13
    Caption = 'Click'
  end
  object bAdd: TButton
    Left = 200
    Top = 192
    Width = 75
    Height = 25
    Caption = 'add'
    TabOrder = 0
    OnClick = bAddClick
  end
  object bDel: TButton
    Left = 280
    Top = 192
    Width = 75
    Height = 25
    Caption = 'del'
    TabOrder = 1
    OnClick = bDelClick
  end
  object Edit1: TEdit
    Left = 200
    Top = 224
    Width = 225
    Height = 21
    TabOrder = 2
    OnChange = Edit1Change
  end
end
