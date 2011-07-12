object dlgRepoBrowser: TdlgRepoBrowser
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Repo Browser'
  ClientHeight = 328
  ClientWidth = 735
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 296
    Width = 735
    Height = 32
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      735
      32)
    object OK: TButton
      Left = 484
      Top = 4
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object Cancel: TButton
      Left = 565
      Top = 4
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object Help: TButton
      Left = 646
      Top = 4
      Width = 75
      Height = 25
      HelpContext = 15210
      Anchors = [akRight, akBottom]
      Caption = 'Help'
      TabOrder = 2
      OnClick = HelpClick
    end
  end
end
