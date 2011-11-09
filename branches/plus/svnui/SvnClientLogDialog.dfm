object SvnLogDialog: TSvnLogDialog
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Log'
  ClientHeight = 514
  ClientWidth = 680
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
  object Panel1: TPanel
    Left = 0
    Top = 482
    Width = 680
    Height = 32
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitTop = 610
    ExplicitWidth = 732
    DesignSize = (
      680
      32)
    object OK: TButton
      Left = 429
      Top = 4
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
      ExplicitLeft = 481
    end
    object Cancel: TButton
      Left = 510
      Top = 4
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
      ExplicitLeft = 562
    end
    object Help: TButton
      Left = 591
      Top = 4
      Width = 75
      Height = 25
      HelpContext = 15210
      Anchors = [akRight, akBottom]
      Caption = 'Help'
      TabOrder = 2
      ExplicitLeft = 643
    end
  end
end
