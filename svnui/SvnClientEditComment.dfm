object EditCommentDialog: TEditCommentDialog
  Left = 0
  Top = 0
  HelpContext = 15207
  Caption = 'Edit Comment'
  ClientHeight = 262
  ClientWidth = 434
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
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 434
    Height = 221
    Align = alClient
    BevelOuter = bvNone
    Caption = 'Panel2'
    TabOrder = 0
    ExplicitWidth = 450
    ExplicitHeight = 309
    DesignSize = (
      434
      221)
    object Comment: TMemo
      Left = 6
      Top = 6
      Width = 422
      Height = 209
      Anchors = [akLeft, akTop, akRight, akBottom]
      TabOrder = 0
      ExplicitWidth = 438
      ExplicitHeight = 297
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 221
    Width = 434
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitTop = 309
    ExplicitWidth = 450
    DesignSize = (
      434
      41)
    object Ok: TButton
      Left = 272
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
      ExplicitLeft = 288
    end
    object Cancel: TButton
      Left = 353
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
      ExplicitLeft = 369
    end
  end
end
