object RecentCommentsDialog: TRecentCommentsDialog
  Left = 0
  Top = 0
  HelpContext = 15207
  Caption = 'Recent Comments'
  ClientHeight = 509
  ClientWidth = 650
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
  object Splitter1: TSplitter
    Left = 0
    Top = 117
    Width = 650
    Height = 3
    Cursor = crVSplit
    Align = alTop
    ExplicitWidth = 220
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 650
    Height = 117
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      650
      117)
    object RecentComment: TListBox
      Left = 6
      Top = 6
      Width = 638
      Height = 105
      Anchors = [akLeft, akTop, akRight, akBottom]
      ItemHeight = 13
      TabOrder = 0
      OnClick = RecentCommentClick
      OnDblClick = RecentCommentDblClick
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 120
    Width = 650
    Height = 348
    Align = alClient
    BevelOuter = bvNone
    Caption = 'Panel2'
    TabOrder = 1
    DesignSize = (
      650
      348)
    object Comment: TMemo
      Left = 6
      Top = 6
      Width = 638
      Height = 336
      Anchors = [akLeft, akTop, akRight, akBottom]
      ReadOnly = True
      TabOrder = 0
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 468
    Width = 650
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    DesignSize = (
      650
      41)
    object Ok: TButton
      Left = 488
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object Cancel: TButton
      Left = 569
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
end
