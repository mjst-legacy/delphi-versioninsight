object HgUpdateDialog: THgUpdateDialog
  Left = 0
  Top = 0
  HelpContext = 15211
  BorderIcons = [biSystemMenu]
  Caption = 'Updating - %s'
  ClientHeight = 474
  ClientWidth = 720
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
    Top = 0
    Width = 720
    Height = 433
    Align = alClient
    BevelEdges = []
    BevelOuter = bvNone
    TabOrder = 0
    object Files: TListView
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 714
      Height = 427
      Align = alClient
      Columns = <
        item
          Caption = 'Action'
          Width = -2
          WidthType = (
            -2)
        end
        item
          Caption = 'File'
          Width = -2
          WidthType = (
            -2)
        end>
      DoubleBuffered = True
      Groups = <
        item
          Header = 'Conflicts'
          GroupID = 0
          State = [lgsNormal]
          HeaderAlign = taLeftJustify
          FooterAlign = taLeftJustify
          TitleImage = -1
        end
        item
          Header = 'Files'
          GroupID = 1
          State = [lgsNormal]
          HeaderAlign = taLeftJustify
          FooterAlign = taLeftJustify
          TitleImage = -1
        end>
      GroupView = True
      ReadOnly = True
      RowSelect = True
      ParentDoubleBuffered = False
      PopupMenu = PopupMenu
      TabOrder = 0
      ViewStyle = vsReport
      OnCustomDrawItem = FilesCustomDrawItem
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 433
    Width = 720
    Height = 41
    Align = alBottom
    BevelEdges = []
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      720
      41)
    object Ok: TButton
      Left = 480
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'OK'
      Default = True
      Enabled = False
      TabOrder = 0
      OnClick = OkClick
    end
    object Abort: TButton
      Left = 561
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'Abort'
      ModalResult = 3
      TabOrder = 1
      OnClick = AbortClick
    end
    object Help: TButton
      Left = 642
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'Help'
      TabOrder = 2
      OnClick = HelpClick
    end
  end
  object ActionList1: TActionList
    Left = 8
    Top = 608
    object ResolveAction: TAction
      Caption = 'Resolve'
      OnExecute = ResolveActionExecute
      OnUpdate = ResolveActionUpdate
    end
  end
  object PopupMenu: TPopupMenu
    Left = 48
    Top = 608
    object Resolve1: TMenuItem
      Action = ResolveAction
    end
  end
end
