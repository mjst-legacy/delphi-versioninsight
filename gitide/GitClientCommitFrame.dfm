object GitCommitFrame: TGitCommitFrame
  Left = 0
  Top = 0
  Width = 589
  Height = 494
  HelpContext = 15202
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  ParentFont = False
  TabOrder = 0
  object Splitter1: TSplitter
    Left = 0
    Top = 302
    Width = 589
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    ExplicitWidth = 192
  end
  object UpperPanel: TPanel
    Left = 0
    Top = 0
    Width = 589
    Height = 302
    Align = alClient
    BevelEdges = []
    BevelOuter = bvNone
    DoubleBuffered = True
    ParentDoubleBuffered = False
    TabOrder = 0
    DesignSize = (
      589
      302)
    object Label1: TLabel
      Left = 8
      Top = 8
      Width = 52
      Height = 13
      Caption = 'Commit to:'
    end
    object Location: TLabel
      Left = 20
      Top = 24
      Width = 40
      Height = 13
      Caption = 'Location'
    end
    object Files: TListView
      Left = 8
      Top = 43
      Width = 573
      Height = 259
      Anchors = [akLeft, akTop, akRight, akBottom]
      Checkboxes = True
      Columns = <
        item
          Caption = 'Name'
          Width = -2
          WidthType = (
            -2)
        end
        item
          Caption = 'Path'
          Width = -2
          WidthType = (
            -2)
        end
        item
          Caption = 'Ext'
          Width = -2
          WidthType = (
            -2)
        end
        item
          Caption = 'Status'
          Width = -2
          WidthType = (
            -2)
        end>
      MultiSelect = True
      ReadOnly = True
      RowSelect = True
      PopupMenu = CommitMenu
      SmallImages = GitImageModule.ShellImagesSmall
      TabOrder = 0
      ViewStyle = vsReport
      OnColumnClick = FilesColumnClick
      OnCustomDraw = FilesCustomDraw
      OnCustomDrawItem = FilesCustomDrawItem
      OnDblClick = FilesDblClick
      OnKeyDown = FilesKeyDown
      OnItemChecked = FilesItemChecked
    end
  end
  object LowerPanel: TPanel
    Left = 0
    Top = 305
    Width = 589
    Height = 189
    Align = alBottom
    BevelEdges = []
    BevelOuter = bvNone
    TabOrder = 1
    OnResize = LowerPanelResize
    DesignSize = (
      589
      189)
    object Label2: TLabel
      Left = 8
      Top = 6
      Width = 45
      Height = 13
      Caption = 'Comment'
    end
    object Comment: TMemo
      Left = 8
      Top = 22
      Width = 573
      Height = 91
      Anchors = [akLeft, akTop, akRight, akBottom]
      ScrollBars = ssVertical
      TabOrder = 0
      OnChange = CommentChange
    end
    object Commit: TButton
      Left = 506
      Top = 150
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'Commit'
      Enabled = False
      TabOrder = 1
      OnClick = CommitClick
    end
    object Externals: TCheckBox
      Left = 8
      Top = 142
      Width = 305
      Height = 17
      Anchors = [akLeft, akBottom]
      Caption = 'Show externals '
      TabOrder = 2
      OnClick = ExternalsClick
    end
    object Recent: TButton
      Left = 425
      Top = 119
      Width = 156
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'Recent Comments'
      Enabled = False
      TabOrder = 3
      OnClick = RecentClick
    end
    object UnversionedFiles: TCheckBox
      Left = 8
      Top = 119
      Width = 305
      Height = 17
      Anchors = [akLeft, akBottom]
      Caption = 'Show unversioned files'
      TabOrder = 4
      OnClick = UnversionedFilesClick
    end
    object CheckAll: TCheckBox
      Left = 8
      Top = 165
      Width = 305
      Height = 17
      Anchors = [akLeft, akBottom]
      Caption = 'Check or uncheck all'
      TabOrder = 5
      OnClick = CheckAllClick
    end
  end
  object CommitMenu: TPopupMenu
    Left = 529
    Top = 6
    object Difference1: TMenuItem
      Action = DiffAction
    end
    object Revert1: TMenuItem
      Action = RevertAction
    end
    object Add1: TMenuItem
      Action = AddAction
    end
    object ResolveAction1: TMenuItem
      Action = ResolveAction
    end
  end
  object CommitActions: TActionList
    Left = 473
    Top = 9
    object DiffAction: TAction
      Caption = '&Difference'
      OnExecute = DoDiff
      OnUpdate = DiffActionUpdate
    end
    object RevertAction: TAction
      Caption = '&Revert'
      OnExecute = RevertActionExecute
      OnUpdate = RevertActionUpdate
    end
    object AddAction: TAction
      Caption = '&Add'
      OnExecute = AddActionExecute
      OnUpdate = AddActionUpdate
    end
    object ResolveAction: TAction
      Caption = 'Res&olve Conflict'
      Hint = 'Resolve Conflict'
      OnExecute = ResolveActionExecute
      OnUpdate = ResolveActionUpdate
    end
  end
end
