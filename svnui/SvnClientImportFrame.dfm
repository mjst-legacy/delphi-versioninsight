object SvnImportFrame: TSvnImportFrame
  Left = 0
  Top = 0
  Width = 919
  Height = 780
  HelpContext = 15203
  TabOrder = 0
  object Splitter1: TSplitter
    Left = 0
    Top = 364
    Width = 919
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    ExplicitTop = 80
    ExplicitWidth = 290
  end
  object Splitter2: TSplitter
    Left = 0
    Top = 569
    Width = 919
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    ExplicitTop = 571
  end
  object Top: TPanel
    Left = 0
    Top = 0
    Width = 919
    Height = 80
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      919
      80)
    object GroupBox1: TGroupBox
      Left = 3
      Top = 3
      Width = 913
      Height = 70
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Repository'
      TabOrder = 0
      DesignSize = (
        913
        70)
      object Label1: TLabel
        Left = 3
        Top = 18
        Width = 88
        Height = 13
        Caption = 'URL of repository:'
      end
      object URL: TComboBoxEx
        Left = 3
        Top = 37
        Width = 879
        Height = 22
        ItemsEx = <>
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        OnKeyUp = URLKeyUp
        OnSelect = URLSelect
      end
      object Browse: TButton
        Left = 888
        Top = 37
        Width = 22
        Height = 22
        Anchors = [akTop, akRight]
        Caption = '...'
        TabOrder = 1
        OnClick = BrowseClick
      end
    end
  end
  object Center: TPanel
    Left = 0
    Top = 80
    Width = 919
    Height = 284
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      919
      284)
    object FilesGroup: TGroupBox
      Left = 6
      Top = -1
      Width = 909
      Height = 279
      Anchors = [akLeft, akTop, akRight, akBottom]
      Caption = 'Files to be committed'
      TabOrder = 0
      object CommitFiles: TCheckListBox
        AlignWithMargins = True
        Left = 5
        Top = 18
        Width = 899
        Height = 256
        Align = alClient
        ItemHeight = 13
        TabOrder = 0
      end
    end
  end
  object Bottom2: TPanel
    Left = 0
    Top = 572
    Width = 919
    Height = 169
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    DesignSize = (
      919
      169)
    object GroupBox2: TGroupBox
      Left = 2
      Top = 4
      Width = 913
      Height = 152
      Anchors = [akLeft, akTop, akRight, akBottom]
      Caption = 'Import Comment'
      TabOrder = 0
      DesignSize = (
        913
        152)
      object Comment: TMemo
        Left = 3
        Top = 13
        Width = 907
        Height = 136
        Anchors = [akLeft, akTop, akRight, akBottom]
        TabOrder = 0
      end
    end
  end
  object Bottom3: TPanel
    Left = 0
    Top = 741
    Width = 919
    Height = 39
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 3
    DesignSize = (
      919
      39)
    object Import: TButton
      Left = 838
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'Import'
      Enabled = False
      TabOrder = 0
      OnClick = ImportClick
    end
    object RecentMessages: TButton
      Left = 676
      Top = 6
      Width = 156
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'Recent Comments'
      Enabled = False
      TabOrder = 1
      OnClick = RecentMessagesClick
    end
  end
  object Bottom1: TPanel
    Left = 0
    Top = 367
    Width = 919
    Height = 202
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 4
    DesignSize = (
      919
      202)
    object OutOfScopeFilesGroupBox: TGroupBox
      Left = 3
      Top = 6
      Width = 913
      Height = 190
      Anchors = [akLeft, akTop, akRight, akBottom]
      Caption = 'File not under %s - These files will not be committed.'
      TabOrder = 0
      DesignSize = (
        913
        190)
      object OutOfScopeFiles: TListBox
        Left = 3
        Top = 16
        Width = 907
        Height = 171
        Anchors = [akLeft, akTop, akRight, akBottom]
        ItemHeight = 13
        TabOrder = 0
      end
    end
  end
end
