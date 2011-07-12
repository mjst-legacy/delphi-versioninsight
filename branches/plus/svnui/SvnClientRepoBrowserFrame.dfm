object FrmRepoBrowser: TFrmRepoBrowser
  Left = 0
  Top = 0
  Width = 731
  Height = 336
  HelpContext = 15210
  TabOrder = 0
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 731
    Height = 47
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      731
      47)
    object Label1: TLabel
      Left = 87
      Top = 6
      Width = 19
      Height = 13
      Caption = 'URL'
    end
    object Label2: TLabel
      Left = 614
      Top = 6
      Width = 40
      Height = 13
      Anchors = [akTop, akRight]
      Caption = 'Revision'
    end
    object btnLoad: TButton
      Left = 6
      Top = 16
      Width = 75
      Height = 25
      Caption = 'Load'
      Enabled = False
      TabOrder = 0
      OnClick = btnLoadClick
    end
    object edURL: TEdit
      Left = 87
      Top = 20
      Width = 521
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      OnChange = edURLChange
      OnKeyDown = edURLKeyDown
    end
    object edRevision: TEdit
      Left = 614
      Top = 20
      Width = 111
      Height = 21
      Anchors = [akTop, akRight]
      TabOrder = 2
      Text = 'HEAD'
      OnKeyDown = edURLKeyDown
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 47
    Width = 731
    Height = 289
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object Splitter1: TSplitter
      Left = 197
      Top = 0
      Height = 289
      ExplicitLeft = 203
      ExplicitHeight = 287
    end
    object Panel3: TPanel
      Left = 0
      Top = 0
      Width = 197
      Height = 289
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
      DesignSize = (
        197
        289)
      object tvFolders: TTreeView
        Left = 6
        Top = 0
        Width = 191
        Height = 283
        Anchors = [akLeft, akTop, akRight, akBottom]
        HideSelection = False
        Images = ImageList
        Indent = 19
        ReadOnly = True
        TabOrder = 0
        OnChange = tvFoldersChange
        OnExpanding = tvFoldersExpanding
      end
    end
    object Panel4: TPanel
      Left = 200
      Top = 0
      Width = 531
      Height = 289
      Align = alClient
      BevelOuter = bvNone
      Caption = 'Panel4'
      TabOrder = 1
      DesignSize = (
        531
        289)
      object lvItems: TListView
        Left = 0
        Top = 0
        Width = 525
        Height = 283
        Anchors = [akLeft, akTop, akRight, akBottom]
        Columns = <
          item
            Caption = 'Name'
            Width = 208
          end
          item
            Caption = 'Author'
            Width = 64
          end
          item
            Caption = 'Date'
            Width = 120
          end
          item
            Alignment = taRightJustify
            Caption = 'Size'
            Width = 48
          end
          item
            Alignment = taRightJustify
            Caption = 'Revision'
            Width = 48
          end>
        ReadOnly = True
        RowSelect = True
        SmallImages = ImageList
        TabOrder = 0
        ViewStyle = vsReport
        OnChange = lvItemsChange
        OnCustomDraw = lvItemsCustomDraw
        OnResize = lvItemsResize
      end
    end
  end
  object ImageList: TImageList
    Left = 224
    Top = 104
  end
  object SysImageList: TImageList
    ShareImages = True
    Left = 224
    Top = 160
  end
end
