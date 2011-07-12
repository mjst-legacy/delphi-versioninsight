object frmVerInsBlameOptions: TfrmVerInsBlameOptions
  Left = 0
  Top = 0
  Width = 488
  Height = 489
  TabOrder = 0
  DesignSize = (
    488
    489)
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 36
    Height = 13
    Caption = 'Presets'
  end
  object GroupBox1: TGroupBox
    Left = 176
    Top = 8
    Width = 304
    Height = 440
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = ' Preset Option  '
    TabOrder = 0
    DesignSize = (
      304
      440)
    object Label2: TLabel
      Left = 8
      Top = 56
      Width = 40
      Height = 13
      Caption = 'Revision'
    end
    object Label3: TLabel
      Left = 8
      Top = 120
      Width = 23
      Height = 13
      Caption = 'Date'
    end
    object Label4: TLabel
      Left = 8
      Top = 224
      Width = 22
      Height = 13
      Caption = 'User'
    end
    object Label5: TLabel
      Left = 24
      Top = 184
      Width = 34
      Height = 13
      Caption = 'Format'
    end
    object Label6: TLabel
      Left = 8
      Top = 16
      Width = 63
      Height = 13
      Caption = 'Paint method'
    end
    object cbShowUserText: TCheckBox
      Left = 8
      Top = 240
      Width = 121
      Height = 17
      Caption = 'Text'
      Checked = True
      State = cbChecked
      TabOrder = 0
    end
    object cbShowUserColor: TCheckBox
      Left = 8
      Top = 264
      Width = 121
      Height = 17
      Caption = 'Color Box'
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
    object cbShowRevisionColor: TCheckBox
      Left = 8
      Top = 96
      Width = 121
      Height = 17
      Caption = 'Color Box'
      Checked = True
      State = cbChecked
      TabOrder = 2
    end
    object cbShowDateColor: TCheckBox
      Left = 8
      Top = 160
      Width = 85
      Height = 17
      Caption = 'Color Box'
      Checked = True
      State = cbChecked
      TabOrder = 3
    end
    object cbShowRevisionText: TCheckBox
      Left = 8
      Top = 72
      Width = 121
      Height = 17
      Caption = 'Text'
      Checked = True
      State = cbChecked
      TabOrder = 4
    end
    object cbShowDateText: TCheckBox
      Left = 8
      Top = 136
      Width = 121
      Height = 17
      Caption = 'Text'
      Checked = True
      State = cbChecked
      TabOrder = 5
    end
    object cboxRevisionStartColor: TColorBox
      Left = 76
      Top = 96
      Width = 104
      Height = 22
      Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbPrettyNames]
      TabOrder = 6
    end
    object cboxRevisionEndColor: TColorBox
      Left = 184
      Top = 96
      Width = 104
      Height = 22
      Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbPrettyNames]
      TabOrder = 7
    end
    object cboxDateStartColor: TColorBox
      Left = 76
      Top = 160
      Width = 104
      Height = 22
      Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbPrettyNames]
      TabOrder = 8
    end
    object cboxDateEndColor: TColorBox
      Left = 184
      Top = 160
      Width = 104
      Height = 22
      Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbPrettyNames]
      TabOrder = 9
    end
    object cbDateFormat: TComboBox
      Left = 24
      Top = 200
      Width = 121
      Height = 21
      Style = csDropDownList
      TabOrder = 10
      Items.Strings = (
        'System'
        'yyyy/mm/dd')
    end
    object cbPaintMethod: TComboBox
      Left = 8
      Top = 32
      Width = 121
      Height = 21
      Style = csDropDownList
      TabOrder = 11
      Items.Strings = (
        'default'
        'successive 1'
        'successive 2')
    end
    object lvUserColors: TListView
      Left = 8
      Top = 288
      Width = 288
      Height = 112
      Anchors = [akLeft, akTop, akRight, akBottom]
      Columns = <
        item
          Caption = 'User'
          Width = 120
        end
        item
          Caption = 'Color'
        end
        item
          Caption = 'Visible Name'
          Width = 100
        end>
      ReadOnly = True
      RowSelect = True
      TabOrder = 12
      ViewStyle = vsReport
      OnAdvancedCustomDrawItem = lvUserColorsAdvancedCustomDrawItem
      OnResize = lvUserColorsResize
      OnSelectItem = lvUserColorsSelectItem
    end
    object btnAddUser: TButton
      Left = 8
      Top = 408
      Width = 75
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Add'
      TabOrder = 13
      OnClick = btnAddUserClick
    end
    object btnDeleteUser: TButton
      Left = 88
      Top = 408
      Width = 75
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Delete'
      TabOrder = 14
      OnClick = btnDeleteUserClick
    end
  end
  object Button1: TButton
    Left = 8
    Top = 456
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Add'
    TabOrder = 1
    OnClick = Button1Click
  end
  object btnDelete: TButton
    Left = 88
    Top = 456
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Delete'
    TabOrder = 2
    OnClick = btnDeleteClick
  end
  object ListBox1: TListBox
    Left = 8
    Top = 24
    Width = 164
    Height = 424
    Anchors = [akLeft, akTop, akBottom]
    ItemHeight = 13
    TabOrder = 3
    OnClick = ListBox1Click
  end
  object btnSaveAs: TButton
    Left = 168
    Top = 456
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Save As'
    TabOrder = 4
    OnClick = btnSaveAsClick
  end
  object btnRename: TButton
    Left = 248
    Top = 456
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Rename'
    TabOrder = 5
    OnClick = btnRenameClick
  end
  object ColorDialog: TColorDialog
    Left = 192
    Top = 328
  end
end
