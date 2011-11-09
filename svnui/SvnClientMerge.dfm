object SvnMergeDialog: TSvnMergeDialog
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'SvnMergeDialog'
  ClientHeight = 353
  ClientWidth = 544
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
    Top = 310
    Width = 544
    Height = 43
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      544
      43)
    object btnBack: TButton
      Left = 296
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '< &Back'
      TabOrder = 0
      OnClick = btnBackClick
    end
    object btnNext: TButton
      Left = 377
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Next >'
      TabOrder = 1
      OnClick = btnNextClick
    end
    object Button3: TButton
      Left = 458
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 2
    end
  end
  object PageControl: TPageControl
    Left = 0
    Top = 0
    Width = 544
    Height = 310
    ActivePage = tsMergeType
    Align = alClient
    Style = tsFlatButtons
    TabOrder = 1
    object tsMergeType: TTabSheet
      Caption = 'Merge type'
      TabVisible = False
      DesignSize = (
        536
        300)
      object GroupBox1: TGroupBox
        Left = 7
        Top = 0
        Width = 522
        Height = 297
        Anchors = [akLeft, akTop, akRight, akBottom]
        Caption = ' Merge type '
        TabOrder = 0
        DesignSize = (
          522
          297)
        object Label1: TLabel
          Left = 33
          Top = 47
          Width = 469
          Height = 26
          Anchors = [akLeft, akTop, akRight]
          AutoSize = False
          Caption = 
            'This method covers the case when you have made one or more revis' +
            'ions to a branch (or to the trunk) and you want to port those ch' +
            'anges across to a different branch.'
          WordWrap = True
        end
        object RadioButton1: TRadioButton
          Left = 10
          Top = 24
          Width = 169
          Height = 17
          Caption = 'Merge a &range of revisions'
          Checked = True
          TabOrder = 0
          TabStop = True
        end
        object RadioButton2: TRadioButton
          Left = 10
          Top = 120
          Width = 121
          Height = 17
          Caption = 'R&eintegrate a branch'
          Enabled = False
          TabOrder = 1
        end
        object RadioButton3: TRadioButton
          Left = 10
          Top = 208
          Width = 145
          Height = 17
          Caption = 'Merge two different &trees'
          Enabled = False
          TabOrder = 2
        end
      end
    end
    object tsMergeRevisionRange: TTabSheet
      Caption = 'Merge revision range'
      ImageIndex = 1
      TabVisible = False
      DesignSize = (
        536
        300)
      object GroupBox2: TGroupBox
        Left = 7
        Top = 0
        Width = 522
        Height = 57
        Anchors = [akLeft, akTop, akRight]
        Caption = ' URL to merge &from '
        TabOrder = 0
        DesignSize = (
          522
          57)
        object URL: TComboBox
          Left = 10
          Top = 20
          Width = 474
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 0
        end
        object BrowseURL: TButton
          Left = 490
          Top = 20
          Width = 21
          Height = 21
          Anchors = [akTop, akRight]
          Caption = '...'
          TabOrder = 1
          OnClick = BrowseURLClick
        end
      end
      object GroupBox3: TGroupBox
        Left = 7
        Top = 63
        Width = 522
        Height = 170
        Anchors = [akLeft, akTop, akRight]
        Caption = ' Revision r&ange to merge '
        TabOrder = 1
        DesignSize = (
          522
          170)
        object Label2: TLabel
          Left = 10
          Top = 70
          Width = 501
          Height = 91
          Anchors = [akLeft, akTop, akRight]
          AutoSize = False
          Caption = 
            'Use the log dialog to select the revisions you want to merge, or' +
            ' enter the revisions to merge, separated by commas. A revision r' +
            'ange can be specified by a dash.'#13#10#13#10'Example: 4-7,9,11,15-HEAD@pe' +
            'grevision'#13#10#13#10'To merge all revisions, leave the box empty.'
          WordWrap = True
          ExplicitWidth = 500
        end
        object Revisions: TEdit
          Left = 10
          Top = 20
          Width = 416
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 0
        end
        object ReverseMerge: TCheckBox
          Left = 10
          Top = 47
          Width = 97
          Height = 17
          Caption = 'R&everse Merge'
          TabOrder = 1
        end
        object btnURLLog: TButton
          Left = 432
          Top = 18
          Width = 79
          Height = 25
          Anchors = [akTop, akRight]
          Caption = 'Show &Log'
          TabOrder = 2
          OnClick = btnURLLogClick
        end
      end
      object GroupBox4: TGroupBox
        Left = 7
        Top = 239
        Width = 522
        Height = 58
        Anchors = [akLeft, akTop, akRight]
        Caption = ' Working Copy '
        TabOrder = 2
        object WCDir: TLabel
          Left = 10
          Top = 24
          Width = 30
          Height = 13
          Caption = 'WCDir'
        end
        object btnWCLog: TButton
          Left = 431
          Top = 16
          Width = 79
          Height = 25
          Anchors = [akTop, akRight]
          Caption = 'Show L&og'
          TabOrder = 0
          OnClick = btnWCLogClick
        end
      end
    end
    object tsMergeOptions: TTabSheet
      Caption = 'Merge options'
      ImageIndex = 2
      TabVisible = False
      DesignSize = (
        536
        300)
      object GroupBox5: TGroupBox
        Left = 7
        Top = 0
        Width = 522
        Height = 182
        Anchors = [akLeft, akTop, akRight]
        Caption = ' Merge revision range : Merge options '
        TabOrder = 0
        DesignSize = (
          522
          182)
        object Label3: TLabel
          Left = 10
          Top = 24
          Width = 65
          Height = 13
          Caption = 'Merge &depth:'
        end
        object IgnoreAncestry: TCheckBox
          Left = 10
          Top = 56
          Width = 97
          Height = 17
          Caption = 'Ignore &ancestry'
          TabOrder = 0
        end
        object IgnoreEOL: TCheckBox
          Left = 10
          Top = 79
          Width = 113
          Height = 17
          Caption = 'Ignore line &endings'
          TabOrder = 1
        end
        object RadioButton4: TRadioButton
          Left = 10
          Top = 110
          Width = 145
          Height = 17
          Caption = '&Compare whitespaces'
          Checked = True
          TabOrder = 2
          TabStop = True
        end
        object IgnoreSpace: TRadioButton
          Left = 10
          Top = 133
          Width = 161
          Height = 17
          Caption = 'Ignore whitespace c&hanges'
          TabOrder = 3
        end
        object IgnoreAllSpace: TRadioButton
          Left = 10
          Top = 156
          Width = 145
          Height = 17
          Caption = 'I&gnore all whitespaces'
          TabOrder = 4
        end
        object Depth: TComboBox
          Left = 296
          Top = 21
          Width = 215
          Height = 21
          Style = csDropDownList
          Anchors = [akLeft, akTop, akRight]
          ItemIndex = 0
          TabOrder = 5
          Text = 'Working copy'
          Items.Strings = (
            'Working copy'
            'Fully recursive'
            'Immediate children, including folders'
            'Only file children'
            'Only this item')
        end
      end
      object Force: TCheckBox
        Left = 17
        Top = 199
        Width = 97
        Height = 17
        Hint = 'Discards local changes upon incoming deletes'
        Caption = 'F&orce the merge'
        TabOrder = 1
      end
      object RecordOnly: TCheckBox
        Left = 17
        Top = 222
        Width = 318
        Height = 17
        Hint = 
          'Marks the revisions as merged, without actually doing the merge.' +
          #13#10'This blocks the revisions from getting merged in the future.'
        Caption = 'Onl&y record the merge (block revisions from gettings merged)'
        TabOrder = 2
      end
      object Button1: TButton
        Left = 454
        Top = 272
        Width = 75
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = '&Test merge'
        TabOrder = 3
        OnClick = Button1Click
      end
    end
  end
end
