object SvnSwitchDialog: TSvnSwitchDialog
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'SvnSwitchDialog'
  ClientHeight = 301
  ClientWidth = 537
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    537
    301)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 31
    Height = 13
    Caption = 'Switch'
  end
  object lbPath: TLabel
    Left = 8
    Top = 24
    Width = 30
    Height = 13
    Caption = 'lbPath'
  end
  object Label2: TLabel
    Left = 8
    Top = 55
    Width = 41
    Height = 13
    Caption = 'To path:'
  end
  object Label3: TLabel
    Left = 8
    Top = 88
    Width = 59
    Height = 13
    Caption = 'Source URL:'
  end
  object Label4: TLabel
    Left = 8
    Top = 128
    Width = 80
    Height = 13
    Caption = 'Destination URL:'
  end
  object lbSource: TLabel
    Left = 8
    Top = 104
    Width = 41
    Height = 13
    Caption = 'lbSource'
  end
  object lbDestination: TLabel
    Left = 8
    Top = 144
    Width = 62
    Height = 13
    Caption = 'lbDestination'
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 168
    Width = 262
    Height = 96
    Caption = ' Revision '
    TabOrder = 0
    DesignSize = (
      262
      96)
    object RevisionLabel: TLabel
      Left = 8
      Top = 43
      Width = 44
      Height = 13
      Caption = 'Re&vision:'
      Enabled = False
      FocusControl = SelectRevision
    end
    object CurrentRevision: TCheckBox
      Left = 8
      Top = 20
      Width = 137
      Height = 17
      Caption = '&Current Revision'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = CurrentRevisionClick
    end
    object SelectRevision: TEdit
      Left = 8
      Top = 62
      Width = 139
      Height = 21
      Enabled = False
      TabOrder = 1
    end
    object btnURLLog: TButton
      Left = 167
      Top = 58
      Width = 79
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Show &Log'
      TabOrder = 2
      OnClick = btnURLLogClick
    end
  end
  object Options: TGroupBox
    Left = 276
    Top = 168
    Width = 253
    Height = 96
    Anchors = [akLeft, akTop, akRight]
    Caption = ' Switch Depth '
    TabOrder = 1
    DesignSize = (
      253
      96)
    object DepthSticky: TCheckBox
      Left = 8
      Top = 48
      Width = 105
      Height = 17
      Caption = '&Make depth sticky'
      TabOrder = 0
    end
    object OmitExternals: TCheckBox
      Left = 8
      Top = 71
      Width = 129
      Height = 17
      Caption = 'Omit E&xternals'
      TabOrder = 1
    end
    object Depth: TComboBox
      Left = 8
      Top = 20
      Width = 237
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      ItemIndex = 0
      TabOrder = 2
      Text = 'Working copy'
      Items.Strings = (
        'Working copy'
        'Fully recursive'
        'Immediate children, including folders'
        'Only file children'
        'Only this item'
        'Exclude')
    end
  end
  object URL: TComboBox
    Left = 55
    Top = 52
    Width = 447
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
    OnChange = URLChange
  end
  object BrowseURL: TButton
    Left = 508
    Top = 52
    Width = 21
    Height = 21
    Anchors = [akTop, akRight]
    Caption = '...'
    TabOrder = 3
    OnClick = BrowseURLClick
  end
  object Ok: TButton
    Left = 372
    Top = 270
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 4
  end
  object Cancel: TButton
    Left = 453
    Top = 270
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
  end
end
