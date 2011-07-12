object SvnRangeSelectDialog: TSvnRangeSelectDialog
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Select Revision Range'
  ClientHeight = 147
  ClientWidth = 329
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox2: TGroupBox
    Left = 8
    Top = 8
    Width = 153
    Height = 96
    Caption = 'From Revision'
    TabOrder = 0
    object FromRevisionLabel: TLabel
      Left = 8
      Top = 43
      Width = 44
      Height = 13
      Caption = 'Re&vision:'
      FocusControl = FromSelectRevision
    end
    object FromSelectRevision: TEdit
      Left = 8
      Top = 62
      Width = 139
      Height = 21
      TabOrder = 0
      OnChange = FromSelectRevisionChange
    end
  end
  object GroupBox1: TGroupBox
    Left = 167
    Top = 8
    Width = 154
    Height = 96
    Caption = 'To Revision'
    TabOrder = 1
    object ToRevisionLabel: TLabel
      Left = 8
      Top = 43
      Width = 44
      Height = 13
      Caption = 'Re&vision:'
      Enabled = False
      FocusControl = ToSelectRevision
    end
    object ToCurrentRevision: TCheckBox
      Left = 8
      Top = 20
      Width = 137
      Height = 17
      Caption = '&Current Revision'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = ToCurrentRevisionClick
    end
    object ToSelectRevision: TEdit
      Left = 8
      Top = 62
      Width = 139
      Height = 21
      Enabled = False
      TabOrder = 1
      OnChange = FromSelectRevisionChange
    end
  end
  object Button1: TButton
    Left = 165
    Top = 114
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    Enabled = False
    ModalResult = 1
    TabOrder = 2
  end
  object Button2: TButton
    Left = 246
    Top = 114
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
end
