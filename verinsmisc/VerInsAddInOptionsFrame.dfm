object frmVerInsOptions: TfrmVerInsOptions
  Left = 0
  Top = 0
  Width = 414
  Height = 240
  TabOrder = 0
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 393
    Height = 97
    Caption = ' Options '
    TabOrder = 0
    object cbEnableGit: TCheckBox
      Left = 5
      Top = 22
      Width = 97
      Height = 17
      Caption = 'Enable Git'
      TabOrder = 0
      OnClick = cbEnableGitClick
    end
    object cbEnableSubversion: TCheckBox
      Left = 5
      Top = 70
      Width = 124
      Height = 17
      Caption = 'Enable Subversion'
      TabOrder = 1
      OnClick = cbEnableGitClick
    end
    object cbEnableMercurial: TCheckBox
      Left = 5
      Top = 46
      Width = 116
      Height = 17
      Caption = 'Enable Mercurial'
      TabOrder = 2
      OnClick = cbEnableGitClick
    end
  end
end
