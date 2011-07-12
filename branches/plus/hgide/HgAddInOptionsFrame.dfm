object frmHgTestsOptions: TfrmHgTestsOptions
  Left = 0
  Top = 0
  Width = 414
  Height = 240
  TabOrder = 0
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 393
    Height = 65
    Caption = ' Mercurial Options '
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 16
      Width = 69
      Height = 13
      Caption = 'Hg Executable'
    end
    object SpeedButton1: TSpeedButton
      Left = 363
      Top = 32
      Width = 23
      Height = 22
      Caption = '...'
      OnClick = SpeedButton1Click
    end
    object edHgExecutable: TEdit
      Left = 8
      Top = 33
      Width = 353
      Height = 21
      TabOrder = 0
    end
  end
  object OpenDialog1: TOpenDialog
    Filter = '*.exe|*.exe'
    Left = 8
    Top = 80
  end
end
