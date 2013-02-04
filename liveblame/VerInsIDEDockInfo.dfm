inherited fmLiveBlameInfo: TfmLiveBlameInfo
  Caption = 'Live Blame Information'
  ClientHeight = 340
  ClientWidth = 369
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel [0]
    Left = 0
    Top = 107
    Width = 369
    Height = 153
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 111
      Width = 72
      Height = 13
      Caption = 'Current Line:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label2: TLabel
      Left = 8
      Top = 4
      Width = 95
      Height = 13
      Caption = 'Current Method: '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object PaintBox1: TPaintBox
      Left = 8
      Top = 20
      Width = 353
      Height = 22
      OnPaint = PaintBox1Paint
    end
    object PaintBox2: TPaintBox
      Left = 8
      Top = 127
      Width = 353
      Height = 22
      OnPaint = PaintBox2Paint
    end
    object PaintBox3: TPaintBox
      Left = 8
      Top = 48
      Width = 353
      Height = 57
      OnPaint = PaintBox3Paint
    end
    object Label3: TLabel
      Left = 109
      Top = 4
      Width = 9
      Height = 13
      Caption = '   '
    end
    object Label4: TLabel
      Left = 109
      Top = 111
      Width = 9
      Height = 13
      Caption = '   '
    end
  end
  object Memo1: TMemo [1]
    AlignWithMargins = True
    Left = 8
    Top = 260
    Width = 353
    Height = 72
    Margins.Left = 8
    Margins.Top = 0
    Margins.Right = 8
    Margins.Bottom = 8
    Align = alClient
    TabOrder = 1
  end
  object Panel2: TPanel [2]
    Left = 0
    Top = 0
    Width = 369
    Height = 107
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    object Label5: TLabel
      Left = 8
      Top = 4
      Width = 57
      Height = 13
      Caption = 'Whole File'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object PaintBox4: TPaintBox
      Left = 8
      Top = 48
      Width = 353
      Height = 57
      OnPaint = PaintBox4Paint
    end
    object PaintBox5: TPaintBox
      Left = 8
      Top = 20
      Width = 353
      Height = 22
      OnPaint = PaintBox5Paint
    end
  end
end
