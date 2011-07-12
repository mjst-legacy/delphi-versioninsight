object HgProgressDialog: THgProgressDialog
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Compare'
  ClientHeight = 84
  ClientWidth = 337
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
  object lbInfo: TLabel
    Left = 8
    Top = 8
    Width = 28
    Height = 13
    Caption = 'lbInfo'
  end
  object btnAbort: TButton
    Left = 254
    Top = 50
    Width = 75
    Height = 25
    Caption = 'Abort'
    ModalResult = 3
    TabOrder = 0
    OnClick = btnAbortClick
  end
  object ProgressBar1: TProgressBar
    Left = 8
    Top = 27
    Width = 321
    Height = 17
    TabOrder = 1
  end
end
