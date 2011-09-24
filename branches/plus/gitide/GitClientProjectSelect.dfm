object GitProjectSelectDialog: TGitProjectSelectDialog
  Left = 0
  Top = 0
  HelpContext = 15206
  BorderStyle = bsDialog
  Caption = 'Select Project Group or Project to Open'
  ClientHeight = 347
  ClientWidth = 537
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Names: TListView
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 531
    Height = 300
    Align = alClient
    Columns = <
      item
        Caption = 'File Name'
        Width = -2
        WidthType = (
          -2)
      end>
    Groups = <
      item
        Header = 'ProjectGroup'
        GroupID = 0
        State = [lgsNormal]
        HeaderAlign = taLeftJustify
        FooterAlign = taLeftJustify
        TitleImage = -1
      end
      item
        Header = 'Project'
        GroupID = 1
        State = [lgsNormal]
        HeaderAlign = taLeftJustify
        FooterAlign = taLeftJustify
        TitleImage = -1
      end>
    GroupView = True
    SmallImages = GitImageModule.ShellImagesSmall
    TabOrder = 0
    ViewStyle = vsReport
    OnChange = NamesChange
  end
  object Panel1: TPanel
    Left = 0
    Top = 306
    Width = 537
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object Ok: TButton
      Left = 368
      Top = 8
      Width = 75
      Height = 25
      Caption = 'OK'
      Default = True
      Enabled = False
      ModalResult = 1
      TabOrder = 0
    end
    object Cancel: TButton
      Left = 449
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
end
