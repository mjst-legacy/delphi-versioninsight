{******************************************************************************}
{                                                                              }
{ RAD Studio Version Insight                                                   }
{                                                                              }
{ The contents of this file are subject to the Mozilla Public License          }
{ Version 1.1 (the "License"); you may not use this file except in compliance  }
{ with the License. You may obtain a copy of the License at                    }
{ http://www.mozilla.org/MPL/                                                  }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{ The Original Code is VerInsBlameOptionsFrame.pas.                            }
{                                                                              }
{ The Initial Developer of the Original Code is Uwe Schuster.                  }
{ Portions created by Uwe Schuster are Copyright © 2011 - 2015 Uwe Schuster.   }
{ All Rights Reserved.                                                         }
{                                                                              }
{ Contributors:                                                                }
{ Uwe Schuster (uschuster)                                                     }
{                                                                              }
{******************************************************************************}

unit VerInsBlameOptionsFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls, Buttons, ExtCtrls, ComCtrls, VerInsBlameSettings;

type
  TfrmVerInsBlameOptions = class(TFrame)
    GroupBox1: TGroupBox;
    Button1: TButton;
    btnDelete: TButton;
    Label1: TLabel;
    ListBox1: TListBox;
    cbShowUserText: TCheckBox;
    cbShowUserColor: TCheckBox;
    cbShowRevisionColor: TCheckBox;
    cbShowDateColor: TCheckBox;
    cbShowRevisionText: TCheckBox;
    cbShowDateText: TCheckBox;
    Label2: TLabel;
    cboxRevisionStartColor: TColorBox;
    cboxRevisionEndColor: TColorBox;
    cboxDateStartColor: TColorBox;
    cboxDateEndColor: TColorBox;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    cbDateFormat: TComboBox;
    cbPaintMethod: TComboBox;
    Label6: TLabel;
    lvUserColors: TListView;
    btnAddUser: TButton;
    btnDeleteUser: TButton;
    ColorDialog: TColorDialog;
    btnSaveAs: TButton;
    btnRename: TButton;
    cbShowRevisionColorBar: TCheckBox;
    cbShowDateColorBar: TCheckBox;
    cbShowUserColorBar: TCheckBox;
    procedure ListBox1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnAddUserClick(Sender: TObject);
    procedure lvUserColorsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure btnDeleteUserClick(Sender: TObject);
    procedure btnSaveAsClick(Sender: TObject);
    procedure btnRenameClick(Sender: TObject);
    procedure lvUserColorsResize(Sender: TObject);
    procedure lvUserColorsAdvancedCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; Stage: TCustomDrawStage;
      var DefaultDraw: Boolean);
  private
    { Private declarations }
    FDateFormatList: TStringList;
    FLastIndex: Integer;
    FPresets: TJVCSLineHistoryPresets;
    procedure ControlsToSettings(ASettings: TJVCSLineHistorySettings);
    procedure FillDateFormatComboBox;
    function GetSettings: TJVCSLineHistorySettings;
    procedure SettingsToControls(ASettings: TJVCSLineHistorySettings);
    procedure UpdateButtonState;
    procedure UpdateControlsFromPreset(AIndex: Integer);
    procedure UpdateListBox(AIndex: Integer);
    procedure UpdatePresetFromControls(AIndex: Integer);
    procedure UpdateUserColors(ASettings: TJVCSLineHistorySettings);
    procedure UpdateUserButtonState;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Init;
    procedure UpdatePresets;
    property Presets: TJVCSLineHistoryPresets read FPresets write FPresets;
  end;

implementation

{$R *.dfm}

{ TfrmVerInsBlameOptions }

procedure TfrmVerInsBlameOptions.btnDeleteClick(Sender: TObject);
begin
  if ListBox1.ItemIndex <> -1 then
  begin
    FPresets.Delete(ListBox1.ItemIndex);
    UpdateListBox(0);
  end;
end;

procedure TfrmVerInsBlameOptions.btnDeleteUserClick(Sender: TObject);
var
  Settings: TJVCSLineHistorySettings;
begin
  Settings := GetSettings;
  if Assigned(Settings) and Assigned(lvUserColors.Selected) then
  begin
    Settings := FPresets[ListBox1.ItemIndex].Settings;
    Settings.UserSettingsList.Delete(lvUserColors.Selected.Index);
    UpdateUserColors(Settings);
  end;
end;

procedure TfrmVerInsBlameOptions.btnRenameClick(Sender: TObject);
var
  Name, OldName: string;
begin
  Name := FPresets[ListBox1.ItemIndex].Name;
  OldName := Name;
  if (InputQuery('Rename Preset', 'Preset name', Name)) and (Name <> '') and
    (OldName <> Name) then
  begin
    if FPresets.IndexOf(Name) = -1 then
    begin
      FPresets[ListBox1.ItemIndex].Name := Name;
      UpdateListBox(ListBox1.ItemIndex);
    end
    else
      MessageDlg(Format('The name "%s" does already exists', [Name]), mtError, [mbOK], 0);
  end;
end;

procedure TfrmVerInsBlameOptions.Button1Click(Sender: TObject);
var
  Name: string;
begin
  Name := Format('Preset %d', [FPresets.Count + 1]);
  if (InputQuery('Add Preset', 'Preset name', Name)) and (Name <> '') then
  begin
    if FPresets.IndexOf(Name) = -1 then
    begin
      FPresets.Add(Name);
      UpdateListBox(FPresets.Count - 1);
    end
    else
      MessageDlg(Format('The name "%s" does already exists', [Name]), mtError, [mbOK], 0);
  end;
end;

procedure TfrmVerInsBlameOptions.btnAddUserClick(Sender: TObject);
var
  UserName, VisibleName: string;
  UserSettingsItem: TJVCSLineHistoryUserSettingsItem;
  Settings: TJVCSLineHistorySettings;
begin
  Settings := GetSettings;
  if Assigned(Settings) and InputQuery('Add User', 'User name', UserName) then
  begin
    if Settings.UserSettingsList.IndexOfUser(UserName) <> -1 then
    begin

    end
    else
    begin
      ColorDialog.Color := clNone;
      if ColorDialog.Execute then
      begin
        VisibleName := '';
        InputQuery('Add User', 'User visible name', VisibleName);
        UserSettingsItem := Settings.UserSettingsList.Add;
        UserSettingsItem.UserName := UserName;
        UserSettingsItem.Color := ColorDialog.Color;
        UserSettingsItem.VisibleName := VisibleName;
        UpdateUserColors(Settings);
      end;
    end;
  end;
end;

procedure TfrmVerInsBlameOptions.btnSaveAsClick(Sender: TObject);
var
  Name: string;
  Preset: TJVCSLineHistoryPreset;
  Settings: TJVCSLineHistorySettings;
begin
  Name := Format('Preset %d', [FPresets.Count + 1]);
  if (InputQuery('Save Preset As', 'Preset name', Name)) and (Name <> '') then
  begin
    if FPresets.IndexOf(Name) = -1 then
    begin
      Preset := FPresets.Add(Name);
      ControlsToSettings(Preset.Settings);
      Settings := GetSettings;
      if Assigned(Settings) then
        Preset.Settings.UserSettingsList.Assign(Settings.UserSettingsList);
      UpdateListBox(FPresets.Count - 1);
    end
    else
      MessageDlg(Format('The name "%s" does already exists', [Name]), mtError, [mbOK], 0);
  end;
end;

procedure TfrmVerInsBlameOptions.ControlsToSettings(ASettings: TJVCSLineHistorySettings);
var
  Fmt: string;
  Idx: Integer;
begin
  ASettings.PaintMethod := cbPaintMethod.ItemIndex;
  ASettings.ShowRevisionInfoText := cbShowRevisionText.Checked;
  ASettings.ShowRevisionInfoColor := cbShowRevisionColor.Checked;
  ASettings.RevisionStartColor := cboxRevisionStartColor.Selected;
  ASettings.RevisionEndColor := cboxRevisionEndColor.Selected;
  ASettings.ShowDateInfoText := cbShowDateText.Checked;
  ASettings.ShowDateInfoColor := cbShowDateColor.Checked;
  ASettings.DateStartColor := cboxDateStartColor.Selected;
  ASettings.DateEndColor := cboxDateEndColor.Selected;

  Fmt := '';
  if (cbDateFormat.ItemIndex > -1) and (cbDateFormat.ItemIndex < cbDateFormat.Items.Count) then
  begin
    Idx := Integer(cbDateFormat.Items.Objects[cbDateFormat.ItemIndex]);
    if (Idx > -1) and (Idx < FDateFormatList.Count) then
      Fmt := FDateFormatList[Idx];
  end;
  ASettings.DateFormat := Fmt;

  ASettings.ShowUserInfoText := cbShowUserText.Checked;
  ASettings.ShowUserInfoColor := cbShowUserColor.Checked;
  ASettings.ColorBarOrderList.Clear;
  if cbShowRevisionColorBar.Checked then
    ASettings.ColorBarOrderList.Add(Pointer(1));
  if cbShowDateColorBar.Checked then
    ASettings.ColorBarOrderList.Add(Pointer(2));
  if cbShowUserColorBar.Checked then
    ASettings.ColorBarOrderList.Add(Pointer(3));
end;

constructor TfrmVerInsBlameOptions.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDateFormatList := TStringList.Create;
  FillDateFormatComboBox;
end;

destructor TfrmVerInsBlameOptions.Destroy;
begin
  FDateFormatList.Free;
  inherited Destroy;
end;

procedure TfrmVerInsBlameOptions.FillDateFormatComboBox;

  procedure AddDateFormatToComboBox(AText, AFormat: string);
  begin
    cbDateFormat.Items.AddObject(AText, TObject(FDateFormatList.Count));
    FDateFormatList.Add(AFormat);
  end;

begin
  if FDateFormatList.Count = 0 then
  begin
    cbDateFormat.Items.Clear;
    AddDateFormatToComboBox('Age', AgeDateFormat);
    AddDateFormatToComboBox('Age2', Age2DateFormat);
    AddDateFormatToComboBox('System', '');
    AddDateFormatToComboBox('yyyy/mm/dd', 'yyyy"/"mm"/"dd');
    AddDateFormatToComboBox('yyyy/mm/dd hh:nn', 'yyyy"/"mm"/"dd hh:nn');
    AddDateFormatToComboBox('yyyy-mm-dd', 'yyyy"-"mm"-"dd');
    AddDateFormatToComboBox('yyyy-mm-dd hh:nn', 'yyyy"-"mm"-"dd hh:nn');
  end;
end;

function TfrmVerInsBlameOptions.GetSettings: TJVCSLineHistorySettings;
begin
  if ListBox1.ItemIndex <> -1 then
    Result := FPresets[ListBox1.ItemIndex].Settings
  else
    Result := nil;
end;

procedure TfrmVerInsBlameOptions.Init;
var
  Idx: Integer;
begin
  Idx := FPresets.IndexOfID(FPresets.SelectedID);
  if Idx = -1 then
    Idx := 0;
  UpdateListBox(Idx);
end;

procedure TfrmVerInsBlameOptions.ListBox1Click(Sender: TObject);
begin
  UpdatePresetFromControls(FLastIndex);
  UpdateControlsFromPreset(ListBox1.ItemIndex);
  FLastIndex := ListBox1.ItemIndex;
end;

procedure TfrmVerInsBlameOptions.lvUserColorsAdvancedCustomDrawItem(
  Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
  Stage: TCustomDrawStage; var DefaultDraw: Boolean);
var
  R: TRect;
  Settings: TJVCSLineHistorySettings;
begin
  if Stage = cdPostPaint then
  begin
    R := Item.DisplayRect(drBounds);
    R.Left := R.Left + Sender.Column[0].Width + 2;
    R.Right := R.Left + Sender.Column[1].Width - 4;
    R.Top := R.Top + 2;
    R.Bottom := R.Bottom - 2;
    Settings := GetSettings;
    if Assigned(Settings) and (Item.Index < Settings.UserSettingsList.Count) then
      Sender.Canvas.Brush.Color := Settings.UserSettingsList[Item.Index].Color
    else
      Sender.Canvas.Brush.Color := clWindowText;
    Sender.Canvas.Rectangle(R);
    SetBkMode(Sender.Canvas.Handle, TRANSPARENT);//fixes strange black background for selected lines
    DefaultDraw := False;
  end;
end;

procedure TfrmVerInsBlameOptions.lvUserColorsResize(Sender: TObject);
begin
  lvUserColors.Height := btnAddUser.Top - lvUserColors.Top - 8;
end;

procedure TfrmVerInsBlameOptions.lvUserColorsSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  UpdateUserButtonState;
end;

procedure TfrmVerInsBlameOptions.SettingsToControls(ASettings: TJVCSLineHistorySettings);
var
  Fmt: string;
  I, Idx: Integer;
begin
  if (ASettings.PaintMethod >= 0) and (ASettings.PaintMethod < cbPaintMethod.Items.Count) then
    cbPaintMethod.ItemIndex := ASettings.PaintMethod;
  cbShowRevisionText.Checked := ASettings.ShowRevisionInfoText;
  cbShowRevisionColor.Checked := ASettings.ShowRevisionInfoColor;
  cboxRevisionStartColor.Selected := ASettings.RevisionStartColor;
  cboxRevisionEndColor.Selected := ASettings.RevisionEndColor;
  cbShowDateText.Checked := ASettings.ShowDateInfoText;
  cbShowDateColor.Checked := ASettings.ShowDateInfoColor;
  cboxDateStartColor.Selected := ASettings.DateStartColor;
  cboxDateEndColor.Selected := ASettings.DateEndColor;

  Fmt := '';
  Idx := FDateFormatList.IndexOf(ASettings.DateFormat);
  if Idx = -1 then
    Idx := 0;
  Fmt := FDateFormatList[Idx];
  for I := 0 to Pred(cbDateFormat.Items.Count) do
    if cbDateFormat.Items.Objects[I] = TObject(Idx) then
    begin
      cbDateFormat.ItemIndex := I;
      Break;
    end;

  cbShowUserText.Checked := ASettings.ShowUserInfoText;
  cbShowUserColor.Checked := ASettings.ShowUserInfoColor;
  UpdateUserColors(ASettings);
  cbShowRevisionColorBar.Checked := ASettings.ColorBarOrderList.IndexOf(Pointer(1)) <> -1;
  cbShowDateColorBar.Checked := ASettings.ColorBarOrderList.IndexOf(Pointer(2)) <> -1;
  cbShowUserColorBar.Checked := ASettings.ColorBarOrderList.IndexOf(Pointer(3)) <> -1;
end;

procedure TfrmVerInsBlameOptions.UpdateButtonState;
begin
  btnDelete.Enabled := ListBox1.ItemIndex <> -1;
  btnSaveAs.Enabled := ListBox1.ItemIndex <> -1;
  btnRename.Enabled := ListBox1.ItemIndex <> -1;
end;

procedure TfrmVerInsBlameOptions.UpdateControlsFromPreset(AIndex: Integer);
begin
  if AIndex <> -1 then
    SettingsToControls(FPresets[AIndex].Settings);
end;

procedure TfrmVerInsBlameOptions.UpdateListBox(AIndex: Integer);
var
  I: Integer;
  S: string;
begin
  ListBox1.Items.BeginUpdate;
  try
    ListBox1.Items.Clear;
    for I := 0 to FPresets.Count - 1 do
    begin
      S := FPresets[I].Name;
      if FPresets[I].ID = FPresets.SelectedID then
        S := S + ' (Active)';
      ListBox1.Items.Add(S);
    end;
    if (AIndex >= 0) and (AIndex < ListBox1.Items.Count) then
    begin
      ListBox1.ItemIndex := AIndex;
      UpdateControlsFromPreset(AIndex);
      FLastIndex := AIndex;
    end;
  finally
    ListBox1.Items.EndUpdate;
  end;
  UpdateButtonState;
end;

procedure TfrmVerInsBlameOptions.UpdatePresetFromControls(AIndex: Integer);
begin
  if AIndex <> -1 then
    ControlsToSettings(FPresets[AIndex].Settings);
end;

procedure TfrmVerInsBlameOptions.UpdatePresets;
begin
  if FLastIndex < FPresets.Count then
    UpdatePresetFromControls(FLastIndex);
end;

procedure TfrmVerInsBlameOptions.UpdateUserButtonState;
begin
  btnDeleteUser.Enabled := Assigned(lvUserColors.Selected);
end;

procedure TfrmVerInsBlameOptions.UpdateUserColors(ASettings: TJVCSLineHistorySettings);
var
  I: Integer;
  Item: TListItem;
begin
  lvUserColors.Items.BeginUpdate;
  try
    lvUserColors.Items.Clear;
    if Assigned(ASettings) then
      for I := 0 to ASettings.UserSettingsList.Count - 1 do
      begin
        Item := lvUserColors.Items.Add;
        Item.Caption := ASettings.UserSettingsList[I].UserName;
        Item.SubItems.Add('WWW');
        Item.SubItems.Add(ASettings.UserSettingsList[I].VisibleName);
      end;
    UpdateUserButtonState;
  finally
    lvUserColors.Items.EndUpdate;
  end;
end;

end.
