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
{ The Original Code is VerInsBlameSettings.pas.                                }
{                                                                              }
{ The Initial Developer of the Original Code is Uwe Schuster.                  }
{ Portions created by Uwe Schuster are Copyright © 2011 Uwe Schuster. All      }
{ Rights Reserved.                                                             }
{                                                                              }
{ Contributors:                                                                }
{ Uwe Schuster (uschuster)                                                     }
{                                                                              }
{******************************************************************************}

unit VerInsBlameSettings;

interface

uses
  SysUtils, Classes, Contnrs, Generics.Collections, Graphics;

const
  AgeDateFormat = 'AGE';
  Age2DateFormat = 'AGE2';

type
  TJVCSLineHistoryUserSettingsItem = class(TPersistent)
  private
    FColor: TColor;
    FUserName: string;
    FVisibleName: string;
  public
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create;
    property Color: TColor read FColor write FColor;
    property UserName: string read FUserName write FUserName;
    property VisibleName: string read FVisibleName write FVisibleName;
  end;

  TJVCSLineHistoryUserSettings = class(TPersistent)
  private
    FItems: TObjectList;
    function GetCount: Integer;
    function GetItems(AIndex: Integer): TJVCSLineHistoryUserSettingsItem;
  public
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create;
    destructor Destroy; override;
    function Add: TJVCSLineHistoryUserSettingsItem;
    procedure Clear;
    procedure Delete(AIndex: Integer);
    function IndexOfUser(const AUserName: string): Integer;
    property Count: Integer read GetCount;
    property Items[AIndex: Integer]: TJVCSLineHistoryUserSettingsItem read GetItems; default;
  end;

  TJVCSLineHistorySettings = class(TObject)
  private
    FColorBarOrderList: TList;
    FDateEndColor: TColor;
    FDateFormat: string;
    FDateStartColor: TColor;
    FLineColorMode: Integer;
    FPaintMethod: Integer;
    FRevisionEndColor: TColor;
    FRevisionStartColor: TColor;
    FShowLineNumbers: Boolean;
    FShowRevisionInfoColor: Boolean;
    FShowRevisionInfoText: Boolean;
    FShowDateInfoColor: Boolean;
    FShowDateInfoText: Boolean;
    FShowUserInfoColor: Boolean;
    FShowUserInfoText: Boolean;
    {$IFDEF LINEINFOEX}
    FShowRevisionCountInfoColor: Boolean;
    FShowRevisionCountInfoText: Boolean;
    FShowFirstRevisionInfoColor: Boolean;
    FShowFirstRevisionInfoText: Boolean;
    {$ENDIF LINEINFOEX}
    FShowOrderList: TList;
    FStaticUserColorList: TStringList;
    FSuppressRevisionTextZeroDot: Boolean;
    FUserSettingsList: TJVCSLineHistoryUserSettings;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(AValue: TJVCSLineHistorySettings);
    property ColorBarOrderList: TList read FColorBarOrderList;
    property DateEndColor: TColor read FDateEndColor write FDateEndColor;
    property DateFormat: string read FDateFormat write FDateFormat;
    property DateStartColor: TColor read FDateStartColor write FDateStartColor;
    property LineColorMode: Integer read FLineColorMode write FLineColorMode;
    property PaintMethod: Integer read FPaintMethod write FPaintMethod;
    property RevisionEndColor: TColor read FRevisionEndColor write FRevisionEndColor;
    property RevisionStartColor: TColor read FRevisionStartColor write FRevisionStartColor;
    property ShowLineNumbers: Boolean read FShowLineNumbers write FShowLineNumbers;
    property ShowRevisionInfoColor: Boolean read FShowRevisionInfoColor write FShowRevisionInfoColor;
    property ShowRevisionInfoText: Boolean read FShowRevisionInfoText write FShowRevisionInfoText;
    property ShowDateInfoColor: Boolean read FShowDateInfoColor write FShowDateInfoColor;
    property ShowDateInfoText: Boolean read FShowDateInfoText write FShowDateInfoText;
    property ShowUserInfoColor: Boolean read FShowUserInfoColor write FShowUserInfoColor;
    property ShowUserInfoText: Boolean read FShowUserInfoText write FShowUserInfoText;
    {$IFDEF LINEINFOEX}
    property ShowRevisionCountInfoColor: Boolean read FShowRevisionCountInfoColor write FShowRevisionCountInfoColor;
    property ShowRevisionCountInfoText: Boolean read FShowRevisionCountInfoText write FShowRevisionCountInfoText;
    property ShowFirstRevisionInfoColor: Boolean read FShowFirstRevisionInfoColor write FShowFirstRevisionInfoColor;
    property ShowFirstRevisionInfoText: Boolean read FShowFirstRevisionInfoText write FShowFirstRevisionInfoText;
    {$ENDIF LINEINFOEX}
    property ShowOrderList: TList read FShowOrderList;
    property StaticUserColorList: TStringList read FStaticUserColorList;
    property SuppressRevisionTextZeroDot: Boolean read FSuppressRevisionTextZeroDot write FSuppressRevisionTextZeroDot;
    property UserSettingsList: TJVCSLineHistoryUserSettings read FUserSettingsList;
  end;

  TJVCSLineHistoryPreset = class(TObject)
  private
    FID: Integer;
    FName: string;
    FSettings: TJVCSLineHistorySettings;
  public
    constructor Create;
    destructor Destroy; override;
    property ID: Integer read FID write FID;
    property Name: string read FName write FName;
    property Settings: TJVCSLineHistorySettings read FSettings;
  end;

  TJVCSLineHistoryPresets = class(TObject)
  private
    FItems: TObjectList<TJVCSLineHistoryPreset>;
    FModificationTimestamp: TDateTime;
    FSelectedID: Integer;
    FKey: string;
    procedure AddDefaultPresets;
    function GetCount: Integer;
    function GetItems(AIndex: Integer): TJVCSLineHistoryPreset;
    procedure LoadSettings(AKeyName: string; ASettings: TJVCSLineHistorySettings);
    procedure SaveSettings(AKeyName: string; ASettings: TJVCSLineHistorySettings);
    procedure SetSelectedID(const Value: Integer);
  public
    constructor Create(AKey: string = '');
    destructor Destroy; override;
    function Add(const AName: string): TJVCSLineHistoryPreset;
    procedure Clear;
    procedure Delete(AIndex: Integer);
    function IndexOf(const AName: string): Integer;
    function IndexOfID(AID: Integer): Integer;
    function GetNextID: Integer;
    procedure Load;
    procedure Save;
    procedure SetModified;
    property Count: Integer read GetCount;
    property Items[AIndex: Integer]: TJVCSLineHistoryPreset read GetItems; default;
    property ModificationTimestamp: TDateTime read FModificationTimestamp;
    property SelectedID: Integer read FSelectedID write SetSelectedID;
  end;

function GetPresets: TJVCSLineHistoryPresets;
procedure SetPresetsKey(const AKey: string);

implementation

uses
  Registry;

constructor TJVCSLineHistoryUserSettingsItem.Create;
begin
  inherited Create;
  FColor := clNone;
  FUserName := '';
  FVisibleName := '';
end;

procedure TJVCSLineHistoryUserSettingsItem.AssignTo(Dest: TPersistent);
begin
  if Dest is TJVCSLineHistoryUserSettingsItem then
  begin
    TJVCSLineHistoryUserSettingsItem(Dest).Color := FColor;
    TJVCSLineHistoryUserSettingsItem(Dest).UserName := FUserName;
    TJVCSLineHistoryUserSettingsItem(Dest).VisibleName := FVisibleName;
  end
  else
    inherited AssignTo(Dest);
end;

constructor TJVCSLineHistoryUserSettings.Create;
begin
  inherited Create;
  FItems := TObjectList.Create;
end;

destructor TJVCSLineHistoryUserSettings.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

function TJVCSLineHistoryUserSettings.Add: TJVCSLineHistoryUserSettingsItem;
begin
  FItems.Add(TJVCSLineHistoryUserSettingsItem.Create);
  Result := TJVCSLineHistoryUserSettingsItem(FItems.Last);
end;

procedure TJVCSLineHistoryUserSettings.AssignTo(Dest: TPersistent);
var
  I: Integer;
begin
  if Dest is TJVCSLineHistoryUserSettings then
  begin
    TJVCSLineHistoryUserSettings(Dest).Clear;
    for I := 0 to Pred(Count) do
      TJVCSLineHistoryUserSettings(Dest).Add.Assign(Items[I]);
  end
  else
    inherited AssignTo(Dest);
end;

procedure TJVCSLineHistoryUserSettings.Clear;
begin
  FItems.Clear;
end;

procedure TJVCSLineHistoryUserSettings.Delete(AIndex: Integer);
begin
  FItems.Delete(AIndex);
end;

function TJVCSLineHistoryUserSettings.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TJVCSLineHistoryUserSettings.GetItems(AIndex: Integer): TJVCSLineHistoryUserSettingsItem;
begin
  Result := TJVCSLineHistoryUserSettingsItem(FItems[AIndex]);
end;

function TJVCSLineHistoryUserSettings.IndexOfUser(const AUserName: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Pred(Count) do
    if Items[I].UserName = AUserName then
    begin
      Result := I;
      Break;
    end;
end;

constructor TJVCSLineHistorySettings.Create;
begin
  inherited Create;
  FColorBarOrderList := TList.Create;
  {//not supported in IDE
  FColorBarOrderList.Add(Pointer(1));
  FColorBarOrderList.Add(Pointer(2));
  FColorBarOrderList.Add(Pointer(3));
  }
  FDateEndColor := clWhite;
  FDateFormat := 'yyyy"/"mm"/"dd';
  FDateStartColor := clRed;
  FLineColorMode := 0;
  FPaintMethod := 0;
  FRevisionEndColor := clWhite;
  FRevisionStartColor := clYellow;
  FShowLineNumbers := False;//True;//not supported in IDE
  FShowRevisionInfoColor := True;
  FShowRevisionInfoText := True;
  FShowDateInfoColor := True;
  FShowDateInfoText := True;
  FShowUserInfoColor := True;
  FShowUserInfoText := True;
  {$IFDEF LINEINFOEX}
  FShowRevisionCountInfoColor := True;
  FShowRevisionCountInfoText := True;
  FShowFirstRevisionInfoColor := True;
  FShowFirstRevisionInfoText := True;
  {$ENDIF LINEINFOEX}
  FShowOrderList := TList.Create;
  FStaticUserColorList := TStringList.Create;
  FSuppressRevisionTextZeroDot := False;
  FUserSettingsList := TJVCSLineHistoryUserSettings.Create;
end;

destructor TJVCSLineHistorySettings.Destroy;
begin
  FColorBarOrderList.Free;
  FShowOrderList.Free;
  FStaticUserColorList.Free;
  FUserSettingsList.Free;
  inherited Destroy;
end;

{$IFNDEF DELPHI6_UP}
procedure AssignTList(ADest, ASource: TList);
var
  I: Integer;
begin
  ADest.Clear;
  for I := 0 to Pred(ASource.Count) do
    ADest.Add(ASource[I]);
end;
{$ENDIF ~DELPHI6_UP}

procedure TJVCSLineHistorySettings.Assign(AValue: TJVCSLineHistorySettings);
begin
  {$IFDEF DELPHI6_UP}
  FColorBarOrderList.Assign(AValue.ColorBarOrderList);
  {$ELSE}
  AssignTList(FColorBarOrderList, AValue.ColorBarOrderList);
  {$ENDIF DELPHI6_UP}
  FDateEndColor := AValue.DateEndColor;
  FDateFormat := AValue.DateFormat;
  FDateStartColor := AValue.DateStartColor;
  FLineColorMode := AValue.LineColorMode;
  FPaintMethod := AValue.PaintMethod;
  FRevisionEndColor := AValue.RevisionEndColor;
  FRevisionStartColor := AValue.RevisionStartColor;
  FShowLineNumbers := AValue.FShowLineNumbers;
  FShowRevisionInfoColor := AValue.ShowRevisionInfoColor;
  FShowRevisionInfoText := AValue.ShowRevisionInfoText;
  FShowDateInfoColor := AValue.ShowDateInfoColor;
  FShowDateInfoText := AValue.ShowDateInfoText;
  FShowUserInfoColor := AValue.ShowUserInfoColor;
  FShowUserInfoText := AValue.ShowUserInfoText;
  {$IFDEF LINEINFOEX}
  FShowRevisionCountInfoColor := AValue.ShowRevisionCountInfoColor;
  FShowRevisionCountInfoText := AValue.ShowRevisionCountInfoText;
  FShowFirstRevisionInfoColor := AValue.ShowFirstRevisionInfoColor;
  FShowFirstRevisionInfoText := AValue.ShowFirstRevisionInfoText;
  {$ENDIF LINEINFOEX}
  {$IFDEF DELPHI6_UP}
  FShowOrderList.Assign(AValue.ShowOrderList);
  {$ELSE}
  AssignTList(FShowOrderList, AValue.ShowOrderList);
  {$ENDIF DELPHI6_UP}
  FStaticUserColorList.Assign(AValue.StaticUserColorList);
  FSuppressRevisionTextZeroDot := AValue.SuppressRevisionTextZeroDot;
  FUserSettingsList.Assign(AValue.UserSettingsList);
end;

{ TJVCSLineHistoryPresets }

constructor TJVCSLineHistoryPresets.Create(AKey: string = '');
begin
  inherited Create;
  FItems := TObjectList<TJVCSLineHistoryPreset>.Create;
  FModificationTimestamp := 0;
  FSelectedID := -1;
  if AKey = '' then
    FKey := 'Software\VersionInsight\Blame'
  else
    FKey := AKey;
end;

procedure TJVCSLineHistoryPresets.Delete(AIndex: Integer);
begin
  FItems.Delete(AIndex);
end;

destructor TJVCSLineHistoryPresets.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

function TJVCSLineHistoryPresets.Add(const AName: string): TJVCSLineHistoryPreset;
begin
  FItems.Add(TJVCSLineHistoryPreset.Create);
  Result := FItems.Last;
  Result.Name := AName;
  Result.ID := GetNextID;
end;

procedure TJVCSLineHistoryPresets.AddDefaultPresets;
var
  Settings: TJVCSLineHistorySettings;
begin
  Settings := Add('Preset 1').Settings;
  Settings.PaintMethod := 1;
  Settings.DateFormat := Age2DateFormat;
  Settings.DateStartColor := $F4F4F4;
  Settings.DateEndColor := clRed;
  Settings.RevisionStartColor := $F4F4F4;
  Settings.RevisionEndColor := clAqua;
  Settings.ShowRevisionInfoText := False;
  Settings.ShowRevisionInfoColor := True;
  Settings.ShowDateInfoText := False;
  Settings.ShowDateInfoColor := False;
  Settings.ShowUserInfoText := False;
  Settings.ShowUserInfoColor := False;

  Settings := Add('Preset 2').Settings;
  Settings.PaintMethod := 1;
  Settings.DateFormat := Age2DateFormat;
  Settings.DateStartColor := $F4F4F4;
  Settings.DateEndColor := clRed;
  Settings.RevisionStartColor := $F4F4F4;
  Settings.RevisionEndColor := clAqua;
  Settings.ShowRevisionInfoText := True;
  Settings.ShowRevisionInfoColor := True;
  Settings.ShowDateInfoText := False;
  Settings.ShowDateInfoColor := False;
  Settings.ShowUserInfoText := False;
  Settings.ShowUserInfoColor := False;

  Settings := Add('Preset 3').Settings;
  Settings.PaintMethod := 1;
  Settings.DateFormat := Age2DateFormat;
  Settings.DateStartColor := $F4F4F4;
  Settings.DateEndColor := clRed;
  Settings.RevisionStartColor := $F4F4F4;
  Settings.RevisionEndColor := clAqua;
  Settings.ShowRevisionInfoText := True;
  Settings.ShowRevisionInfoColor := True;
  Settings.ShowDateInfoText := True;
  Settings.ShowDateInfoColor := True;
  Settings.ShowUserInfoText := True;
  Settings.ShowUserInfoColor := True;
end;

procedure TJVCSLineHistoryPresets.Clear;
begin
  FItems.Clear;
end;

function TJVCSLineHistoryPresets.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TJVCSLineHistoryPresets.GetItems(AIndex: Integer): TJVCSLineHistoryPreset;
begin
  Result := FItems[AIndex];
end;

function TJVCSLineHistoryPresets.IndexOf(const AName: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Count - 1 do
    if Items[I].Name = AName then
    begin
      Result := I;
      Break;
    end;
end;

function TJVCSLineHistoryPresets.IndexOfID(AID: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Count - 1 do
    if Items[I].ID = AID then
    begin
      Result := I;
      Break;
    end;
end;

procedure TJVCSLineHistoryPresets.Load;
var
  RegIniFile: TRegIniFile;
  Name: string;
  I, Idx, ID, SelID, PresetCount: Integer;
  Preset: TJVCSLineHistoryPreset;
begin
  RegIniFile := TRegIniFile.Create(FKey);
  try
    PresetCount := RegIniFile.ReadInteger('Presets', 'Count', 0);
    Clear;
    if PresetCount > 0 then
    begin
      for I := 0 to PresetCount - 1 do
      begin
        ID := RegIniFile.ReadInteger('Presets', Format('ID%d', [I]), -1);
        Name := RegIniFile.ReadString('Presets', Format('Name%d', [I]), '');
        if Name <> '' then
        begin
          Preset := Add(Name);
          Preset.ID := ID;
          LoadSettings(Format('Preset%d', [I]), Preset.Settings);
        end;
      end;
      for I := 0 to Count - 1 do
        if Items[I].ID = -1 then
          Items[I].ID := GetNextID;
      SelID := RegIniFile.ReadInteger('Presets', 'SelectedID', FSelectedID);
      Idx := IndexOfID(SelID);
      if Idx <> -1 then
        FSelectedID := SelID
      else
        FSelectedID := -1;
    end
    else
    begin
      AddDefaultPresets;
      if Count > 0 then
        FSelectedID := Items[0].ID
      else
        FSelectedID := -1;
    end;
  finally
    RegIniFile.Free;
  end;
end;

procedure TJVCSLineHistoryPresets.LoadSettings(AKeyName: string; ASettings: TJVCSLineHistorySettings);
var
  RegIniFile: TRegIniFile;
  I, Cnt, Int: Integer;
  S, S2: string;
  UserSettingsItem: TJVCSLineHistoryUserSettingsItem;
begin
  RegIniFile := TRegIniFile.Create(FKey);
  try
    //Clear;
    ASettings.DateEndColor := RegIniFile.ReadInteger(AKeyName, 'DateEndColor', ASettings.DateEndColor);
    ASettings.DateFormat := RegIniFile.ReadString(AKeyName, 'DateFormat', ASettings.DateFormat);
    ASettings.DateStartColor := RegIniFile.ReadInteger(AKeyName, 'DateStartColor', ASettings.DateStartColor);
    ASettings.PaintMethod := RegIniFile.ReadInteger(AKeyName, 'PaintMethod', ASettings.PaintMethod);
    ASettings.RevisionEndColor := RegIniFile.ReadInteger(AKeyName, 'RevisionEndColor', ASettings.RevisionEndColor);
    ASettings.RevisionStartColor := RegIniFile.ReadInteger(AKeyName, 'RevisionStartColor', ASettings.RevisionStartColor);
    ASettings.ShowRevisionInfoColor := RegIniFile.ReadBool(AKeyName, 'ShowRevisionInfoColor', ASettings.ShowRevisionInfoColor);
    ASettings.ShowRevisionInfoText := RegIniFile.ReadBool(AKeyName, 'ShowRevisionInfoText', ASettings.ShowRevisionInfoText);
    ASettings.ShowDateInfoColor := RegIniFile.ReadBool(AKeyName, 'ShowDateInfoColor', ASettings.ShowDateInfoColor);
    ASettings.ShowDateInfoText := RegIniFile.ReadBool(AKeyName, 'ShowDateInfoText', ASettings.ShowDateInfoText);
    ASettings.ShowUserInfoColor := RegIniFile.ReadBool(AKeyName, 'ShowUserInfoColor', ASettings.ShowUserInfoColor);
    ASettings.ShowUserInfoText := RegIniFile.ReadBool(AKeyName, 'ShowUserInfoText', ASettings.ShowUserInfoText);
    Cnt := RegIniFile.ReadInteger(AKeyName, 'UserSettingsListCount', -1);
    if Cnt >= 0 then
    begin
      ASettings.UserSettingsList.Clear;
      for I := 0 to Pred(Cnt) do
      begin
        Int := RegIniFile.ReadInteger(AKeyName, Format('UserSettingsListColor%d', [I]), -1);
        S := RegIniFile.ReadString(AKeyName, Format('UserSettingsListUserName%d', [I]), '');
        S2 := RegIniFile.ReadString(AKeyName, Format('UserSettingsListVisibleName%d', [I]), '');
        if (S <> '') and (ASettings.UserSettingsList.IndexOfUser(S) = -1) and ((S2 <> '') or (Int <> -1)) then
        begin
          UserSettingsItem := ASettings.UserSettingsList.Add;
          UserSettingsItem.UserName := S;
          UserSettingsItem.Color := TColor(Int);
          UserSettingsItem.VisibleName := S2;
        end;
      end;
    end;
  finally
    RegIniFile.Free;
  end;
end;

function TJVCSLineHistoryPresets.GetNextID: Integer;
var
  I: Integer;
begin
  Result := 1;
  for I := 0 to Count - 1 do
    if Items[I].ID >= Result then
      Result := Items[I].ID + 1;
end;

procedure TJVCSLineHistoryPresets.Save;
var
  RegIniFile: TRegIniFile;
  I: Integer;
begin
  RegIniFile := TRegIniFile.Create(FKey);
  try
    RegIniFile.WriteInteger('Presets', 'Count', Count);
    for I := 0 to Count - 1 do
    begin
      RegIniFile.WriteInteger('Presets', Format('ID%d', [I]), Items[I].ID);
      RegIniFile.WriteString('Presets', Format('Name%d', [I]), Items[I].Name);
      SaveSettings(Format('Preset%d', [I]), Items[I].Settings);
    end;
  finally
    RegIniFile.Free;
  end;
  SetModified;
end;

procedure TJVCSLineHistoryPresets.SaveSettings(AKeyName: string; ASettings: TJVCSLineHistorySettings);
var
  RegIniFile: TRegIniFile;
  I: Integer;
  UserSettingsItem: TJVCSLineHistoryUserSettingsItem;
begin
  RegIniFile := TRegIniFile.Create(FKey);
  try
    RegIniFile.WriteInteger(AKeyName, 'DateEndColor', ASettings.DateEndColor);
    RegIniFile.WriteString(AKeyName, 'DateFormat', ASettings.DateFormat);
    RegIniFile.WriteInteger(AKeyName, 'DateStartColor', ASettings.DateStartColor);
    RegIniFile.WriteInteger(AKeyName, 'PaintMethod', ASettings.PaintMethod);
    RegIniFile.WriteInteger(AKeyName, 'RevisionEndColor', ASettings.RevisionEndColor);
    RegIniFile.WriteInteger(AKeyName, 'RevisionStartColor', ASettings.RevisionStartColor);
    RegIniFile.WriteBool(AKeyName, 'ShowRevisionInfoColor', ASettings.ShowRevisionInfoColor);
    RegIniFile.WriteBool(AKeyName, 'ShowRevisionInfoText', ASettings.ShowRevisionInfoText);
    RegIniFile.WriteBool(AKeyName, 'ShowDateInfoColor', ASettings.ShowDateInfoColor);
    RegIniFile.WriteBool(AKeyName, 'ShowDateInfoText', ASettings.ShowDateInfoText);
    RegIniFile.WriteBool(AKeyName, 'ShowUserInfoColor', ASettings.ShowUserInfoColor);
    RegIniFile.WriteBool(AKeyName, 'ShowUserInfoText', ASettings.ShowUserInfoText);
    RegIniFile.WriteInteger(AKeyName, 'UserSettingsListCount', ASettings.UserSettingsList.Count);
    for I := 0 to Pred(ASettings.UserSettingsList.Count) do
    begin
      UserSettingsItem := ASettings.UserSettingsList[I];
      RegIniFile.WriteInteger(AKeyName, Format('UserSettingsListColor%d', [I]), Integer(UserSettingsItem.Color));
      RegIniFile.WriteString(AKeyName, Format('UserSettingsListUserName%d', [I]), UserSettingsItem.UserName);
      RegIniFile.WriteString(AKeyName, Format('UserSettingsListVisibleName%d', [I]), UserSettingsItem.VisibleName);
    end;
  finally
    RegIniFile.Free;
  end;
end;

procedure TJVCSLineHistoryPresets.SetModified;
begin
  FModificationTimestamp := Now;
end;

procedure TJVCSLineHistoryPresets.SetSelectedID(const Value: Integer);
var
  RegIniFile: TRegIniFile;
begin
  if FSelectedID <> Value then
  begin
    FSelectedID := Value;
    RegIniFile := TRegIniFile.Create(FKey);
    try
      RegIniFile.WriteInteger('Presets', 'SelectedID', FSelectedID);
    finally
      RegIniFile.Free;
    end;
  end;
end;

{ TJVCSLineHistoryPreset }

constructor TJVCSLineHistoryPreset.Create;
begin
  inherited Create;
  FID := -1;
  FSettings := TJVCSLineHistorySettings.Create;
end;

destructor TJVCSLineHistoryPreset.Destroy;
begin
  FSettings.Free;
  inherited Destroy;
end;

var
  DefaultPresets: TJVCSLineHistoryPresets = nil;
  DefaultPresetsKey: string = '';

function GetPresets: TJVCSLineHistoryPresets;
begin
  if not Assigned(DefaultPresets) then
  begin
    DefaultPresets := TJVCSLineHistoryPresets.Create(DefaultPresetsKey);
    DefaultPresets.Load;
  end;
  Result := DefaultPresets;
end;

procedure SetPresetsKey(const AKey: string);
begin
  DefaultPresetsKey := AKey;
end;

initialization

finalization
  DefaultPresets.Free;

end.
