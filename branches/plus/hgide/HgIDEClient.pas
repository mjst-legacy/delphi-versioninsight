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
{ The Original Code is HgIDEClient.pas.                                        }
{                                                                              }
{ The Initial Developer of the Original Code is Uwe Schuster.                  }
{ Portions created by Uwe Schuster are Copyright © 2010 Uwe Schuster. All      }
{ Rights Reserved.                                                             }
{                                                                              }
{ Contributors:                                                                }
{ Uwe Schuster (uschuster)                                                     }
{                                                                              }
{******************************************************************************}

unit HgIDEClient;

interface

uses
  SysUtils, Classes, HgClient, HgIDEColors;

type
  THgOptions = class(TObject)
  private
    FDeleteBackupFilesAfterCommit: Boolean;
  public
    constructor Create;
    procedure Load;
    procedure Save;
    property DeleteBackupFilesAfterCommit: Boolean read FDeleteBackupFilesAfterCommit write FDeleteBackupFilesAfterCommit;
  end;

  THgIDEClient = class(TObject)
  private
    FHistoryProviderIndex: Integer;
    FColors: THgColors;
    FHgClient: THgClient;
    FHgInitialized: Boolean;
    FOptions: THgOptions;
    procedure Initialize;
    procedure Finalize;
    function GetHgClient: THgClient;
  public
    constructor Create;
    destructor Destroy; override;
    property Colors: THgColors read FColors;
    function HgInitialize: Boolean;
    property HgClient: THgClient read GetHgClient;
    property Options: THgOptions read FOptions;
  end;

procedure Register;

function BaseRegKey: string;
procedure LoadSourceRepoHistory(const List: TStringList);
procedure SaveSourceRepoHistory(const List: TStringList);

var
  IDEClient: THgIDEClient;

implementation

uses
  Registry, ToolsAPI, FileHistoryAPI, HgIDEHistory, HgIDEAddInOptions, HgIDEMenus,
  HgImages, HgIDEConst, HgIDEIcons, HgIDEFileStates;

const
  sSourceRepoHistory = 'SourceRepoHistory';
  sSourceRepoHistoryItem = 'SourceRepoHistory%d';
  MaxSourceRepoHistory = 20;
  cOptions = 'Options';
  cDeleteBackupFilesAfterCommit = 'DeleteBackupFilesAfterCommit';

procedure Register;
begin
  IDEClient := THgIDEClient.Create;
  RegisterMenus(IDEClient);
  RegisterAddInOptions;
  RegisterImages;
  RegisterFileStateProvider;
end;

{ THgOptions }

constructor THgOptions.Create;
begin
  inherited Create;
  FDeleteBackupFilesAfterCommit := False;
  Load;
end;

procedure THgOptions.Load;
var
  Reg: TRegistry;
  BaseKey, Key: string;
begin
  Reg := TRegistry.Create;
  try
    BaseKey := BaseRegKey + cOptions;
    if not Reg.KeyExists(BaseKey) then
      Exit;
    Reg.OpenKeyReadOnly(BaseKey);
    Key := cDeleteBackupFilesAfterCommit;
    if Reg.ValueExists(Key) then
      FDeleteBackupFilesAfterCommit := Reg.ReadBool(Key);
  finally
    Reg.Free;
  end;
end;

procedure THgOptions.Save;
var
  Reg: TRegistry;
  BaseKey: string;
begin
  Reg := TRegistry.Create;
  try
    BaseKey := BaseRegKey + cOptions;
    Reg.OpenKey(BaseKey, True);
    Reg.WriteBool(cDeleteBackupFilesAfterCommit, FDeleteBackupFilesAfterCommit);
  finally
    Reg.Free;
  end;
end;

{ THgIDEClient }

constructor THgIDEClient.Create;
begin
  inherited Create;
  FColors := THgColors.Create;
  FOptions := THgOptions.Create;
  Initialize;
end;

destructor THgIDEClient.Destroy;
begin
  Finalize;
  FOptions.Free;
  FColors.Free;
  inherited Destroy;
end;

procedure THgIDEClient.Finalize;
var
  FileHistoryManager: IOTAFileHistoryManager;
begin
  if (FHistoryProviderIndex <> -1) and Assigned(BorlandIDEServices)
    and BorlandIDEServices.GetService(IOTAFileHistoryManager, FileHistoryManager)
  then
    FileHistoryManager.UnregisterHistoryProvider(FHistoryProviderIndex);
  FHistoryProviderIndex := -1;
  FreeAndNil(HgImageModule);
end;

function THgIDEClient.GetHgClient: THgClient;
begin
  if not FHgInitialized then
    HgInitialize;
  Result := FHgClient;
end;

function THgIDEClient.HgInitialize: Boolean;
var
  RegIniFile: TRegIniFile;
  Key: string;
begin
  Result := True;
  if FHgInitialized then
    Exit;
  FHgInitialized := True;

  HgImageModule := THgImageModule.Create(nil);

  FHgClient := THgClient.Create;

  Key := (BorlandIDEServices as IOTAServices).GetBaseRegistryKey + '\VersionInsight';
  RegIniFile := TRegIniFile.Create(Key);
  try
    FHgClient.HgExecutable := RegIniFile.ReadString('Mercurial', 'Executable', '');
  finally
    RegIniFile.Free;
  end;
end;

procedure THgIDEClient.Initialize;
var
  FileHistoryManager: IOTAFileHistoryManager;
begin
  if Assigned(BorlandIDEServices) then
  begin
    if BorlandIDEServices.GetService(IOTAFileHistoryManager, FileHistoryManager) then
      FHistoryProviderIndex := FileHistoryManager.RegisterHistoryProvider(THgFileHistoryProvider.Create(Self));
  end;
end;

function BaseRegKey: string;
begin
  Result := (BorlandIDEServices as IOTAServices).GetBaseRegistryKey + '\' + sMercurial + '\';
end;

procedure LoadSourceRepoHistory(const List: TStringList);
var
  Reg: TRegistry;
  BaseKey: string;
  Key: string;
  S: string;
  I: Integer;
begin
  Reg := TRegistry.Create;
  try
    BaseKey := BaseRegKey + sSourceRepoHistory;
    if not Reg.KeyExists(BaseKey) then
      Exit;
    Reg.OpenKeyReadOnly(BaseKey);
    for I := 0 to MaxSourceRepoHistory - 1 do
    begin
      Key := Format(sSourceRepoHistoryItem, [I]);
      S := Reg.ReadString(Key);
      if S = '' then
        Break
      else
        List.Add(S);
    end;
  finally
    Reg.Free;
  end;
end;

procedure SaveSourceRepoHistory(const List: TStringList);
var
  Reg: TRegistry;
  BaseKey: string;
  Key: string;
  I: Integer;
  Count: Integer;
begin
  Reg := TRegistry.Create;
  try
    BaseKey := BaseRegKey + sSourceRepoHistory;
    Reg.OpenKey(BaseKey, True);
    if List.Count > MaxSourceRepoHistory then
      Count := MaxSourceRepoHistory
    else
      Count := List.Count;
    for I := 0 to Count - 1 do
    begin
      Key := Format(sSourceRepoHistoryItem, [I]);
      Reg.WriteString(Key, List[I]);
    end;
  finally
    Reg.Free;
  end;
end;

initialization

finalization
  UnregisterFileStateProvider;

end.
