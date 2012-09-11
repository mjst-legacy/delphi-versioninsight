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
{ The Original Code is delphisvn: Subversion plugin for CodeGear Delphi.       }
{                                                                              }
{ The Initial Developer of the Original Code is Ondrej Kelle.                  }
{ Portions created by Ondrej Kelle are Copyright Ondrej Kelle. All rights      }
{ reserved.                                                                    }
{                                                                              }
{ Portions created or modified by Embarcadero Technologies are                 }
{ Copyright © 2010 Embarcadero Technologies, Inc. All Rights Reserved          }
{ Modifications include a major re-write of delphisvn. New functionality for   }
{ diffing, international character support, asynchronous gathering of data,    }
{ check-out and import, usability, tighter integration into RAD Studio, and    }
{ other new features.  Most original source files not used or re-written.      }
{                                                                              }
{ Contributors:                                                                }
{ Ondrej Kelle (tondrej)                                                       }
{ Uwe Schuster (uschuster)                                                     }
{ Embarcadero Technologies                                                     }
{                                                                              }
{******************************************************************************}
{                                                                              }
{ This unit contains TSvnIDEClient, a utility data module class for Delphi IDE }
{ Subversion plugin (to be used as a single global instance).                  }
{                                                                              }
{******************************************************************************}

unit SvnIDEClient;

interface

uses SvnClient, svn_client, Classes, SysUtils, SvnIDEColors;

type
  TSvnOptions = class(TObject)
  private
    FAlternativeCommitLayout: Boolean;
    FBlameOptions: TSvnBlameOptions;
    FClearFileStatesAfterCloseAll: Boolean;
    FDeleteBackupFilesAfterCommit: Boolean;
    FKeepCommitViewOpenAfterCommit: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Load;
    procedure Save;
    property AlternativeCommitLayout: Boolean read FAlternativeCommitLayout write FAlternativeCommitLayout;
    property BlameOptions: TSvnBlameOptions read FBlameOptions;
    property ClearFileStatesAfterCloseAll: Boolean read FClearFileStatesAfterCloseAll write FClearFileStatesAfterCloseAll;
    property DeleteBackupFilesAfterCommit: Boolean read FDeleteBackupFilesAfterCommit write FDeleteBackupFilesAfterCommit;
    property KeepCommitViewOpenAfterCommit: Boolean read FKeepCommitViewOpenAfterCommit write FKeepCommitViewOpenAfterCommit;
  end;

  TSvnIDEClient = class
  private
    FHistoryProviderIndex: Integer;
    FColors: TSvnColors;
    FOptions: TSvnOptions;
    FSvnClient: TSvnClient;
    FSyncData: Pointer;
    FSvnInitialized: Boolean;

    function GetSvnClient: TSvnClient;

    procedure SvnClientLoginPrompt(Sender: TObject; const Realm: string; var UserName, Password: string;
      var Cancel, Save: Boolean);
    procedure SvnClientSSLClientCertPrompt(Sender: TObject; const Realm: string; var CertFileName: string;
      var Cancel, Save: Boolean);
    procedure SvnClientSSLClientPasswordPrompt(Sender: TObject; const Realm: string; var Password: string;
      var Cancel, Save: Boolean);
    procedure SvnClientSSLServerTrustPrompt(Sender: TObject; const Realm: string;
      const CertInfo: TSvnAuthSSLServerCertInfo; Failures: TSSLServerTrustFailures; var Cancel, Save: Boolean);
    procedure SvnClientUserNamePrompt(Sender: TObject; const Realm: string; var UserName: string;
      var Cancel, Save: Boolean);

    procedure SyncLoginPrompt;
    procedure SyncSSLClientCertPrompt;
    procedure SyncSSLClientPasswordPrompt;
    procedure SyncSSLServerTrustPrompt;
    procedure SyncUserNamePrompt;

    procedure Initialize;
    procedure Finalize;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SettingsModified;
    property Colors: TSvnColors read FColors;
    property Options: TSvnOptions read FOptions;
    function SvnInitialize: Boolean;
    property SvnClient: TSvnClient read GetSvnClient;
  end;

procedure Register;

function BaseRegKey: string;
procedure LoadURLHistory(const List: TStringList);
procedure SaveURLHistory(const List: TStringList);
function HandleSvnException(ExceptObject: TObject): Boolean;
function GetSvnExceptionMessage(ExceptObject: TObject; var Message: string): Boolean;
procedure ShowSvnExceptionMessage(const Message: string);

var
  IDEClient: TSvnIDEClient;

implementation

uses ToolsApi, FileHistoryApi, Controls, Forms, Dialogs,
  SvnClientLoginPrompt, SvnClientSSLClientCertPrompt, SvnIDEMenus,
  SvnClientSSLServerTrustPrompt, SvnIDEHistory, SvnImages, SvnIDEConst,
  Registry, SvnIDENotifier, SvnIDEClean, SvnIDEAddInOptions, SvnIDEIcons,
  SvnIDEFileStates;

const
 sURLHistory = 'URLHistory';
 sUrlHistoryItem = 'URLHistory%d';
 MaxURLHistory = 20;
 cOptions = 'Options';
 cAlternativeCommitLayout = 'AlternativeCommitLayout';
 cClearFileStatesAfterCloseAll = 'ClearFileStatesAfterCloseAll';
 cDeleteBackupFilesAfterCommit = 'DeleteBackupFilesAfterCommit';
 cKeepCommitViewOpenAfterCommit = 'KeepCommitViewOpenAfterCommit';
 cBlameIgnoreEOL = 'BlameIgnoreEOL';
 cBlameCompareSpaces = 'BlameCompareSpaces';

type
  PSyncLoginPrompt = ^TSyncLoginPrompt;
  TSyncLoginPrompt = record
    SvnClient: TSvnClient;
    Realm: string;
    UserName: string;
    Password: string;
    Cancel: Boolean;
    Save: Boolean;
  end;

  PSyncSSLClientCertPrompt = ^TSyncSSLClientCertPrompt;
  TSyncSSLClientCertPrompt = record
    SvnClient: TSvnClient;
    Realm: string;
    CertFileName: string;
    Cancel: Boolean;
    Save: Boolean;
  end;

  PSyncSSLClientPasswordPrompt = ^TSyncSSLClientPasswordPrompt;
  TSyncSSLClientPasswordPrompt = record
    SvnClient: TSvnClient;
    Realm: string;
    Password: string;
    Cancel: Boolean;
    Save: Boolean;
  end;

  PSyncSSLServerTrustPrompt = ^TSyncSSLServerTrustPrompt;
  TSyncSSLServerTrustPrompt = record
    SvnClient: TSvnClient;
    Realm: string;
    CertInfo: TSvnAuthSSLServerCertInfo;
    Failures: TSSLServerTrustFailures;
    Cancel: Boolean;
    Save: Boolean;
  end;

procedure Register;
begin
  IDEClient := TSvnIDEClient.Create;
  RegisterMenus(IDEClient);
  RegisterFileNotification;
  RegisterAddInOptions;
  RegisterImages;
  RegisterFileStateProvider;
end;

{ TSvnOptions }

constructor TSvnOptions.Create;
begin
  inherited Create;
  FAlternativeCommitLayout := False;
  FBlameOptions := TSvnBlameOptions.Create;
  FClearFileStatesAfterCloseAll := False;
  FDeleteBackupFilesAfterCommit := False;
  FKeepCommitViewOpenAfterCommit := False;
  Load;
end;

destructor TSvnOptions.Destroy;
begin
  FBlameOptions.Free;
  inherited Destroy;
end;

procedure TSvnOptions.Load;
var
  Reg: TRegistry;
  BaseKey, Key: string;
  IntValue: Integer;
begin
  Reg := TRegistry.Create;
  try
    BaseKey := BaseRegKey + cOptions;
    if not Reg.KeyExists(BaseKey) then
      Exit;
    Reg.OpenKeyReadOnly(BaseKey);
    Key := cAlternativeCommitLayout;
    if Reg.ValueExists(Key) then
      FAlternativeCommitLayout := Reg.ReadBool(Key);
    Key := cClearFileStatesAfterCloseAll;
    if Reg.ValueExists(Key) then
      FClearFileStatesAfterCloseAll := Reg.ReadBool(Key);
    Key := cDeleteBackupFilesAfterCommit;
    if Reg.ValueExists(Key) then
      FDeleteBackupFilesAfterCommit := Reg.ReadBool(Key);
    Key := cKeepCommitViewOpenAfterCommit;
    if Reg.ValueExists(Key) then
      FKeepCommitViewOpenAfterCommit := Reg.ReadBool(Key);

    Key := cBlameIgnoreEOL;
    if Reg.ValueExists(Key) then
      FBlameOptions.IgnoreEOL := Reg.ReadBool(Key);
    Key := cBlameCompareSpaces;
    if Reg.ValueExists(Key) then
    begin
      IntValue := Reg.ReadInteger(Key);
      FBlameOptions.IgnoreSpace := IntValue = 1;
      FBlameOptions.IgnoreSpaceAll := IntValue = 2;
    end;
  finally
    Reg.Free;
  end;
end;

procedure TSvnOptions.Save;
var
  Reg: TRegistry;
  BaseKey: string;
  IntValue: Integer;
begin
  Reg := TRegistry.Create;
  try
    BaseKey := BaseRegKey + cOptions;
    Reg.OpenKey(BaseKey, True);
    Reg.WriteBool(cAlternativeCommitLayout, FAlternativeCommitLayout);
    Reg.WriteBool(cClearFileStatesAfterCloseAll, FClearFileStatesAfterCloseAll);
    Reg.WriteBool(cDeleteBackupFilesAfterCommit, FDeleteBackupFilesAfterCommit);
    Reg.WriteBool(cKeepCommitViewOpenAfterCommit, FKeepCommitViewOpenAfterCommit);

    Reg.WriteBool(cBlameIgnoreEOL, FBlameOptions.IgnoreEOL);
    if FBlameOptions.IgnoreSpaceAll then
      IntValue := 2
    else
    if FBlameOptions.IgnoreSpace then
      IntValue := 1
    else
      IntValue := 0;
    Reg.WriteInteger(cBlameCompareSpaces, IntValue);
  finally
    Reg.Free;
  end;
end;

{ TSvnIDEClient }

constructor TSvnIDEClient.Create;
begin
  inherited;
  FColors := TSvnColors.Create;
  FOptions := TSvnOptions.Create;
  Initialize;
end;

destructor TSvnIDEClient.Destroy;
begin
  Unloading := True;
  Finalize;
  FOptions.Free;
  FColors.Free;
  inherited;
end;

procedure TSvnIDEClient.Finalize;
var
  FileHistoryManager: IOTAFileHistoryManager;
begin
  if (FHistoryProviderIndex <> -1) and Assigned(BorlandIDEServices)
    and BorlandIDEServices.GetService(IOTAFileHistoryManager, FileHistoryManager)
  then
    FileHistoryManager.UnregisterHistoryProvider(FHistoryProviderIndex);
  FHistoryProviderIndex := -1;

  FreeAndNil(SvnImageModule);
  FreeAndNil(FSvnClient);
end;

function TSvnIDEClient.GetSvnClient: TSvnClient;
begin
  if not FSvnInitialized then
    SvnInitialize;
  Result := FSvnClient;
end;

procedure TSvnIDEClient.Initialize;
var
  FileHistoryManager: IOTAFileHistoryManager;
  Services: IOTAServices;
  Reg: TRegIniFile;
  S: string;
begin
  if Assigned(BorlandIDEServices) then
  begin
    if BorlandIDEServices.GetService(IOTAFileHistoryManager, FileHistoryManager) then
      FHistoryProviderIndex := FileHistoryManager.RegisterHistoryProvider(TSvnFileHistoryProvider.Create(Self));
    if BorlandIDEServices.GetService(IOTAServices, Services) then
    begin
      Reg := TRegIniFile.Create(Services.GetBaseRegistryKey);
      try
        S := IncludeTrailingPathDelimiter(Services.GetRootDirectory) + 'bin\subversion';
        BaseDllDir := IncludeTrailingPathDelimiter(Reg.ReadString('Subversion', 'SvnDllDir', S));
      finally
        Reg.Free;
      end;
    end;
  end;
end;

procedure TSvnIDEClient.SettingsModified;
begin
  if FSvnInitialized then
    SvnClient.BlameOptions.Assign(FOptions.BlameOptions);
end;

procedure TSvnIDEClient.SvnClientLoginPrompt(Sender: TObject; const Realm: string; var UserName, Password: string;
  var Cancel, Save: Boolean);
begin
  FSyncData := AllocMem(SizeOf(TSyncLoginPrompt));
  try
    PSyncLoginPrompt(FSyncData)^.SvnClient := Sender as TSvnClient;
    PSyncLoginPrompt(FSyncData)^.Realm := Realm;
    PSyncLoginPrompt(FSyncData)^.UserName := UserName;
    PSyncLoginPrompt(FSyncData)^.Password := Password;
    PSyncLoginPrompt(FSyncData)^.Cancel := Cancel;
    PSyncLoginPrompt(FSyncData)^.Save := Save;
    TThread.Synchronize(nil, SyncLoginPrompt);
    UserName := PSyncLoginPrompt(FSyncData)^.UserName;
    Password := PSyncLoginPrompt(FSyncData)^.Password;
    Cancel := PSyncLoginPrompt(FSyncData)^.Cancel;
    Save := PSyncLoginPrompt(FSyncData)^.Save;
  finally
    System.Finalize(TSyncLoginPrompt(FSyncData^));
    FreeMem(FSyncData);
    FSyncData := nil;
  end;
end;

procedure TSvnIDEClient.SvnClientSSLClientCertPrompt(Sender: TObject; const Realm: string; var CertFileName: string;
  var Cancel, Save: Boolean);
begin
  FSyncData := AllocMem(SizeOf(TSyncSSLClientCertPrompt));
  try
    PSyncSSLClientCertPrompt(FSyncData)^.SvnClient := Sender as TSvnClient;
    PSyncSSLClientCertPrompt(FSyncData)^.Realm := Realm;
    PSyncSSLClientCertPrompt(FSyncData)^.CertFileName := CertFileName;
    PSyncSSLClientCertPrompt(FSyncData)^.Cancel := Cancel;
    PSyncSSLClientCertPrompt(FSyncData)^.Save := Save;
    TThread.Synchronize(nil, SyncSSLClientCertPrompt);
    CertFileName := PSyncSSLClientCertPrompt(FSyncData)^.CertFileName;
    Cancel := PSyncSSLClientCertPrompt(FSyncData)^.Cancel;
    Save := PSyncSSLClientCertPrompt(FSyncData)^.Save;
  finally
    System.Finalize(TSyncSSLClientCertPrompt(FSyncData^));
    FreeMem(FSyncData);
    FSyncData := nil;
  end;
end;

procedure TSvnIDEClient.SvnClientSSLClientPasswordPrompt(Sender: TObject; const Realm: string; var Password: string;
  var Cancel, Save: Boolean);
begin
  FSyncData := AllocMem(SizeOf(TSyncSSLClientPasswordPrompt));
  try
    PSyncSSLClientPasswordPrompt(FSyncData)^.SvnClient := Sender as TSvnClient;
    PSyncSSLClientPasswordPrompt(FSyncData)^.Realm := Realm;
    PSyncSSLClientPasswordPrompt(FSyncData)^.Password := Password;
    PSyncSSLClientPasswordPrompt(FSyncData)^.Cancel := Cancel;
    PSyncSSLClientPasswordPrompt(FSyncData)^.Save := Save;
    TThread.Synchronize(nil, SyncSSLClientPasswordPrompt);
    Password := PSyncSSLClientPasswordPrompt(FSyncData)^.Password;
    Cancel := PSyncSSLClientPasswordPrompt(FSyncData)^.Cancel;
    Save := PSyncSSLClientPasswordPrompt(FSyncData)^.Save;
  finally
    System.Finalize(TSyncSSLClientPasswordPrompt(FSyncData^));
    FreeMem(FSyncData);
    FSyncData := nil;
  end;
end;

procedure TSvnIDEClient.SvnClientSSLServerTrustPrompt(Sender: TObject; const Realm: string;
  const CertInfo: TSvnAuthSSLServerCertInfo; Failures: TSSLServerTrustFailures; var Cancel, Save: Boolean);
begin
  FSyncData := AllocMem(SizeOf(TSyncSSLServerTrustPrompt));
  try
    PSyncSSLServerTrustPrompt(FSyncData)^.SvnClient := Sender as TSvnClient;
    PSyncSSLServerTrustPrompt(FSyncData)^.Realm := Realm;
    PSyncSSLServerTrustPrompt(FSyncData)^.CertInfo := CertInfo;
    PSyncSSLServerTrustPrompt(FSyncData)^.Failures := Failures;
    PSyncSSLServerTrustPrompt(FSyncData)^.Cancel := Cancel;
    PSyncSSLServerTrustPrompt(FSyncData)^.Save := Save;
    TThread.Synchronize(nil, SyncSSLServerTrustPrompt);
    Cancel := PSyncSSLServerTrustPrompt(FSyncData)^.Cancel;
    Save := PSyncSSLServerTrustPrompt(FSyncData)^.Save;
  finally
    System.Finalize(TSyncSSLServerTrustPrompt(FSyncData^));
    FreeMem(FSyncData);
    FSyncData := nil;
  end;
end;

procedure TSvnIDEClient.SvnClientUserNamePrompt(Sender: TObject; const Realm: string; var UserName: string;
  var Cancel, Save: Boolean);
begin
  FSyncData := AllocMem(SizeOf(TSyncLoginPrompt));
  try
    PSyncLoginPrompt(FSyncData)^.SvnClient := Sender as TSvnClient;
    PSyncLoginPrompt(FSyncData)^.Realm := Realm;
    PSyncLoginPrompt(FSyncData)^.UserName := UserName;
    PSyncLoginPrompt(FSyncData)^.Password := '';
    PSyncLoginPrompt(FSyncData)^.Cancel := Cancel;
    PSyncLoginPrompt(FSyncData)^.Save := Save;
    TThread.Synchronize(nil, SyncUserNamePrompt);
    UserName := PSyncLoginPrompt(FSyncData)^.UserName;
    Cancel := PSyncLoginPrompt(FSyncData)^.Cancel;
    Save := PSyncLoginPrompt(FSyncData)^.Save;
  finally
    System.Finalize(TSyncLoginPrompt(FSyncData^));
    FreeMem(FSyncData);
    FSyncData := nil;
  end;
end;

function TSvnIDEClient.SvnInitialize: Boolean;
begin
  Result := True;
  if FSvnInitialized then
    Exit;
  FSvnInitialized := True;
  SvnImageModule := TSvnImageModule.Create(nil);

  FSvnClient := TSvnClient.Create;
  FSvnClient.OnLoginPrompt := SvnClientLoginPrompt;
  FSvnClient.OnUserNamePrompt := SvnClientUserNamePrompt;
  FSvnClient.OnSSLServerTrustPrompt := SvnClientSSLServerTrustPrompt;
  FSvnClient.OnSSLClientCertPrompt := SvnClientSSLClientCertPrompt;
  FSvnClient.OnSSLClientPasswordPrompt := SvnClientSSLClientPasswordPrompt;
  FSvnClient.BlameOptions.Assign(FOptions.BlameOptions);

  FSvnClient.Initialize;
end;

procedure TSvnIDEClient.SyncLoginPrompt;
begin
  with PSyncLoginPrompt(FSyncData)^ do
    Cancel := ShowSvnClientLoginPrompt(SvnClient, Realm, UserName, Password, Save) <> mrOK;
end;

procedure TSvnIDEClient.SyncSSLClientCertPrompt;
begin
  with PSyncSSLClientCertPrompt(FSyncData)^ do
    Cancel := ShowSvnClientSSLClientCertPrompt(SvnClient, Realm, CertFileName, Save) <> mrOK;
end;

procedure TSvnIDEClient.SyncSSLClientPasswordPrompt;

begin
  with PSyncSSLClientPasswordPrompt(FSyncData)^ do
    Cancel := ShowSvnClientSSLClientPasswordPrompt(SvnClient, Realm, Password, Save) <> mrOK;
end;

procedure TSvnIDEClient.SyncSSLServerTrustPrompt;
begin
  with PSyncSSLServerTrustPrompt(FSyncData)^ do
    Cancel := ShowSvnClientSSLServerTrustPrompt(SvnClient, Realm, CertInfo, Failures, Save) <> mrOK;
end;

procedure TSvnIDEClient.SyncUserNamePrompt;
begin
  with PSyncLoginPrompt(FSyncData)^ do
    Cancel := ShowSvnClientLoginPrompt(SvnClient, Realm, UserName, Password, Save, [lpoUserName]) <> mrOK;
end;

function BaseRegKey: string;
begin
  Result := (BorlandIDEServices as IOTAServices).GetBaseRegistryKey + '\' + sSubversion + '\';
end;

procedure LoadURLHistory(const List: TStringList);
var
  Reg: TRegistry;
  BaseKey: string;
  Key: string;
  S: string;
  I: Integer;
begin
  Reg := TRegistry.Create;
  try
    BaseKey := BaseRegKey + sURLHistory;
    if not Reg.KeyExists(BaseKey) then
      Exit;
    Reg.OpenKeyReadOnly(BaseKey);
    for I := 0 to MaxURLHistory - 1 do
    begin
      Key := Format(sUrlHistoryItem, [I]);
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

procedure SaveURLHistory(const List: TStringList);
var
  Reg: TRegistry;
  BaseKey: string;
  Key: string;
  I: Integer;
  Count: Integer;
begin
  Reg := TRegistry.Create;
  try
    BaseKey := BaseRegKey + sURLHistory;
    Reg.OpenKey(BaseKey, True);
    if List.Count > MaxURLHistory then
      Count := MaxURLHistory
    else
      Count := List.Count;
    for I := 0 to Count - 1 do
    begin
      Key := Format(sUrlHistoryItem, [I]);
      Reg.WriteString(Key, List[I]);
    end;
  finally
    Reg.Free;
  end;
end;

function HandleSvnException(ExceptObject: TObject): Boolean;
var
  S: string;
begin
  Result := GetSvnExceptionMessage(ExceptObject, S);
  if Result then
    ShowSvnExceptionMessage(S);
end;

function GetSvnExceptionMessage(ExceptObject: TObject; var Message: string): Boolean;
var
  I: Integer;
begin
  Result := ExceptObject is ESvnError;
  if Result then
  begin
    Message := '';
    for I := 0 to ESvnError(ExceptObject).Count - 1 do
      Message := Message + ESvnError(ExceptObject).Messages[I] + sLineBreak;
  end
end;

procedure ShowSvnExceptionMessage(const Message: string);
begin
  MessageDlg(Message, mtError, [mbOK], 0);
end;

initialization
finalization
  UnregisterFileStateProvider;
  UnRegisterMenus;
  IDEClient.Free;

end.
