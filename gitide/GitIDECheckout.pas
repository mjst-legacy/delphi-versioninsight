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
{ The Initial Developer of the Original Code is Embarcadero Technologies.      }
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
unit GitIDECheckout;

interface

function DoCheckOutProject(var ProjectName: string; const Connection: string = ''): Boolean;

implementation

uses {SvnClient, }GitIDEClient, GitClientCheckout, {SvnIDEMessageView, }ToolsApi,
  Classes, GitClientProjectSelect, SysUtils, {SvnClientRepoBrowserDialog,}
  GitClientUpdate, Generics.Collections, {SvnIDEConst, }Graphics;

type
  TCheckoutThread = class(TThread)
  protected
    FPathName: string;
    FTargetDir: string;
    FSyncPath: string;
    FSyncAction: string;
    FSyncTextColor: TColor;
    FUpdateDialog: TGitUpdateDialog;
    FExceptionMessage: string;
    FProjectNames: TStringList;
    FProjectGroupNames: TStringList;
    FProjectName: string;
    FAborted: Boolean;
    procedure AbortCallBack;
    procedure Add(const Path, Action: string; TextColor: TColor);
    procedure SearchProjects;
    procedure SyncAdd;
    procedure SyncCompleted;
    procedure Execute; override;
    procedure CloneCallBack(Sender: TObject; const AText: string; var Cancel: Boolean);
    procedure OpenProject;
  public
    constructor Create(const PathName, TargetDir: string); reintroduce;
    destructor Destroy; override;
  end;

function DoCheckout(const PathName, TargetDir: string; const ProjectName: string): string;
begin
  Result := '';
  TCheckoutThread.Create(PathName, TargetDir);
end;

function DoCheckOutProject(var ProjectName: string; const Connection: string): Boolean;
var
  PathName: string;
  TargetDir: string;
  InitalDirectory: string;
  URLHistory: TStringList;
  Loc: Integer;
begin
  InitalDirectory := (BorlandIDEServices as IOTAServices).GetStartupDirectory;
  URLHistory := TStringList.Create;
  try
    LoadSourceRepoHistory(URLHistory);
    Loc := Pos(';', Connection);
    if Loc = 0  then
      PathName := Connection
    else
    begin
      PathName := Copy(Connection, 1, Loc - 1);
      ProjectName := Copy(Connection, Loc + 1, MaxInt);
    end;
    Result := GetCheckoutInformation(URLHistory, InitalDirectory, PathName, TargetDir);
    if Result then
      SaveSourceRepoHistory(URLHistory);
  finally
    URLHistory.Free;
  end;
  if Result then
    ProjectName := DoCheckout(PathName, TargetDir, ProjectName);
end;

{ TCheckoutThread }

procedure TCheckoutThread.AbortCallBack;
begin
  FAborted := True;
end;

procedure TCheckoutThread.Add(const Path, Action: string; TextColor: TColor);
begin
  FSyncPath := Path;
  FSyncAction := Action;
  FSyncTextColor := TextColor;
  Synchronize(nil, SyncAdd);
end;

procedure TCheckoutThread.CloneCallBack(Sender: TObject; const AText: string;
  var Cancel: Boolean);
var
  Action: string;
  P: Integer;
begin
  Action := AText;
  P := Pos(#$1B + '[K', Action);
  if P > 0 then
    Delete(Action, P, MaxInt);
  Add('', Action, clNone);
  Cancel := FAborted;
end;

constructor TCheckoutThread.Create(const PathName, TargetDir: string);
begin
  inherited Create(True);
  FPathName := PathName;
  FTargetDir := TargetDir;
  FUpdateDialog := GetUpdateDialog(PathName, AbortCallBack, nil, nil);
  FUpdateDialog.Caption := Format('Clone - %s', [PathName]);
  FUpdateDialog.Show;
  FProjectNames := TStringList.Create;
  FProjectGroupNames := TStringList.Create;
  FAborted := False;
  FreeOnTerminate := True;
  Resume;
end;

destructor TCheckoutThread.Destroy;
begin
  FProjectNames.Free;
  FProjectGroupNames.Free;
  inherited;
end;

procedure EliminateChildProjectFiles(AProjectNames: TStringList);
var
  I: Integer;
  ParentExt: string;
  ParentFileDictionary: TDictionary<string, string>;
begin
  if AProjectNames.Count > 1 then
  begin
    AProjectNames.Sorted := True;
    ParentFileDictionary := TDictionary<string, string>.Create;
    try
      //Delphi mappings
      ParentFileDictionary.Add('.dpr', '.dproj');
      ParentFileDictionary.Add('.dpk', '.dproj');
      for I := AProjectNames.Count - 1 downto 0 do
        if ParentFileDictionary.TryGetValue(AnsiLowerCase(ExtractFileExt(AProjectNames[I])), ParentExt) and
          (AProjectNames.IndexOf(ChangeFileExt(AProjectNames[I], ParentExt)) <> -1) then
          AProjectNames.Delete(I);
    finally
      ParentFileDictionary.Free;
    end;
  end;
end;

procedure TCheckoutThread.Execute;
begin
  NameThreadForDebugging('VerIns Git Clone');
  try
    FExceptionMessage := '';
    IDEClient.GitClient.Clone(FPathName, FTargetDir, CloneCallBack);
  except
    {
    if not GetSvnExceptionMessage(ExceptObject, FExceptionMessage) then
      raise;
    }
  end;
  Synchronize(nil, SyncCompleted);
end;

procedure TCheckoutThread.OpenProject;
begin
  if FProjectName = '' then
    SelectProject(FProjectName, FProjectNames, FProjectGroupNames)
  else
  begin
    FProjectName := IncludeTrailingPathDelimiter(FTargetDir) + FProjectName;
  end;
  if FProjectName <> '' then
    (BorlandIDEServices as IOTAModuleServices).OpenModule(FProjectName);
end;

procedure TCheckoutThread.SearchProjects;

  procedure FindProjects(const APath: string);
  var
    F: TSearchRec;
    R: Integer;
    FileName: string;
  begin
    R := FindFirst(IncludeTrailingPathDelimiter(APath) + '*.*', faAnyFile, F);
    try
      while R = 0 do
      begin
        if (F.Name <> '.') and (F.Name <> '..') then
        begin
          if ((F.Attr and faDirectory) <> 0) and not SameText(F.Name, '.git') then
            FindProjects(IncludeTrailingPathDelimiter(APath) + F.Name)
          else
          begin
            FileName := IncludeTrailingPathDelimiter(APath) + F.Name;
            if (BorlandIDEServices as IOTAServices).IsProject(FileName) then
              FProjectNames.Add(FileName);
            if (BorlandIDEServices as IOTAServices).IsProjectGroup(FileName) then
              FProjectGroupNames.Add(FileName);
          end;
        end;
        R := FindNext(F);
      end;
    finally
      FindClose(F);
    end;
  end;

begin
  FindProjects(FTargetDir);
end;

procedure TCheckoutThread.SyncAdd;
begin
  FUpdateDialog.Add(FSyncPath, FSyncAction, False, FSyncTextColor);
end;

procedure TCheckoutThread.SyncCompleted;
begin
  if FExceptionMessage <> '' then
    //ShowSvnExceptionMessage(FExceptionMessage)
  else
  begin
    SearchProjects;
    EliminateChildProjectFiles(FProjectNames);
    FProjectGroupNames.Sort;
    OpenProject;
  end;
  FUpdateDialog.Completed;
end;

end.
