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
{ The Original Code is GitIDEIcons.pas.                                        }
{                                                                              }
{ The Initial Developer of the Original Code is Uwe Schuster.                  }
{ Portions created by Uwe Schuster are Copyright © 2011 - 2014 Uwe Schuster.   }
{ All Rights Reserved.                                                         }
{                                                                              }
{ Contributors:                                                                }
{ Uwe Schuster (uschuster)                                                     }
{                                                                              }
{******************************************************************************}

unit GitIDEIcons;

interface

var
  //Project Manager/Editor popup menu
  GitImageIndex: Integer = -1;
  CommitImageIndex: Integer = -1;
  LogImageIndex: Integer = -1;
  RevertImageIndex: Integer = -1;

  //Message View
  GitMessageViewImageIndex: Integer = -1;

procedure RegisterImages;

implementation

uses
  ToolsAPI, SysUtils, Registry, VerInsIDEUtils;

{.$R ..\resources\git.res}

procedure RegisterImages;

  function GetGitIconFileName: string;
  var
    Key, Dir: string;
    RegIniFile: TRegIniFile;
  begin
    Result := '';
    Key := (BorlandIDEServices as IOTAServices).GetBaseRegistryKey + '\VersionInsight';
    RegIniFile := TRegIniFile.Create(Key);
    try
      Dir := ExtractFilePath(RegIniFile.ReadString('Git', 'Executable', ''));
      if DirectoryExists(Dir) and SameText(Copy(Dir, Length(Dir) - 3, 4), 'bin\') then
      begin
        Delete(Dir, Length(Dir) - 3, 4);
        if FileExists(Dir + 'etc\git.ico') then
          Result := Dir + 'etc\git.ico'
        else
        if FileExists(Dir + 'unins000.exe') then
          Result := Dir + 'unins000.exe';
      end;
    finally
      RegIniFile.Free;
    end;
  end;

var
  GitIconFileName: string;
begin
  //GitImageIndex := RegisterMenuIcon('git_menuicon');
  GitIconFileName := GetGitIconFileName;
  if GitIconFileName <> '' then
  begin
    if SameText(ExtractFileExt(GitIconFileName), '.ico') then
      GitImageIndex := RegisterMenuIconFromFile(GitIconFileName, 'git_menuicon')
    else
      GitImageIndex := RegisterMenuIconFromModule(GitIconFileName, 'git_menuicon', 'MAINICON');
  end;

  CommitImageIndex := RegisterMenuIcon('tsvn_menucommit');
  LogImageIndex := RegisterMenuIcon('tsvn_menulog');
  RevertImageIndex := RegisterMenuIcon('tsvn_menurevert');

  if GitIconFileName <> '' then
  begin
    if SameText(ExtractFileExt(GitIconFileName), '.ico') then
      GitMessageViewImageIndex := RegisterMessageViewIconFromFile(GitIconFileName, 'git_menuicon')
    else
      GitMessageViewImageIndex := RegisterMessageViewIconFromModule(GitIconFileName, 'git_menuicon', 'MAINICON');
  end;
end;

end.
