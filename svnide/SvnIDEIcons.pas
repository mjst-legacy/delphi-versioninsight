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
{ The Original Code is SvnIDEIcons.pas.                                        }
{                                                                              }
{ The Initial Developer of the Original Code is Uwe Schuster.                  }
{ Portions created by Uwe Schuster are Copyright © 2011 Uwe Schuster. All      }
{ Rights Reserved.                                                             }
{                                                                              }
{ Contributors:                                                                }
{ Uwe Schuster (uschuster)                                                     }
{                                                                              }
{******************************************************************************}

unit SvnIDEIcons;

interface

var
  //Project Manager/Editor popup menu
  SubversionImageIndex: Integer = -1;
  CommitImageIndex: Integer = -1;
  UpdateImageIndex: Integer = -1;
  LogImageIndex: Integer = -1;
  CleanImageIndex: Integer = -1;
  RepoBrowserImageIndex: Integer = -1;

  //Message View
  SubversionMessageViewImageIndex: Integer = -1;

procedure RegisterImages;

implementation

uses
  ToolsAPI, SysUtils, Registry, VerInsIDEUtils;

procedure RegisterImages;

  function GetSubversionIconBinName: string;
  const
    sLibSvnClientDll = 'libsvn_client-1.dll';
  var
    Key: string;
    RegIniFile: TRegIniFile;
  begin
    Key := (BorlandIDEServices as IOTAServices).GetBaseRegistryKey;
    RegIniFile := TRegIniFile.Create(Key);
    try
      Result := RegIniFile.ReadString('Subversion', 'SvnDllDir', '');
    finally
      RegIniFile.Free;
    end;
    if FileExists(IncludeTrailingPathDelimiter(Result) + sLibSvnClientDll) then
      Result := IncludeTrailingPathDelimiter(Result) + sLibSvnClientDll
    else
      Result := sLibSvnClientDll;
  end;

begin
  //SubversionImageIndex := RegisterMenuIcon('subversion_menuicon');
  SubversionImageIndex := RegisterMenuIconFromModuleResourceID(GetSubversionIconBinName, 'subversion_menuicon', 1);
  CommitImageIndex := RegisterMenuIcon('tsvn_menucommit');
  UpdateImageIndex := RegisterMenuIcon('tsvn_menuupdate');
  LogImageIndex := RegisterMenuIcon('tsvn_menulog');
  CleanImageIndex := RegisterMenuIcon('tsvn_menucleanup');
  RepoBrowserImageIndex := RegisterMenuIcon('tsvn_menurepobrowse');

  //SubversionMessageViewImageIndex := RegisterMessageViewIcon('subversion_menuicon');
  SubversionMessageViewImageIndex := RegisterMessageViewIconFromModuleResourceID(GetSubversionIconBinName, 'subversion_menuicon', 1);
end;

end.
