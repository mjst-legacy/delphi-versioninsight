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
{ The Original Code is HgIDEIcons.pas.                                         }
{                                                                              }
{ The Initial Developer of the Original Code is Uwe Schuster.                  }
{ Portions created by Uwe Schuster are Copyright © 2011 - 2014 Uwe Schuster.   }
{ All Rights Reserved.                                                         }
{                                                                              }
{ Contributors:                                                                }
{ Uwe Schuster (uschuster)                                                     }
{                                                                              }
{******************************************************************************}

unit HgIDEIcons;

interface

var
  //Project Manager/Editor popup menu
  MercurialImageIndex: Integer = -1;
  CommitImageIndex: Integer = -1;
  LogImageIndex: Integer = -1;
  RevertImageIndex: Integer = -1;

  //Message View
  MercurialMessageViewImageIndex: Integer = -1;

procedure RegisterImages;

implementation

uses
  ToolsAPI, Registry, VerInsIDEUtils;

procedure RegisterImages;

  function GetMercurialIconBinName: string;
  var
    Key: string;
    RegIniFile: TRegIniFile;
  begin
    Key := (BorlandIDEServices as IOTAServices).GetBaseRegistryKey + '\VersionInsight';
    RegIniFile := TRegIniFile.Create(Key);
    try
      Result := RegIniFile.ReadString('Mercurial', 'Executable', '');
    finally
      RegIniFile.Free;
    end;
  end;

begin
  //MercurialImageIndex := RegisterMenuIcon('mercurial_menuicon');
  MercurialImageIndex := RegisterMenuIconFromModuleResourceID(GetMercurialIconBinName, 'mercurial_menuicon', 0);
  CommitImageIndex := RegisterMenuIcon('tsvn_menucommit');
  LogImageIndex := RegisterMenuIcon('tsvn_menulog');
  RevertImageIndex := RegisterMenuIcon('tsvn_menurevert');

  //MercurialMessageViewImageIndex := RegisterMessageViewIcon('mercurial_menuicon');
  MercurialMessageViewImageIndex := RegisterMessageViewIconFromModuleResourceID(GetMercurialIconBinName, 'mercurial_menuicon', 0);
end;

end.
