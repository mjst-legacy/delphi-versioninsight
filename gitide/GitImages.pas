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
{ This unit contains images for Subversion client UI.                          }
{                                                                              }
{******************************************************************************}

unit GitImages;

interface

uses Windows, SysUtils, Classes, Graphics, jpeg, ImgList, Controls;

type
  TOverlayIcon = (oiAdded, oiConflict, oiDeleted, oiSubversion, oiLocked, oiModified, oiReadOnly);

  PShellInfo = ^TShellInfo;
  TShellInfo = record
    ImageIndexes: array[Boolean, Boolean] of Integer;
    TypeName: array[0..MAX_PATH - 1] of Char;
  end;

  TGitImageModule = class(TDataModule)
    ShellImagesLarge: TImageList;
    ShellImagesSmall: TImageList;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    FShellCache: TStringList;
    procedure ClearShellCache;
  public
    function GetShellImageIndex(const Path: string; Open: Boolean = False; Large: Boolean = False): Integer;
  end;

var
  GitImageModule: TGitImageModule = nil;

implementation

uses
  ComObj, ShellAPI, VerInsResources;

{$R *.dfm}

// TDataSvnImages private

procedure TGitImageModule.ClearShellCache;
var
  I: Integer;
begin
  for I := 0 to FShellCache.Count - 1 do
    FreeMem(Pointer(FShellCache.Objects[I]));
  FShellCache.Clear;
end;

// TDataSvnImages public

function TGitImageModule.GetShellImageIndex(const Path: string; Open: Boolean = False; Large: Boolean = False): Integer;
const
  OpenFlags: array[Boolean] of Cardinal = (0, SHGFI_OPENICON);
  LargeFlags: array[Boolean] of Cardinal = (SHGFI_SMALLICON, 0);
var
  Ext, S: string;
  Index: Integer;
  FileInfo: TSHFileInfo;
  SysImageList: THandle;
  P: PShellInfo;
begin
  Result := -1;
  if Path = '' then
    Exit;

  if FileGetAttr(Path) and FILE_ATTRIBUTE_DIRECTORY = 0 then
  begin
    Ext := AnsiUpperCase(ExtractFileExt(Path));
    // for files with no extension, store invalid filename character '?' (to distinguish from non-root directories)
    if Ext = '' then
      S := '?'
    // for *.exe, *.cur and *.ico files, store the full path to use their individual icons
    else if AnsiSameText(Ext, '.ICO') or AnsiSameText(Ext, '.CUR') or AnsiSameText(Ext, '.EXE') then
      S := Path
    // otherwise store the extension
    else
      S := Ext;
  end
  else
  begin
    // for root directories, store the full path to use their individual icons (local drives vs. network shares)
    if AnsiSameText(ExcludeTrailingPathDelimiter(Path), ExtractFileDrive(Path)) then
      S := Path
    // for non-root directories, store an empty string
    else
      S := '';
  end;

  if FShellCache.Find(S, Index) then
    P := PShellInfo(FShellCache.Objects[Index])
  else
  begin
    P := AllocMem(SizeOf(TShellInfo));
    try
      FillChar(FileInfo, SizeOf(TSHFileInfo), 0);
      P^.ImageIndexes[False, False] := -1;
      P^.ImageIndexes[False, True] := -1;
      P^.ImageIndexes[True, False] := -1;
      P^.ImageIndexes[True, True] := -1;

      FShellCache.AddObject(S, TObject(P));
    except
      FreeMem(P);
      raise;
    end;
  end;

  Result := P^.ImageIndexes[Open, Large];
  if Result = -1 then
  begin
    SysImageList := SHGetFileInfo(PChar(Path), FILE_ATTRIBUTE_NORMAL, FileInfo, SizeOf(TSHFileInfo),
      SHGFI_ICON or SHGFI_SYSICONINDEX or OpenFlags[Open] or LargeFlags[Large]);
    if FileInfo.iIcon <> 0 then
    begin
      Result := FileInfo.iIcon;
      P^.ImageIndexes[Open, Large] := Result;

      DestroyIcon(FileInfo.hIcon);
      if SysImageList <> 0 then
      begin
        if Large then
          ShellImagesLarge.Handle := SysImageList
        else
          ShellImagesSmall.Handle := SysImageList;
      end;
    end;
  end;
end;

// TDataSvnImages event handlers

procedure TGitImageModule.DataModuleCreate(Sender: TObject);
begin
  FShellCache := TStringList.Create;
  FShellCache.Duplicates := dupError;
  FShellCache.Sorted := True;
end;

procedure TGitImageModule.DataModuleDestroy(Sender: TObject);
begin
  ClearShellCache;
  FShellCache.Free;
end;

end.
