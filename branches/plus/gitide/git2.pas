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
{ The Original Code is git2.pas.                                               }
{                                                                              }
{ The Initial Developer of the Original Code is Uwe Schuster.                  }
{ Portions created by Uwe Schuster are Copyright © 2015 Uwe Schuster.          }
{ All Rights Reserved.                                                         }
{                                                                              }
{ Contributors:                                                                }
{ Uwe Schuster (uschuster)                                                     }
{                                                                              }
{******************************************************************************}

unit git2;

interface

uses
  Windows;

type
  size_t = NativeUInt;

//----- buffer.h -------------------------------------------------------------------------------------------------------

type
  Pgit_buf = ^Tgit_buf;
  Tgit_buf = record
    ptr: PAnsiChar;
    asize, size: size_t;
  end;

function GIT_BUF_INIT_CONST(STR: PAnsiChar; LEN: size_t): Tgit_buf;

var
  git_buf_free: procedure(buffer: Pgit_buf); cdecl;

//----- types.h --------------------------------------------------------------------------------------------------------

type
  Pgit_repository = ^Tgit_repository;
  Tgit_repository = record
  end;

//----- global.h -------------------------------------------------------------------------------------------------------

var
  git_libgit2_init: function: Integer; cdecl;
  git_libgit2_shutdown: function: Integer; cdecl;

//----- repository.h ---------------------------------------------------------------------------------------------------

var
  git_repository_open: function(out out_: Pgit_repository; const path: PAnsiChar): Integer; cdecl;
  git_repository_discover: function(out_: Pgit_buf; const start_path: PAnsiChar; across_fs: Integer; const ceiling_dirs: PAnsiChar): Integer; cdecl;
  git_repository_free: procedure(repo: Pgit_repository); cdecl;

//----- status.h -------------------------------------------------------------------------------------------------------

const
  GIT_STATUS_CURRENT          = 0;

  GIT_STATUS_INDEX_NEW        = 1 shl 0;
  GIT_STATUS_INDEX_MODIFIED   = 1 shl 1;
  GIT_STATUS_INDEX_DELETED    = 1 shl 2;
  GIT_STATUS_INDEX_RENAMED    = 1 shl 3;
  GIT_STATUS_INDEX_TYPECHANGE = 1 shl 4;

  GIT_STATUS_WT_NEW           = 1 shl 7;
  GIT_STATUS_WT_MODIFIED      = 1 shl 8;
  GIT_STATUS_WT_DELETED       = 1 shl 9;
  GIT_STATUS_WT_TYPECHANGE    = 1 shl 10;
  GIT_STATUS_WT_RENAMED       = 1 shl 11;
  GIT_STATUS_WT_UNREADABLE    = 1 shl 12;

  GIT_STATUS_IGNORED          = 1 shl 14;
  GIT_STATUS_CONFLICTED       = 1 shl 15;

var
  git_status_file: function(out status_flags: UInt; repo: Pgit_repository; const path: PAnsiChar): Integer; cdecl;

//----------------------------------------------------------------------------------------------------------------------

function Git2LibLoaded: Boolean;

function LoadGit2Lib(const AFileName: string = ''): Boolean;

procedure FreeGit2Lib;

implementation

var
  Git2Lib: THandle = INVALID_HANDLE_VALUE;

function Git2LibLoaded: Boolean;
begin
  Result := Git2Lib <> INVALID_HANDLE_VALUE;
end;

function LoadGit2Lib(const AFileName: string = ''): Boolean;
var
  LibFileName: string;
begin
  Result := not Git2LibLoaded;
  if Result then
  begin
    if AFileName <> '' then
      LibFileName := AFileName
    else
      LibFileName := 'git2.dll';
    Git2Lib := LoadLibrary(PChar(LibFileName));
    Result := Git2Lib <> 0;
    if not Result then
      Git2Lib := INVALID_HANDLE_VALUE
    else
    begin
      @git_buf_free := GetProcAddress(Git2Lib, 'git_buf_free');
      @git_libgit2_init := GetProcAddress(Git2Lib, 'git_libgit2_init');
      @git_libgit2_shutdown := GetProcAddress(Git2Lib, 'git_libgit2_shutdown');
      @git_repository_open := GetProcAddress(Git2Lib, 'git_repository_open');
      @git_repository_discover := GetProcAddress(Git2Lib, 'git_repository_discover');
      @git_repository_free := GetProcAddress(Git2Lib, 'git_repository_free');
      @git_status_file := GetProcAddress(Git2Lib, 'git_status_file');
    end;
  end;
end;

procedure FreeGit2Lib;
begin
  if Git2LibLoaded then
    FreeLibrary(Git2Lib);
  Git2Lib := INVALID_HANDLE_VALUE;
  @git_buf_free := nil;
  @git_libgit2_init := nil;
  @git_libgit2_shutdown := nil;
  @git_repository_open := nil;
  @git_repository_discover := nil;
  @git_repository_free := nil;
  @git_status_file := nil;
end;

//----- buffer.h -------------------------------------------------------------------------------------------------------

function GIT_BUF_INIT_CONST(STR: PAnsiChar; LEN: size_t): Tgit_buf;
begin
  Result.ptr := STR;
  Result.asize := 0;
  Result.size := 0;
end;

initialization

finalization
  FreeGit2Lib;

end.
