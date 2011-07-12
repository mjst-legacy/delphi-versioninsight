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
{ The Original Code is VerInsIDEUtils.pas.                                     }
{                                                                              }
{ The Initial Developer of the Original Code is Uwe Schuster.                  }
{ Portions created by Uwe Schuster are Copyright © 2011 Uwe Schuster. All      }
{ Rights Reserved.                                                             }
{                                                                              }
{ Contributors:                                                                }
{ Uwe Schuster (uschuster)                                                     }
{                                                                              }
{******************************************************************************}

unit VerInsIDEUtils;

interface

function RegisterMenuIcon(const AResourceName: string): Integer;
function RegisterMenuIconFromFile(const AFileName, AIdent: string): Integer;
function RegisterMenuIconFromModule(const AFileName, AIdent: string; ARessource: string): Integer;
function RegisterMenuIconFromModuleResourceID(const AFileName, AIdent: string; ARessourceID: Integer): Integer;
function RegisterMessageViewIcon(const AResourceName: string): Integer;
function RegisterMessageViewIconFromModuleResourceID(const AFileName, AIdent: string; ARessourceID: Integer): Integer;

implementation

uses
  {$IFDEF TOOLSPROAPI}
  ToolsProAPI,
  {$ENDIF TOOLSPROAPI}
  ToolsAPI, Windows, SysUtils, Graphics, Controls;

function RegisterMenuIcon(const AResourceName: string): Integer;
var
  ImageList: TImageList;
  Ico: TIcon;
begin
  ImageList := TImageList.Create(nil);
  Ico := TIcon.Create;
  try
    Ico.LoadFromResourceName(HInstance, AResourceName);
    ImageList.AddIcon(Ico);
    if ImageList.Count > 0 then
      Result := (BorlandIDEServices as INTAServices).AddImages(ImageList, AResourceName)
    else
      Result := -1;
  finally
    Ico.Free;
    ImageList.Free;
  end;
end;

function RegisterMenuIconFromFile(const AFileName, AIdent: string): Integer;
var
  ImageList: TImageList;
  Ico: TIcon;
begin
  Result := -1;
  ImageList := TImageList.Create(nil);
  Ico := TIcon.Create;
  try
    Ico.LoadFromFile(AFileName);
    ImageList.AddIcon(Ico);
    if ImageList.Count > 0 then
      Result := (BorlandIDEServices as INTAServices).AddImages(ImageList, AIdent);
  finally
    Ico.Free;
    ImageList.Free;
  end;
end;

function RegisterMenuIconFromModule(const AFileName, AIdent: string; ARessource: string): Integer;
var
  Module: HMODULE;
var
  ImageList: TImageList;
  Ico: TIcon;
begin
  Result := -1;
  if FileExists(AFileName) then
  begin
    Module := LoadLibraryEx(PChar(AFileName), 0, LOAD_LIBRARY_AS_DATAFILE);
    try
      if Module <> 0 then
      begin
        ImageList := TImageList.Create(nil);
        Ico := TIcon.Create;
        try
          Ico.LoadFromResourceName(Module, ARessource);
          ImageList.AddIcon(Ico);
          if ImageList.Count > 0 then
            Result := (BorlandIDEServices as INTAServices).AddImages(ImageList, AIdent);
        finally
          Ico.Free;
          ImageList.Free;
        end;
      end;
    finally
      if Module <> 0 then
        FreeLibrary(Module);
    end;
  end;
end;

function RegisterMenuIconFromModuleResourceID(const AFileName, AIdent: string; ARessourceID: Integer): Integer;
var
  Module: HMODULE;
var
  ImageList: TImageList;
  Ico: TIcon;
begin
  Result := -1;
  if (ExtractFilePath(AFileName) = '') or FileExists(AFileName) then
  begin
    Module := LoadLibraryEx(PChar(AFileName), 0, LOAD_LIBRARY_AS_DATAFILE);
    try
      if Module <> 0 then
      begin
        ImageList := TImageList.Create(nil);
        Ico := TIcon.Create;
        try
          Ico.LoadFromResourceID(Module, ARessourceID);
          ImageList.AddIcon(Ico);
          if ImageList.Count > 0 then
            Result := (BorlandIDEServices as INTAServices).AddImages(ImageList, AIdent);
        finally
          Ico.Free;
          ImageList.Free;
        end;
      end;
    finally
      if Module <> 0 then
        FreeLibrary(Module);
    end;
  end;
end;

function RegisterMessageViewIcon(const AResourceName: string): Integer;
{$IFDEF TOOLSPROAPI}
var
  ImageList: TImageList;
  Ico: TIcon;
{$ENDIF ~TOOLSPROAPI}
begin
  {$IFDEF TOOLSPROAPI}
  ImageList := TImageList.Create(nil);
  Ico := TIcon.Create;
  try
    Ico.LoadFromResourceName(HInstance, AResourceName);
    ImageList.AddIcon(Ico);
    if (ImageList.Count > 0) and Supports(BorlandIDEServices, INTAProMessageServices) then
      Result := (BorlandIDEServices as INTAProMessageServices).AddImages(ImageList, AResourceName)
    else
      Result := -1;
  finally
    Ico.Free;
    ImageList.Free;
  end;
  {$ELSE ~TOOLSPROAPI}
  Result := -1;
  {$ENDIF ~TOOLSPROAPI}
end;

function RegisterMessageViewIconFromModuleResourceID(const AFileName, AIdent: string; ARessourceID: Integer): Integer;
{$IFDEF TOOLSPROAPI}
var
  Module: HMODULE;
  ImageList: TImageList;
  Ico: TIcon;
{$ENDIF TOOLSPROAPI}
begin
  Result := -1;
  {$IFDEF TOOLSPROAPI}
  if (ExtractFilePath(AFileName) = '') or FileExists(AFileName) then
  begin
    Module := LoadLibraryEx(PChar(AFileName), 0, LOAD_LIBRARY_AS_DATAFILE);
    try
      if Module <> 0 then
      begin
        ImageList := TImageList.Create(nil);
        Ico := TIcon.Create;
        try
          Ico.LoadFromResourceID(Module, ARessourceID);
          ImageList.AddIcon(Ico);
          if (ImageList.Count > 0) and Supports(BorlandIDEServices, INTAProMessageServices) then
            Result := (BorlandIDEServices as INTAProMessageServices).AddImages(ImageList, AIdent)
          else
            Result := -1;
        finally
          Ico.Free;
          ImageList.Free;
        end;
      end;
    finally
      if Module <> 0 then
        FreeLibrary(Module);
    end;
  end;
  {$ENDIF TOOLSPROAPI}
end;

end.
