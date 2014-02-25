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
{ The Original Code is VerInsIDEAddInOptions.pas.                              }
{                                                                              }
{ The Initial Developer of the Original Code is Uwe Schuster.                  }
{ Portions created by Uwe Schuster are Copyright © 2011 - 2014 Uwe Schuster.   }
{ All Rights Reserved.                                                         }
{                                                                              }
{ Contributors:                                                                }
{ Uwe Schuster (uschuster)                                                     }
{                                                                              }
{******************************************************************************}

unit VerInsIDEAddInOptions;

interface

uses
  Forms, ToolsAPI, VerInsAddInOptionsFrame;

type
  TVerInsAddInOptions = class(TInterfacedObject, INTAAddInOptions)
  private
    FFrame: TfrmVerInsOptions;
  public
    procedure DialogClosed(Accepted: Boolean);
    procedure FrameCreated(AFrame: TCustomFrame);
    function GetArea: string;
    function GetCaption: string;
    function GetFrameClass: TCustomFrameClass;
    function GetHelpContext: Integer;
    function ValidateContents: Boolean;
    function IncludeInIDEInsight: Boolean;
  end;

procedure RegisterAddInOptions;

implementation

uses
  Registry;

{ THgAddInOptions }

procedure TVerInsAddInOptions.DialogClosed(Accepted: Boolean);
var
  RegIniFile: TRegIniFile;
  Key: string;
begin
  if Accepted then
  begin
    Key := (BorlandIDEServices as IOTAServices).GetBaseRegistryKey + '\VersionInsight';
    RegIniFile := TRegIniFile.Create(Key);
    try
      RegIniFile.WriteBool('Git', 'Enabled', FFrame.cbEnableGit.Checked);
      RegIniFile.WriteBool('Mercurial', 'Enabled', FFrame.cbEnableMercurial.Checked);
      RegIniFile.WriteBool('Subversion', 'Enabled', FFrame.cbEnableSubversion.Checked);
    finally
      RegIniFile.Free;
    end;
  end;
end;

procedure TVerInsAddInOptions.FrameCreated(AFrame: TCustomFrame);
var
  RegIniFile: TRegIniFile;
  Key: string;
begin
  FFrame := TfrmVerInsOptions(AFrame);
  Key := (BorlandIDEServices as IOTAServices).GetBaseRegistryKey + '\VersionInsight';
  RegIniFile := TRegIniFile.Create(Key);
  try
    FFrame.cbEnableGit.Checked := RegIniFile.ReadBool('Git', 'Enabled', True);
    FFrame.cbEnableMercurial.Checked := RegIniFile.ReadBool('Mercurial', 'Enabled', True);
    FFrame.cbEnableSubversion.Checked := RegIniFile.ReadBool('Subversion', 'Enabled', True);
  finally
    RegIniFile.Free;
  end;
  FFrame.EnableWarnings;
end;

function TVerInsAddInOptions.GetArea: string;
begin
  Result := 'Version Control';
end;

function TVerInsAddInOptions.GetCaption: string;
begin
  Result := 'Options';
end;

function TVerInsAddInOptions.GetFrameClass: TCustomFrameClass;
begin
  Result := TfrmVerInsOptions;
end;

function TVerInsAddInOptions.GetHelpContext: Integer;
begin
  Result := 0;
end;

function TVerInsAddInOptions.IncludeInIDEInsight: Boolean;
begin
  Result := True;
end;

function TVerInsAddInOptions.ValidateContents: Boolean;
begin
  Result := True;
end;

var
  VerInsAddInOptions: TVerInsAddInOptions = nil;

procedure RegisterAddInOptions;
begin
  VerInsAddInOptions := TVerInsAddInOptions.Create;
  (BorlandIDEServices as INTAEnvironmentOptionsServices).RegisterAddInOptions(VerInsAddInOptions);
end;

initialization

finalization
  (BorlandIDEServices as INTAEnvironmentOptionsServices).UnregisterAddInOptions(VerInsAddInOptions);
  VerInsAddInOptions := nil;

end.
