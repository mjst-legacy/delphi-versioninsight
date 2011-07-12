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
{ The Original Code is VerInsIDEBlameAddInOptions.pas.                         }
{                                                                              }
{ The Initial Developer of the Original Code is Uwe Schuster.                  }
{ Portions created by Uwe Schuster are Copyright © 2011 Uwe Schuster. All      }
{ Rights Reserved.                                                             }
{                                                                              }
{ Contributors:                                                                }
{ Uwe Schuster (uschuster)                                                     }
{                                                                              }
{******************************************************************************}

unit VerInsIDEBlameAddInOptions;

interface

uses
  Forms, ToolsAPI, VerInsBlameOptionsFrame;

type
  TVerInsBlameAddInOptions = class(TInterfacedObject, INTAAddInOptions)
  private
    FFrame: TfrmVerInsBlameOptions;
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
  VerInsBlameSettings;

{ TVerInsBlameAddInOptions }

procedure TVerInsBlameAddInOptions.DialogClosed(Accepted: Boolean);
begin
  if Accepted and (GetPresets <> nil) then
  begin
    FFrame.UpdatePresets;
    GetPresets.Save;
  end;
end;

procedure TVerInsBlameAddInOptions.FrameCreated(AFrame: TCustomFrame);
begin
  FFrame := TfrmVerInsBlameOptions(AFrame);
  FFrame.Presets := GetPresets;
  FFrame.Init;
end;

function TVerInsBlameAddInOptions.GetArea: string;
begin
  Result := 'Version Control';
end;

function TVerInsBlameAddInOptions.GetCaption: string;
begin
  Result := 'Blame';
end;

function TVerInsBlameAddInOptions.GetFrameClass: TCustomFrameClass;
begin
  Result := TfrmVerInsBlameOptions;
end;

function TVerInsBlameAddInOptions.GetHelpContext: Integer;
begin
  Result := 0;
end;

function TVerInsBlameAddInOptions.IncludeInIDEInsight: Boolean;
begin
  Result := True;
end;

function TVerInsBlameAddInOptions.ValidateContents: Boolean;
begin
  Result := True;
end;

var
  VerInsBlameAddInOptions: TVerInsBlameAddInOptions = nil;

procedure RegisterAddInOptions;
begin
  VerInsBlameAddInOptions := TVerInsBlameAddInOptions.Create;
  (BorlandIDEServices as INTAEnvironmentOptionsServices).RegisterAddInOptions(VerInsBlameAddInOptions);
end;

initialization

finalization
  (BorlandIDEServices as INTAEnvironmentOptionsServices).UnregisterAddInOptions(VerInsBlameAddInOptions);
  VerInsBlameAddInOptions := nil;

end.
