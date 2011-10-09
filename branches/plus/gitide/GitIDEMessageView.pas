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
unit GitIDEMessageView;

interface

uses
  ToolsAPI;

type
  TSvnMessageView = class(TInterfacedObject, IOTAMessageNotifier)
  protected
    FMessageGroup: IOTAMessageGroup;
    FNotifierIndex: Integer;
    {IOTANotifier}
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;
    { IOTAMessageNotifier }
    procedure MessageGroupAdded(const Group: IOTAMessageGroup);
    procedure MessageGroupDeleted(const Group: IOTAMessageGroup);
    { Misc }
    function GetMessageGroup: IOTAMessageGroup;
  public
    constructor Create;
    destructor Destroy; override;
    procedure CheckMessageGroup(ClearMessages: Boolean = False);
    procedure WriteMessage(const AFileName, AText: string);
    procedure WriteTitle(const TitleMessage: string);
    property MessageGroup: IOTAMessageGroup read GetMessageGroup;
  end;

  TSvnMessage = class(TInterfacedObject, IOTACustomMessage, IOTACustomMessage50,
    IOTACustomMessage100)
  private
    FFileName: string;
    FText: string;
    { IOTACustomMessage }
    function GetColumnNumber: Integer;
    function GetFileName: string;
    function GetLineNumber: Integer;
    function GetLineText: string;
    procedure ShowHelp;

    { IOTACustomMessage50 }
    function GetChildCount: Integer;
    function GetChild(Index: Integer): IOTACustomMessage50;

    { IOTACustomMessage100 }
    function CanGotoSource(var DefaultHandling: Boolean): Boolean;
    procedure TrackSource(var DefaultHandling: Boolean);
    procedure GotoSource(var DefaultHandling: Boolean);
  public
    constructor Create(const AFileName, AText: string);
  end;

var
  SvnMessageView: TSvnMessageView;

implementation

uses
  SysUtils, GitIDEConst;

{ TSvnMessage }

function TSvnMessage.CanGotoSource(var DefaultHandling: Boolean): Boolean;
begin
  DefaultHandling := FileExists(FFileName);
  Result := DefaultHandling;
end;

constructor TSvnMessage.Create(const AFileName, AText: string);
begin
  inherited Create;
  FFileName := AFileName;
  FText := AText;
end;

function TSvnMessage.GetChild(Index: Integer): IOTACustomMessage50;
begin
  Result := nil;
end;

function TSvnMessage.GetChildCount: Integer;
begin
  Result := 0;
end;

function TSvnMessage.GetColumnNumber: Integer;
begin
  Result := 0;
end;

function TSvnMessage.GetFileName: string;
begin
  Result := FFileName;
end;

function TSvnMessage.GetLineNumber: Integer;
begin
  Result := 1;
end;

function TSvnMessage.GetLineText: string;
begin
  Result := FText;
end;

procedure TSvnMessage.GotoSource(var DefaultHandling: Boolean);
begin
  DefaultHandling := True;
end;

procedure TSvnMessage.ShowHelp;
begin

end;

procedure TSvnMessage.TrackSource(var DefaultHandling: Boolean);
begin
  DefaultHandling := True;
end;

{ TSvnMessageView }

procedure TSvnMessageView.AfterSave;
begin

end;

procedure TSvnMessageView.BeforeSave;
begin

end;

procedure TSvnMessageView.CheckMessageGroup(ClearMessages: Boolean);
begin
  if not Assigned(FMessageGroup) then
  begin
    FMessageGroup := (BorlandIDEServices as IOTAMessageServices).GetGroup(sGit);
    if not Assigned(FMessageGroup) then
    begin
      FMessageGroup := (BorlandIDEServices as IOTAMessageServices).AddMessageGroup(sGit);
      FMessageGroup.AutoScroll := True;
    end;
  end;
  if ClearMessages then
    (BorlandIDEServices as IOTAMessageServices).ClearMessageGroup(FMessageGroup);
  (BorlandIDEServices as IOTAMessageServices).ShowMessageView(FMessageGroup);
end;

constructor TSvnMessageView.Create;
begin
  inherited;
  FNotifierIndex := (BorlandIDEServices as IOTAMessageServices).AddNotifier(Self as IOTAMessageNotifier);
end;

destructor TSvnMessageView.Destroy;
begin
  inherited;
end;

procedure TSvnMessageView.Destroyed;
begin
  FNotifierIndex := -1;
end;

function TSvnMessageView.GetMessageGroup: IOTAMessageGroup;
begin
  CheckMessageGroup;
  Result := FMessageGroup;
end;

procedure TSvnMessageView.MessageGroupAdded(const Group: IOTAMessageGroup);
begin
// Not used
end;

procedure TSvnMessageView.MessageGroupDeleted(const Group: IOTAMessageGroup);
begin
  if Group.Name = sGit then
    FMessageGroup := nil;
end;

procedure TSvnMessageView.Modified;
begin

end;

procedure TSvnMessageView.WriteMessage(const AFileName, AText: string);
begin
  CheckMessageGroup;
  (BorlandIDEServices as IOTAMessageServices).
    AddCustomMessage(TSvnMessage.Create(AFileName, AText), FMessageGroup);
end;

procedure TSvnMessageView.WriteTitle(const TitleMessage: string);
begin
  CheckMessageGroup;
  (BorlandIDEServices as IOTAMessageServices).
    AddTitleMessage(TitleMessage, FMessageGroup);
end;

initialization
  SvnMessageView := TSvnMessageView.Create;
finalization
  if Assigned(SvnMessageView) and (SvnMessageView.FNotifierIndex <> -1) then
    (BorlandIDEServices as IOTAMessageServices).RemoveNotifier(SvnMessageView.FNotifierIndex);
  SvnMessageView := nil;
end.
