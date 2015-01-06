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
{ The Original Code is VerInsAddInOptionsFrame.pas.                            }
{                                                                              }
{ The Initial Developer of the Original Code is Uwe Schuster.                  }
{ Portions created by Uwe Schuster are Copyright © 2011 - 2015 Uwe Schuster.   }
{ All Rights Reserved.                                                         }
{                                                                              }
{ Contributors:                                                                }
{ Uwe Schuster (uschuster)                                                     }
{                                                                              }
{******************************************************************************}

unit VerInsAddInOptionsFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons;

type
  TfrmVerInsOptions = class(TFrame)
    GroupBox1: TGroupBox;
    cbEnableGit: TCheckBox;
    cbEnableSubversion: TCheckBox;
    cbEnableMercurial: TCheckBox;
    procedure cbEnableGitClick(Sender: TObject);
  private
    { Private declarations }
    FWarningGit: Boolean;
    FWarningMercurial: Boolean;
    FWarningSubversion: Boolean;
    FWarningsEnabled: Boolean;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure EnableWarnings;
  end;

implementation

{$R *.dfm}

{ TfrmVerInsOptions }

procedure TfrmVerInsOptions.cbEnableGitClick(Sender: TObject);
var
  ShowWarning: Boolean;
begin
  if FWarningsEnabled then
  begin
    if Sender = cbEnableGit then
    begin
      ShowWarning := FWarningGit;
      FWarningGit := False;
    end
    else
    if Sender = cbEnableMercurial then
    begin
      ShowWarning := FWarningMercurial;
      FWarningMercurial := False;
    end
    else
    if Sender = cbEnableSubversion then
    begin
      ShowWarning := FWarningSubversion;
      FWarningSubversion := False;
    end
    else
      ShowWarning := False;
    if ShowWarning then
      MessageDlg('Changing this option will only take effect the next time the IDE is started', mtInformation, [mbOK], 0);
  end;
end;

constructor TfrmVerInsOptions.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWarningGit := True;
  FWarningMercurial := True;
  FWarningSubversion := True;
  FWarningsEnabled := False;
end;

procedure TfrmVerInsOptions.EnableWarnings;
begin
  FWarningsEnabled := True;
end;

end.
