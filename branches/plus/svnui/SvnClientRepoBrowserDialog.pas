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
{ The Initial Developer of the Original Code is Uwe Schuster.                  }
{                                                                              }
{ Portions created or modified by Embarcadero Technologies are                 }
{ Copyright © 2010 Embarcadero Technologies, Inc. All Rights Reserved          }
{ Modifications include a major re-write of delphisvn. New functionality for   }
{ diffing, international character support, asynchronous gathering of data,    }
{ check-out and import, usability, tighter integration into RAD Studio, and    }
{ other new features.  Most original source files not used or re-written.      }
{                                                                              }
{ Contributors:                                                                }
{ Uwe Schuster (uschuster)                                                     }
{                                                                              }
{******************************************************************************}

unit SvnClientRepoBrowserDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, SvnClient, SvnClientRepoBrowserFrame;

type
  TdlgRepoBrowser = class(TForm)
    Panel1: TPanel;
    OK: TButton;
    Cancel: TButton;
    Help: TButton;
    procedure FormCreate(Sender: TObject);
    procedure HelpClick(Sender: TObject);
  private
    { Private declarations }
    FRepoBrowserFrame: TfrmRepoBrowser;
  public
    { Public declarations }
    property RepoBrowserFrame: TfrmRepoBrowser read FRepoBrowserFrame;
  end;

var
  dlgRepoBrowser: TdlgRepoBrowser;

function GetRepoURL(SvnClient: TSvnClient; var AURL: string): Boolean;

implementation

{$R *.dfm}

function GetRepoURL(SvnClient: TSvnClient; var AURL: string): Boolean;
begin
  dlgRepoBrowser := TdlgRepoBrowser.Create(Application);
  try
    dlgRepoBrowser.RepoBrowserFrame.URL := AURL;
    dlgRepoBrowser.RepoBrowserFrame.SvnClient := SvnClient;
    Result := dlgRepoBrowser.ShowModal = mrOK;
    if Result then
      AURL := dlgRepoBrowser.RepoBrowserFrame.URL;
  finally
    dlgRepoBrowser.Free;
  end;
end;

procedure TdlgRepoBrowser.FormCreate(Sender: TObject);
begin
  Constraints.MinHeight := MulDiv(350, Screen.PixelsPerInch, 96);
  Constraints.MinWidth := MulDiv(550, Screen.PixelsPerInch, 96);
  FRepoBrowserFrame := TfrmRepoBrowser.Create(Self);
  FRepoBrowserFrame.Parent := Self;
  FRepoBrowserFrame.Align := alClient;
end;

procedure TdlgRepoBrowser.HelpClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

end.
