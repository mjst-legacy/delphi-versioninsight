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
{ The Original Code is SvnClientLogDialog.pas.                                 }
{                                                                              }
{ The Initial Developer of the Original Code is Uwe Schuster.                  }
{ Portions created by Uwe Schuster are Copyright © 2011 Uwe Schuster. All      }
{ Rights Reserved.                                                             }
{                                                                              }
{ Contributors:                                                                }
{ Uwe Schuster (uschuster)                                                     }
{                                                                              }
{******************************************************************************}

unit SvnClientLogDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, SvnClientLog, StdCtrls, ExtCtrls;

type
  TSvnLogDialog = class(TForm)
    Panel1: TPanel;
    OK: TButton;
    Cancel: TButton;
    Help: TButton;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FLogFrame: TSvnLogFrame;
  public
    { Public declarations }
    property LogFrame: TSvnLogFrame read FLogFrame;
  end;

var
  SvnLogDialog: TSvnLogDialog;

implementation

{$R *.dfm}

procedure TSvnLogDialog.FormCreate(Sender: TObject);
begin
  FLogFrame := TSvnLogFrame.Create(Self);
  FLogFrame.Parent := Self;
  FLogFrame.Align := alClient;
  Constraints.MinHeight := Height;
  Constraints.MinWidth := Width;
end;

end.
