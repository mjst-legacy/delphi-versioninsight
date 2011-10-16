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
{ The Original Code is HgAddInOptionsFrame.pas.                                }
{                                                                              }
{ The Initial Developer of the Original Code is Uwe Schuster.                  }
{ Portions created by Uwe Schuster are Copyright © 2010 Uwe Schuster. All      }
{ Rights Reserved.                                                             }
{                                                                              }
{ Contributors:                                                                }
{ Uwe Schuster (uschuster)                                                     }
{                                                                              }
{******************************************************************************}

unit HgAddInOptionsFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls, Buttons, ExtCtrls;

type
  TfrmHgTestsOptions = class(TFrame)
    GroupBox1: TGroupBox;
    edHgExecutable: TEdit;
    Label1: TLabel;
    OpenDialog1: TOpenDialog;
    SpeedButton1: TSpeedButton;
    GroupBox2: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    cboxConflicted: TColorBox;
    cboxAdded: TColorBox;
    cboxDeleted: TColorBox;
    cboxMerged: TColorBox;
    cboxModified: TColorBox;
    cbStatusColorsEnabled: TCheckBox;
    GroupBox3: TGroupBox;
    cbDeleteBackupFilesAfterCommit: TCheckBox;
    procedure SpeedButton1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TfrmHgTestsOptions.SpeedButton1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
    edHgExecutable.Text := OpenDialog1.FileName;
end;

end.
