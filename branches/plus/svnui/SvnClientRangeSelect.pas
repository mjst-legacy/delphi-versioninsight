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
{ The Original Code is SvnClientRangeSelect.pas.                               }
{                                                                              }
{ The Initial Developer of the Original Code is Uwe Schuster.                  }
{ Portions created by Uwe Schuster are Copyright © 2010 - 2014 Uwe Schuster.   }
{ All Rights Reserved.                                                         }
{                                                                              }
{ Contributors:                                                                }
{ Uwe Schuster (uschuster)                                                     }
{                                                                              }
{******************************************************************************}

unit SvnClientRangeSelect;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TSvnRangeSelectDialog = class(TForm)
    GroupBox2: TGroupBox;
    FromRevisionLabel: TLabel;
    FromSelectRevision: TEdit;
    GroupBox1: TGroupBox;
    ToRevisionLabel: TLabel;
    ToCurrentRevision: TCheckBox;
    ToSelectRevision: TEdit;
    Button1: TButton;
    Button2: TButton;
    procedure FromSelectRevisionChange(Sender: TObject);
    procedure ToCurrentRevisionClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  SvnRangeSelectDialog: TSvnRangeSelectDialog;

implementation

{$R *.dfm}

procedure TSvnRangeSelectDialog.ToCurrentRevisionClick(Sender: TObject);
begin
  ToRevisionLabel.Enabled := not ToCurrentRevision.Checked;
  ToSelectRevision.Enabled := not ToCurrentRevision.Checked;
  FromSelectRevisionChange(nil);
end;

procedure TSvnRangeSelectDialog.FromSelectRevisionChange(Sender: TObject);
begin
  Button1.Enabled := (StrToIntDef(FromSelectRevision.Text, -2) <> -2) and
    (ToCurrentRevision.Checked or (StrToIntDef(ToSelectRevision.Text, -2) <> -2))
end;

end.
