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
{ The Original Code is SvnClientEditComment.pas.                               }
{                                                                              }
{ The Initial Developer of the Original Code is Uwe Schuster.                  }
{ Portions created by Uwe Schuster are Copyright © 2011 - 2014 Uwe Schuster.   }
{ All Rights Reserved.                                                         }
{                                                                              }
{ Contributors:                                                                }
{ Uwe Schuster (uschuster)                                                     }
{                                                                              }
{******************************************************************************}

unit SvnClientEditComment;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TEditCommentDialog = class(TForm)
    Panel2: TPanel;
    Panel3: TPanel;
    Ok: TButton;
    Cancel: TButton;
    Comment: TMemo;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

function EditComment(const AOwner: TComponent; var AComment: string): Boolean;

implementation

{$R *.dfm}

function EditComment(const AOwner: TComponent; var AComment: string): Boolean;
var
  EditCommentDialog: TEditCommentDialog;
begin
  EditCommentDialog := TEditCommentDialog.Create(AOwner);
  EditCommentDialog.Comment.Text := AComment;
  EditCommentDialog.Comment.SelStart := Length(AComment);
  Result := EditCommentDialog.ShowModal = mrOk;
  if Result then
    AComment := EditCommentDialog.Comment.Text;
end;

procedure TEditCommentDialog.FormCreate(Sender: TObject);
begin
  Constraints.MinHeight := MulDiv(300, Screen.PixelsPerInch, 96);
  Constraints.MinWidth := MulDiv(450, Screen.PixelsPerInch, 96);
end;

end.
