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
unit HgClientRecentComments;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  THgRecentCommentsDialog = class(TForm)
    Panel1: TPanel;
    Splitter1: TSplitter;
    Panel2: TPanel;
    Panel3: TPanel;
    Ok: TButton;
    RecentComment: TListBox;
    Cancel: TButton;
    Comment: TMemo;
    procedure RecentCommentClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  function SelectRecentComments(const AOwner: TComponent; const Comments: TStringList): string;

implementation

{$R *.dfm}

function SelectRecentComments(const AOwner: TComponent; const Comments: TStringList): string;
var
  RecentCommentsDialog: THgRecentCommentsDialog;
begin
  RecentCommentsDialog := THgRecentCommentsDialog.Create(AOwner);
  RecentCommentsDialog.RecentComment.Items.Assign(Comments);
  if RecentCommentsDialog.ShowModal = mrOk then
    Result := RecentCommentsDialog.Comment.Text
  else
    Result := '';
end;

procedure THgRecentCommentsDialog.FormCreate(Sender: TObject);
begin
  Constraints.MinHeight := MulDiv(350, Screen.PixelsPerInch, 96);
  Constraints.MinWidth := MulDiv(450, Screen.PixelsPerInch, 96);
  RecentComment.ItemIndex := 0;
end;

procedure THgRecentCommentsDialog.RecentCommentClick(Sender: TObject);
begin
  if RecentComment.ItemIndex <> -1 then
    Comment.Text := RecentComment.Items[RecentComment.ItemIndex]
  else
    Comment.Clear;
end;

end.
