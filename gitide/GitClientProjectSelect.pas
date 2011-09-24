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
unit GitClientProjectSelect;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, GitImages;

type
  TGitProjectSelectDialog = class(TForm)
    Names: TListView;
    Panel1: TPanel;
    Ok: TButton;
    Cancel: TButton;
    procedure NamesChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  function SelectProject(var FileName: string; const ProjectNames,
    ProjectGroupNames: TStringList): Boolean;

implementation

{$R *.dfm}

const
  ProjectGroupID = 0;
  ProjectID = 1;

function SelectProject(var FileName: string; const ProjectNames,
  ProjectGroupNames: TStringList): Boolean;
var
  SvnProjectSelectDialog: TGitProjectSelectDialog;
  I: Integer;
  Item: TListItem;
begin
  Result := False;
  if (ProjectNames.Count = 0) and (ProjectGroupNames.Count = 0) then
    Exit;
  SvnProjectSelectDialog := TGitProjectSelectDialog.Create(Application);
  try
    for I := 0 to ProjectGroupNames.Count - 1 do
    begin
      Item := SvnProjectSelectDialog.Names.Items.Add;
      Item.Caption := ProjectGroupNames[I];
      Item.GroupID := ProjectGroupID;
      Item.ImageIndex := GitImageModule.GetShellImageIndex(ProjectGroupNames[I]);
    end;
    for I := 0 to ProjectNames.Count - 1 do
    begin
      Item := SvnProjectSelectDialog.Names.Items.Add;
      Item.Caption := ProjectNames[I];
      Item.GroupID := ProjectID;
      Item.ImageIndex := GitImageModule.GetShellImageIndex(ProjectNames[I]);
    end;
    if SvnProjectSelectDialog.Names.Items.Count > 0 then
      SvnProjectSelectDialog.Names.Selected := SvnProjectSelectDialog.Names.Items[0];
    if SvnProjectSelectDialog.ShowModal = mrOk then
    begin
      if Assigned(SvnProjectSelectDialog.Names.Selected) then
      begin
        FileName := SvnProjectSelectDialog.Names.Selected.Caption;
        Result := True
      end;
    end;
  finally
    SvnProjectSelectDialog.Free;
  end;
end;

procedure TGitProjectSelectDialog.NamesChange(Sender: TObject;
  Item: TListItem; Change: TItemChange);
begin
  if Change = ctState then
    Ok.Enabled := Assigned(Names.Selected);
end;

end.
