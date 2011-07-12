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
unit SvnClientConflict;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TSvnConflictDialog = class(TForm)
    Local: TRadioButton;
    Server: TRadioButton;
    Merge: TRadioButton;
    Ok: TButton;
    Description: TLabel;
    MergeView: TRadioButton;
  public
    constructor Create(AOwner: TComponent; const FileName, MergeViewerName: string;
      CanPostpone: Boolean); reintroduce;
  end;

implementation

uses SvnUIConst;

{$R *.dfm}

{ TSvnConflictDialog }

constructor TSvnConflictDialog.Create(AOwner: TComponent;
  const FileName, MergeViewerName: string; CanPostpone: Boolean);
var
  DescHeight: Integer;
  DescDelta: Integer;
begin
  inherited Create(AOwner);
  DescHeight := Description.Height;
  Description.Caption := Format(Description.Caption, [FileName]);
  DescDelta := Description.Height - DescHeight;
  // Resize and move everything
  if DescDelta <> 0 then
  begin
    Height := Height + DescDelta;
    Local.Top := Local.Top + DescDelta;
    Server.Top := Server.Top + DescDelta;
    Merge.Top := Merge.Top + DescDelta;
    Ok.Top := Ok.Top + DescDelta;
    MergeView.Top := MergeView.Top + DescDelta;
  end;
  Merge.Visible := CanPostPone;
  if  MergeViewerName <> '' then
  begin
    MergeView.Visible := True;
    MergeView.Caption := Format(MergeView.Caption, [MergeViewerName]);
  end
  else
    MergeView.Visible := False;
  if (not Merge.Visible) and MergeView.Visible then
    MergeView.Top := Merge.Top;
end;

end.
