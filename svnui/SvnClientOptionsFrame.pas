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
{ The Original Code is SvnClientOptionsFrame.pas.                              }
{                                                                              }
{ The Initial Developer of the Original Code is Uwe Schuster.                  }
{ Portions created by Uwe Schuster are Copyright © 2010 Uwe Schuster. All      }
{ Rights Reserved.                                                             }
{                                                                              }
{ Contributors:                                                                }
{ Uwe Schuster (uschuster)                                                     }
{                                                                              }
{******************************************************************************}

unit SvnClientOptionsFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, ComCtrls, StdCtrls, ExtCtrls;

type
  TSvnOptionsFrame = class(TFrame)
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    cboxConflicted: TColorBox;
    cboxAdded: TColorBox;
    cboxDeleted: TColorBox;
    cboxMerged: TColorBox;
    cboxModified: TColorBox;
    cbStatusColorsEnabled: TCheckBox;
    GroupBox2: TGroupBox;
    cbDeleteBackupFilesAfterCommit: TCheckBox;
    cbAlternativeCommitLayout: TCheckBox;
    GroupBox5: TGroupBox;
    IgnoreEOL: TCheckBox;
    CompareSpace: TRadioButton;
    IgnoreSpace: TRadioButton;
    IgnoreAllSpace: TRadioButton;
    cbClearFileStatesAfterCloseAll: TCheckBox;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

end.
