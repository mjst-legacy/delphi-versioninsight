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
{ The Original Code is SvnClientProgress.pas.                                  }
{                                                                              }
{ The Initial Developer of the Original Code is Uwe Schuster.                  }
{ Portions created by Uwe Schuster are Copyright © 2010 - 2014 Uwe Schuster.   }
{ All Rights Reserved.                                                         }
{                                                                              }
{ Contributors:                                                                }
{ Uwe Schuster (uschuster)                                                     }
{                                                                              }
{******************************************************************************}

unit HgClientProgress;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls;

type
  TAbortCallBack = procedure of object;

  THgProgressDialog = class(TForm)
    btnAbort: TButton;
    ProgressBar1: TProgressBar;
    lbInfo: TLabel;
    procedure btnAbortClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FAbortCallBack: TAbortCallBack;
  public
    { Public declarations }
    property AbortCallBack: TAbortCallBack read FAbortCallBack write FAbortCallBack;
  end;

var
  HgProgressDialog: THgProgressDialog;

implementation

{$R *.dfm}

procedure THgProgressDialog.btnAbortClick(Sender: TObject);
begin
  FAbortCallBack;
end;

procedure THgProgressDialog.FormCreate(Sender: TObject);
begin
  lbInfo.Caption := '';
end;

end.
