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
{ The Initial Developer of the Original Code is Ondrej Kelle.                  }
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
{                                                                              }
{ This unit contains SSL certificate prompt dialog for a Subversion client.    }
{                                                                              }
{******************************************************************************}

unit SvnClientSSLClientCertPrompt;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  SvnClient;

type
  TFormSvnClientSSLClientCertPrompt = class(TForm)
    ButtonBrowse: TButton;
    ButtonCancel: TButton;
    ButtonOK: TButton;
    CheckBoxSave: TCheckBox;
    EditCertFileName: TEdit;
    LabelCertFileName: TLabel;
    LabelRealm: TLabel;
    OpenDialog: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure ButtonBrowseClick(Sender: TObject);
    procedure EditChange(Sender: TObject);
  private
  public
  end;

function ShowSvnClientSSLClientCertPrompt(SvnClient: TSvnClient; const Realm: string;
  var CertFileName: string; var Save: Boolean): TModalResult;

implementation

{$R *.dfm}

resourcestring
  SCertFileCaption = 'Please provide SSL client certificate';
  SCertFileRealmCaption = 'Please provide SSL client certificate for server:'#13#10'%s';

function ShowSvnClientSSLClientCertPrompt(SvnClient: TSvnClient; const Realm: string;
  var CertFileName: string; var Save: Boolean): TModalResult;
var
  Form: TFormSvnClientSSLClientCertPrompt;
begin
  Form := TFormSvnClientSSLClientCertPrompt.Create(nil);
  try
    if Realm = '' then
      Form.LabelRealm.Caption := SCertFileCaption
    else
      Form.LabelRealm.Caption := Format(SCertFileRealmCaption, [Realm]);
    Form.EditCertFileName.Text := CertFileName;
    Form.EditChange(Form.EditCertFileName);
    Form.CheckBoxSave.Checked := Save;
    if CertFileName = '' then
      Form.ActiveControl := Form.EditCertFileName
    else
      Form.ActiveControl := Form.ButtonOK;

    Result := Form.ShowModal;

    if Result = mrOK then
    begin
      CertFileName := Form.EditCertFileName.Text;
      Save := Form.CheckBoxSave.Checked;
    end;
  finally
    Form.Free;
  end;
end;

// TFormSvnClientSSLClientCertPrompt event handlers

procedure TFormSvnClientSSLClientCertPrompt.FormCreate(Sender: TObject);
begin
  Constraints.MinHeight := Height;
  Constraints.MinWidth := Width;
end;

procedure TFormSvnClientSSLClientCertPrompt.ButtonBrowseClick(Sender: TObject);
begin
  OpenDialog.FileName := EditCertFileName.Text;
  if OpenDialog.Execute then
    EditCertFileName.Text := OpenDialog.FileName;
end;

procedure TFormSvnClientSSLClientCertPrompt.EditChange(Sender: TObject);
begin
  ButtonOK.Enabled := (EditCertFileName.Text <> '');
end;

end.
