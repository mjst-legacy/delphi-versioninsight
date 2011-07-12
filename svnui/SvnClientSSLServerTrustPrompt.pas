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
{ This unit contains SSL server trust prompt dialog for a Subversion client.   }
{                                                                              }
{******************************************************************************}

unit SvnClientSSLServerTrustPrompt;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  svn_client, SvnClient;

type
  TFormSvnClientSSLServerTrustPrompt = class(TForm)
    ButtonCancel: TButton;
    ButtonOK: TButton;
    CheckBoxSave: TCheckBox;
    EditFingerprint: TEdit;
    EditHostName: TEdit;
    EditIssuer: TEdit;
    EditValidFrom: TEdit;
    EditValidUntil: TEdit;
    ImageLogo: TImage;
    LabelCertData: TLabel;
    LabelFailures: TLabel;
    LabelFingerprint: TLabel;
    LabelHostName: TLabel;
    LabelIssuer: TLabel;
    LabelRealm: TLabel;
    LabelTrustPrompt: TLabel;
    LabelValidFrom: TLabel;
    LabelValidUntil: TLabel;
    MemoCertData: TMemo;
    PageControl: TPageControl;
    TabSheetCertificate: TTabSheet;
    TabSheetPrompt: TTabSheet;
    procedure FormCreate(Sender: TObject);
  end;

function ShowSvnClientSSLServerTrustPrompt(SvnClient: TSvnClient; const Realm: string;
  const CertInfo: TSvnAuthSSLServerCertInfo; Failures: TSSLServerTrustFailures;
  var Save: Boolean): TModalResult;

implementation

uses SvnUIConst;

{$R *.dfm}

function ShowSvnClientSSLServerTrustPrompt(SvnClient: TSvnClient; const Realm: string;
  const CertInfo: TSvnAuthSSLServerCertInfo; Failures: TSSLServerTrustFailures;
  var Save: Boolean): TModalResult;
var
  Form: TFormSvnClientSSLServerTrustPrompt;
  Y: Integer;

  procedure CreateLabel(Left, Top: Integer; const Caption: string);
  var
    L: TLabel;
  begin
    L := TLabel.Create(Form);
    L.Parent := Form.TabSheetPrompt;
    L.Left := Left;
    L.Top := Top;
    L.Caption := Caption;
  end;

begin
  Form := TFormSvnClientSSLServerTrustPrompt.Create(nil);
  try
    if Realm = '' then
      Form.LabelRealm.Caption := SServerSSLCertCaption
    else
      Form.LabelRealm.Caption := Format(SServerSSLCertRealmCaption, [Realm]);
    Form.EditHostName.Text := string(UTF8String(CertInfo.HostName));
    Form.EditFingerprint.Text := string(UTF8String(CertInfo.fingerprint));
    Form.EditValidFrom.Text := string(UTF8String(CertInfo.valid_from));
    Form.EditValidUntil.Text := string(UTF8String(CertInfo.valid_until));
    Form.EditIssuer.Text := string(UTF8String(CertInfo.issuer_dname));
    Form.MemoCertData.Text := string(UTF8String(CertInfo.ascii_cert));
    Form.LabelFailures.Caption := SServerSSLCertFailures;
    Form.CheckBoxSave.Checked := Save;
    Y := 40;
    if sslCertNotYetValid in Failures then
    begin
      CreateLabel(8, Y, SServerSSLCertNotYetValid);
      Inc(Y, 16);
    end;
    if sslCertExpired in Failures then
    begin
      CreateLabel(8, Y, SServerSSLCertExpired);
      Inc(Y, 16);
    end;
    if sslCertHostNameMismatch in Failures then
    begin
      CreateLabel(8, Y, SServerSSLCertHostName);
      Inc(Y, 16);
    end;
    if sslCertAuthorityUnknown in Failures then
    begin
      CreateLabel(8, Y, SServerSSLCertAuthority);
      Inc(Y, 16);
    end;
    if sslCertOther in Failures then
    begin
      CreateLabel(8, Y, SServerSSLCertOther);
      Inc(Y, 16);
    end;
    Inc(Y, 16);
    Form.LabelTrustPrompt.Top := Y;
    Form.CheckBoxSave.Top := Y + 24;

    Result := Form.ShowModal;
    if Result = mrOK then
      Save := Form.CheckBoxSave.Checked;
  finally
    Form.Free;
  end;
end;

// TFormSvnClientSSLServerTrustPrompt event handlers
procedure TFormSvnClientSSLServerTrustPrompt.FormCreate(Sender: TObject);
begin
  Constraints.MinHeight := Height;
  Constraints.MinWidth := Width;
end;

end.
