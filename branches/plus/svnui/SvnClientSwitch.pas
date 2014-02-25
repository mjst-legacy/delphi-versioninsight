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
{ The Original Code is SvnClientSwitch.pas.                                    }
{                                                                              }
{ The Initial Developer of the Original Code is Uwe Schuster.                  }
{ Portions created by Uwe Schuster are Copyright © 2012 - 2014 Uwe Schuster.   }
{ All Rights Reserved.                                                         }
{                                                                              }
{ Contributors:                                                                }
{ Uwe Schuster (uschuster)                                                     }
{                                                                              }
{******************************************************************************}

unit SvnClientSwitch;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, svn_client;

type
  TBrowseURLCallBack = function(var AURL: string): Boolean;
  TSelectLogRevisionCallBack = function(const AURL: string; var ASelectedRevision: Integer): Boolean of object;

  TSwitchParameter = record
    Path: string;
    URL: string;
    Revision: TSvnRevNum;
    Depth: TSvnDepth;
    DepthIsSticky: Boolean;
    IgnoreExternals: Boolean;
  end;

  TSvnSwitchDialog = class(TForm)
    GroupBox2: TGroupBox;
    RevisionLabel: TLabel;
    CurrentRevision: TCheckBox;
    SelectRevision: TEdit;
    Options: TGroupBox;
    DepthSticky: TCheckBox;
    OmitExternals: TCheckBox;
    Depth: TComboBox;
    Label1: TLabel;
    lbPath: TLabel;
    Label2: TLabel;
    URL: TComboBox;
    BrowseURL: TButton;
    Label3: TLabel;
    Label4: TLabel;
    lbSource: TLabel;
    lbDestination: TLabel;
    btnURLLog: TButton;
    Ok: TButton;
    Cancel: TButton;
    procedure FormCreate(Sender: TObject);
    procedure BrowseURLClick(Sender: TObject);
    procedure URLChange(Sender: TObject);
    procedure CurrentRevisionClick(Sender: TObject);
    procedure btnURLLogClick(Sender: TObject);
  private
    { Private declarations }
    FBrowseURLCallBack: TBrowseURLCallBack;
    FRootURL: string;
    FSelectLogRevisionCallBack: TSelectLogRevisionCallBack;
    function GetParameter: TSwitchParameter;
    procedure UpdateDestinationURL;
  public
    { Public declarations }
    property BrowseURLCallBack: TBrowseURLCallBack read FBrowseURLCallBack write FBrowseURLCallBack;
    property Parameter: TSwitchParameter read GetParameter;
    property RootURL: string read FRootURL write FRootURL;
    property SelectLogRevisionCallBack: TSelectLogRevisionCallBack read FSelectLogRevisionCallBack write FSelectLogRevisionCallBack;
  end;

var
  SvnSwitchDialog: TSvnSwitchDialog;

function GetSwitchInformation(URLHistory: TStringList; const ARootURL, AURL, APath, ANativePath: string; BrowseURLCallBack: TBrowseURLCallBack;
  ASelectLogRevisionCallBack: TSelectLogRevisionCallBack; var AParameter: TSwitchParameter): Boolean;

implementation

uses
  SvnUIConst;

{$R *.dfm}

function GetSwitchInformation(URLHistory: TStringList; const ARootURL, AURL, APath, ANativePath: string; BrowseURLCallBack: TBrowseURLCallBack;
  ASelectLogRevisionCallBack: TSelectLogRevisionCallBack; var AParameter: TSwitchParameter): Boolean;
var
  I: Integer;
  URL, S: string;
  Parameter: TSwitchParameter;
begin
  SvnSwitchDialog := TSvnSwitchDialog.Create(Application);
  SvnSwitchDialog.BrowseURLCallBack := BrowseURLCallBack;
  SvnSwitchDialog.SelectLogRevisionCallBack := ASelectLogRevisionCallBack;
  for I := 0 to URLHistory.Count - 1 do
    if Pos(ARootURL, URLHistory[I]) = 1 then
    begin
      S := URLHistory[I];
      Delete(S, 1, Length(ARootURL));
      SvnSwitchDialog.URL.Items.Add(S);
    end;
  SvnSwitchDialog.RootURL := ARootURL;
  SvnSwitchDialog.lbSource.Caption := AURL;
  SvnSwitchDialog.lbPath.Caption := APath;
  SvnSwitchDialog.URL.Text := Copy(AURL, Length(ARootURL) + 1, MaxInt);
  SvnSwitchDialog.UpdateDestinationURL;
  SvnSwitchDialog.Caption := Format(sSwitchOptionsCaption, [ANativePath]);
  Result := SvnSwitchDialog.ShowModal = mrOk;
  Parameter := SvnSwitchDialog.Parameter;
  URL := Parameter.URL;
  if Result then
    AParameter := Parameter;
  if (URL <> '') and (URLHistory.IndexOf(URL) = -1) then
    URLHistory.Insert(0, URL);
  SvnSwitchDialog.Free;
end;

procedure TSvnSwitchDialog.BrowseURLClick(Sender: TObject);
var
  SURL: string;
begin
  SURL := FRootURL + URL.Text;
  if FBrowseURLCallBack(SURL) and AnsiSameText(FRootURL, Copy(SURL, 1, Length(FRootURL))) then
  begin
    Delete(SURL, 1, Length(FRootURL));
    URL.Text := SURL;
    UpdateDestinationURL;
  end;
end;

procedure TSvnSwitchDialog.btnURLLogClick(Sender: TObject);
var
  Revision: Integer;
begin
  if FSelectLogRevisionCallBack(lbDestination.Caption, Revision) then
  begin
    CurrentRevision.Checked := False;
    SelectRevision.Text := IntToStr(Revision);
  end;
end;

procedure TSvnSwitchDialog.CurrentRevisionClick(Sender: TObject);
begin
  RevisionLabel.Enabled := not CurrentRevision.Checked;
  SelectRevision.Enabled := not CurrentRevision.Checked;
end;

procedure TSvnSwitchDialog.FormCreate(Sender: TObject);
begin
  Constraints.MinHeight := Height;
  Constraints.MinWidth := Width;
  Constraints.MaxHeight := Height;
end;

function TSvnSwitchDialog.GetParameter: TSwitchParameter;
begin
  Result.Path := lbPath.Caption;
  Result.URL := FRootURL + URL.Text;
  if CurrentRevision.Checked then
    Result.Revision := -1
  else if not TryStrToInt(SelectRevision.Text, Result.Revision) then
    Result.Revision := -1;
  case Depth.ItemIndex of
    0: Result.Depth := svnDepthUnknown;
    1: Result.Depth := svnDepthInfinity;
    2: Result.Depth := svnDepthImmediates;
    3: Result.Depth := svnDepthFiles;
    4: Result.Depth := svnDepthEmpty;
    5: Result.Depth := svnDepthExclude;
    else
      Result.Depth := svnDepthEmpty;
  end;
  Result.DepthIsSticky := DepthSticky.Checked;
  Result.IgnoreExternals := OmitExternals.Checked;
end;

procedure TSvnSwitchDialog.UpdateDestinationURL;
begin
  lbDestination.Caption := FRootURL + URL.Text;
end;

procedure TSvnSwitchDialog.URLChange(Sender: TObject);
begin
  UpdateDestinationURL;
end;

end.
