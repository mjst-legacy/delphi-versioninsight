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
{ The Original Code is SvnClientMerge.pas.                                     }
{                                                                              }
{ The Initial Developer of the Original Code is Uwe Schuster.                  }
{ Portions created by Uwe Schuster are Copyright © 2011 - 2014 Uwe Schuster.   }
{ All Rights Reserved.                                                         }
{                                                                              }
{ Contributors:                                                                }
{ Uwe Schuster (uschuster)                                                     }
{                                                                              }
{******************************************************************************}

unit SvnClientMerge;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, svn_client, Generics.Collections;

type
  TBrowseURLCallBack = function(var AURL: string): Boolean;
  TSelectLogRevisionsCallBack = function(const AURL: string; ASelectedRevisions: TList<Integer>): Boolean of object;
  TShowLogCallBack = procedure(const APath: string) of object;

  TMergeKind = (mkRange);

  TMergeParameter = record
    MergeType: TMergeKind;
    URL: string;
    RevisionStr: string;
    ReverseMerge: Boolean;
    TargetWcpath: string;
    Depth: TSvnDepth;
    IgnoreAncestry: Boolean;
    Force: Boolean;
    RecordOnly: Boolean;
    DryRun: Boolean;
    IgnoreEOL: Boolean;
    IgnoreSpace: Boolean;
    IgnoreAllSpace: Boolean;
  end;

  TTestMergeCallBack = procedure(AParameter: TMergeParameter) of object;

  TSvnMergeDialog = class(TForm)
    Panel1: TPanel;
    btnBack: TButton;
    btnNext: TButton;
    Button3: TButton;
    PageControl: TPageControl;
    tsMergeType: TTabSheet;
    tsMergeRevisionRange: TTabSheet;
    GroupBox1: TGroupBox;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    Label1: TLabel;
    GroupBox2: TGroupBox;
    URL: TComboBox;
    BrowseURL: TButton;
    GroupBox3: TGroupBox;
    Revisions: TEdit;
    ReverseMerge: TCheckBox;
    Label2: TLabel;
    GroupBox4: TGroupBox;
    WCDir: TLabel;
    tsMergeOptions: TTabSheet;
    GroupBox5: TGroupBox;
    Label3: TLabel;
    IgnoreAncestry: TCheckBox;
    IgnoreEOL: TCheckBox;
    RadioButton4: TRadioButton;
    IgnoreSpace: TRadioButton;
    IgnoreAllSpace: TRadioButton;
    Force: TCheckBox;
    RecordOnly: TCheckBox;
    Depth: TComboBox;
    Button1: TButton;
    btnURLLog: TButton;
    btnWCLog: TButton;
    procedure btnNextClick(Sender: TObject);
    procedure btnBackClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BrowseURLClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure btnURLLogClick(Sender: TObject);
    procedure btnWCLogClick(Sender: TObject);
  private
    { Private declarations }
    FBrowseURLCallBack: TBrowseURLCallBack;
    FPages: array of TTabSheet;
    FSelectLogRevisionsCallBack: TSelectLogRevisionsCallBack;
    FShowLogCallBack: TShowLogCallBack;
    FTestMergeCallBack: TTestMergeCallBack;
    function IndexOfCurrentPage: Integer;
    procedure UpdateButtons;
    procedure UpdateCaption;
    function GetParameter: TMergeParameter;
  public
    { Public declarations }
    property BrowseURLCallBack: TBrowseURLCallBack read FBrowseURLCallBack write FBrowseURLCallBack;
    property Parameter: TMergeParameter read GetParameter;
    property SelectLogRevisionsCallBack: TSelectLogRevisionsCallBack read FSelectLogRevisionsCallBack write FSelectLogRevisionsCallBack;
    property ShowLogCallBack: TShowLogCallBack read FShowLogCallBack write FShowLogCallBack;
    property TestMergeCallBack: TTestMergeCallBack read FTestMergeCallBack write FTestMergeCallBack;
  end;

var
  SvnMergeDialog: TSvnMergeDialog;

function GetMergeInformation(URLHistory: TStringList; const APath: string; BrowseURLCallBack: TBrowseURLCallBack;
  ATestMergeCallBack: TTestMergeCallBack; ASelectLogRevisionsCallBack: TSelectLogRevisionsCallBack;
  AShowLogCallBack: TShowLogCallBack; var AParameter: TMergeParameter): Boolean;

implementation

uses
  SvnUIConst;

{$R *.dfm}

function GetMergeInformation(URLHistory: TStringList; const APath: string; BrowseURLCallBack: TBrowseURLCallBack;
  ATestMergeCallBack: TTestMergeCallBack; ASelectLogRevisionsCallBack: TSelectLogRevisionsCallBack;
  AShowLogCallBack: TShowLogCallBack; var AParameter: TMergeParameter): Boolean;
var
  URL: string;
  Parameter: TMergeParameter;
begin
  SvnMergeDialog := TSvnMergeDialog.Create(Application);
  SvnMergeDialog.WCDir.Caption := APath;
  SvnMergeDialog.BrowseURLCallBack := BrowseURLCallBack;
  SvnMergeDialog.SelectLogRevisionsCallBack := ASelectLogRevisionsCallBack;
  SvnMergeDialog.ShowLogCallBack := AShowLogCallBack;
  SvnMergeDialog.TestMergeCallBack := ATestMergeCallBack;
  SvnMergeDialog.URL.Items.Assign(URLHistory);
  Result := SvnMergeDialog.ShowModal = mrOk;
  Parameter := SvnMergeDialog.Parameter;
  URL := Parameter.URL;
  if Result then
    AParameter := Parameter;
  if (URL <> '') and (URLHistory.IndexOf(URL) = -1) then
    URLHistory.Insert(0, URL);
  SvnMergeDialog.Free;
end;

procedure TSvnMergeDialog.BrowseURLClick(Sender: TObject);
var
  SURL: string;
begin
  SURL := URL.Text;
  if FBrowseURLCallBack(SURL) then
  begin
    URL.Text := SURL;
    UpdateButtons;
  end;
end;

procedure TSvnMergeDialog.btnBackClick(Sender: TObject);
var
  Idx: Integer;
begin
  Idx := IndexOfCurrentPage;
  if Idx > Low(FPages) then
    PageControl.ActivePage := FPages[Idx - 1];
  UpdateButtons;
  UpdateCaption;
end;

procedure TSvnMergeDialog.btnNextClick(Sender: TObject);
var
  Idx: Integer;
begin
  Idx := IndexOfCurrentPage;
  if Idx < High(FPages) then
    PageControl.ActivePage := FPages[Idx + 1];
  UpdateButtons;
  UpdateCaption;
end;

procedure TSvnMergeDialog.btnURLLogClick(Sender: TObject);

  function AddToRevStr(AStr: string; AFirstRev, ALastRev: Integer): string;
  var
    S: string;
  begin
    if AFirstRev = ALastRev then
      S := IntToStr(AFirstRev)
    else
      S := IntToStr(AFirstRev) + '-' + IntToStr(ALastRev);
    if AStr <> '' then
      Result := AStr + ',' + S
    else
      Result := S;
  end;

var
  RevisionList: TList<Integer>;
  I, FirstRev, LastRev: Integer;
  S: string;
begin
  RevisionList := TList<Integer>.Create;
  try
    if FSelectLogRevisionsCallBack(URL.Text, RevisionList) then
    begin
      S := '';
      FirstRev := -1;
      LastRev := -1;
      for I := 0 to Pred(RevisionList.Count) do
      begin
        if (FirstRev <> -1) and (RevisionList[I] = LastRev + 1) then
          Inc(LastRev)
        else
        begin
          if FirstRev <> -1 then
            S := AddToRevStr(S, FirstRev, LastRev);
          FirstRev := RevisionList[I];
          LastRev := FirstRev;
        end;
      end;
      if FirstRev <> -1 then
        S := AddToRevStr(S, FirstRev, LastRev);
      Revisions.Text := S;
    end;
  finally
    RevisionList.Free;
  end;
end;

procedure TSvnMergeDialog.btnWCLogClick(Sender: TObject);
begin
  FShowLogCallBack(WCDir.Caption);
end;

procedure TSvnMergeDialog.Button1Click(Sender: TObject);
var
  TestParameter: TMergeParameter;
begin
  TestParameter := Parameter;
  TestParameter.DryRun := True;
  FTestMergeCallBack(TestParameter);
end;

procedure TSvnMergeDialog.FormCreate(Sender: TObject);
begin
  SetLength(FPages, 3);
  FPages[0] := tsMergeType;
  FPages[1] := tsMergeRevisionRange;
  FPages[2] := tsMergeOptions;
  PageControl.ActivePage := FPages[Low(FPages)];
  UpdateButtons;
  UpdateCaption;
  Constraints.MinHeight := Height;
  Constraints.MinWidth := Width;
end;

function TSvnMergeDialog.GetParameter: TMergeParameter;
begin
  Result.MergeType := mkRange;
  Result.URL := URL.Text;
  Result.RevisionStr := Revisions.Text;
  Result.ReverseMerge := ReverseMerge.Checked;
  Result.TargetWcpath := WCDir.Caption;
  case Depth.ItemIndex of
    0: Result.Depth := svnDepthUnknown;
    1: Result.Depth := svnDepthInfinity;
    2: Result.Depth := svnDepthImmediates;
    3: Result.Depth := svnDepthFiles;
    4: Result.Depth := svnDepthEmpty;
    else
      Result.Depth := svnDepthEmpty;
  end;
  Result.IgnoreAncestry := IgnoreAncestry.Checked;
  Result.Force := Force.Checked;
  Result.RecordOnly := RecordOnly.Checked;
  Result.DryRun := False;
  Result.IgnoreEOL := IgnoreEOL.Checked;
  Result.IgnoreSpace := IgnoreSpace.Checked;
  Result.IgnoreAllSpace := IgnoreAllSpace.Checked;
end;

function TSvnMergeDialog.IndexOfCurrentPage: Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := Low(FPages) to High(FPages) do
    if FPages[I] = PageControl.ActivePage then
    begin
      Result := I;
      Break;
    end;
end;

procedure TSvnMergeDialog.UpdateButtons;
begin
  btnBack.Enabled := IndexOfCurrentPage <> Low(FPages);
  if IndexOfCurrentPage = High(FPages) then
  begin
    btnNext.Caption := sMergeCaption;
    btnNext.ModalResult := mrOk;
    btnNext.Enabled := Trim(URL.Text) <> '';
  end
  else
  begin
    btnNext.Caption := sNextCaption;
    btnNext.ModalResult := mrNone;
    btnNext.Enabled := True;
  end;
  btnURLLog.Enabled := Assigned(FSelectLogRevisionsCallBack);
  btnWCLog.Enabled := Assigned(FShowLogCallBack);
end;

procedure TSvnMergeDialog.UpdateCaption;
begin
  Caption := PageControl.ActivePage.Caption;
end;

end.
