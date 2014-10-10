library DelphiSVN210;

uses
  Classes,
  ToolsAPI,
  Registry,
  apr in '..\svn\apr.pas',
  svn_client in '..\svn\svn_client.pas',
  SvnClient in '..\svn\SvnClient.pas',
  svnconst in '..\svn\svnconst.pas',
  SvnTree in '..\svn\SvnTree.pas',
  SvnClientLoginPrompt in '..\svnui\SvnClientLoginPrompt.pas' {FormSvnClientLoginPrompt},
  SvnClientSSLClientCertPrompt in '..\svnui\SvnClientSSLClientCertPrompt.pas' {FormSvnClientSSLClientCertPrompt},
  SvnClientSSLServerTrustPrompt in '..\svnui\SvnClientSSLServerTrustPrompt.pas' {FormSvnClientSSLServerTrustPrompt},
  SvnImages in '..\svnui\SvnImages.pas' {SvnImageModule: TDataModule},
  SvnUIConst in '..\svnui\SvnUIConst.pas',
  SvnClientCommitFrame in '..\svnui\SvnClientCommitFrame.pas' {SvnCommitFrame: TFrame},
  SvnClientLog in '..\svnui\SvnClientLog.pas' {SvnLogFrame: TFrame},
  SvnClientImportFrame in '..\svnui\SvnClientImportFrame.pas' {SvnImportFrame: TFrame},
  SvnClientRecentComments in '..\svnui\SvnClientRecentComments.pas' {Form1},
  SvnClientCheckout in '..\svnui\SvnClientCheckout.pas' {CheckoutDialog},
  SvnClientProjectSelect in '..\svnui\SvnClientProjectSelect.pas' {SvnProjectSelectDialog},
  SvnClientConflict in '..\svnui\SvnClientConflict.pas' {SvnConflictDialog},
  SvnClientUpdate in '..\svnui\SvnClientUpdate.pas' {UpdateDialog},
  SvnClientRepoBrowserFrame in '..\svnui\SvnClientRepoBrowserFrame.pas' {FrmRepoBrowser: TFrame},
  SvnClientRepoBrowserDialog in '..\svnui\SvnClientRepoBrowserDialog.pas' {dlgRepoBrowser},
  SvnUITypes in '..\svnui\SvnUITypes.pas',
  SvnUIUtils in '..\svnui\SvnUIUtils.pas',
  SvnClientOptionsFrame in '..\svnui\SvnClientOptionsFrame.pas' {SvnOptionsFrame: TFrame},
  SvnClientProgress in '..\svnui\SvnClientProgress.pas' {SvnProgressDialog},
  SvnClientRangeSelect in '..\svnui\SvnClientRangeSelect.pas' {SvnRangeSelectDialog},
  SvnClientEditComment in '..\svnui\SvnClientEditComment.pas' {EditCommentDialog},
  SvnClientLogDialog in '..\svnui\SvnClientLogDialog.pas' {SvnLogDialog},
  SvnClientMerge in '..\svnui\SvnClientMerge.pas' {SvnMergeDialog},
  SvnClientSwitch in '..\svnui\SvnClientSwitch.pas' {SvnSwitchDialog},
  SvnIDEClient in '..\svnide\SvnIDEClient.pas',
  SvnIDEHistory in '..\svnide\SvnIDEHistory.pas',
  SvnIDEConst in '..\svnide\SvnIDEConst.pas',
  SvnIDEMenus in '..\svnide\SvnIDEMenus.pas',
  SvnIDENotifier in '..\svnide\SvnIDENotifier.pas',
  SvnIDECommit in '..\svnide\SvnIDECommit.pas',
  SvnIDEUpdate in '..\svnide\SvnIDEUpdate.pas',
  SvnIDEClean in '..\svnide\SvnIDEClean.pas',
  SvnIDELog in '..\svnide\SvnIDELog.pas',
  SvnIDEImport in '..\svnide\SvnIDEImport.pas',
  SvnIDEMessageView in '..\svnide\SvnIDEMessageView.pas',
  SvnIDECheckout in '..\svnide\SvnIDECheckout.pas',
  SvnIDEUtils in '..\svnide\SvnIDEUtils.pas',
  SvnIDERepoBrowser in '..\svnide\SvnIDERepoBrowser.pas',
  SvnIDEAddInOptions in '..\svnide\SvnIDEAddInOptions.pas',
  SvnIDEColors in '..\svnide\SvnIDEColors.pas',
  SvnIDETypes in '..\svnide\SvnIDETypes.pas',
  SvnIDEIcons in '..\svnide\SvnIDEIcons.pas',
  SvnIDEFileStates in '..\svnide\SvnIDEFileStates.pas',
  SvnIDEMerge in '..\svnide\SvnIDEMerge.pas',
  SvnIDERevert in '..\svnide\SvnIDERevert.pas',
  SvnIDESwitch in '..\svnide\SvnIDESwitch.pas',
  HgClient in '..\hgide\HgClient.pas',
  HgIDEClient in '..\hgide\HgIDEClient.pas',
  HgIDEHistory in '..\hgide\HgIDEHistory.pas',
  HgAddInOptionsFrame in '..\hgide\HgAddInOptionsFrame.pas' {frmHgTestsOptions: TFrame},
  HgIDEAddInOptions in '..\hgide\HgIDEAddInOptions.pas',
  HgIDEConst in '..\hgide\HgIDEConst.pas',
  HgIDEMenus in '..\hgide\HgIDEMenus.pas',
  HgIDETypes in '..\hgide\HgIDETypes.pas',
  HgIDEUtils in '..\hgide\HgIDEUtils.pas',
  HgUIConst in '..\hgide\HgUIConst.pas',
  HgUITypes in '..\hgide\HgUITypes.pas',
  HgUIUtils in '..\hgide\HgUIUtils.pas',
  HgClientProgress in '..\hgide\HgClientProgress.pas' {HgProgressDialog},
  HgIDELog in '..\hgide\HgIDELog.pas',
  HgClientLog in '..\hgide\HgClientLog.pas' {HgLogFrame: TFrame},
  HgClientRecentComments in '..\hgide\HgClientRecentComments.pas' {HgRecentCommentsDialog},
  HgImages in '..\hgide\HgImages.pas' {HgImageModule: TDataModule},
  HgTree in '..\hgide\HgTree.pas',
  HgClientCommitFrame in '..\hgide\HgClientCommitFrame.pas' {HgCommitFrame: TFrame},
  HgIDECommit in '..\hgide\HgIDECommit.pas',
  HgIDEMessageView in '..\hgide\HgIDEMessageView.pas',
  HgIDECheckout in '..\hgide\HgIDECheckout.pas',
  HgClientCheckout in '..\hgide\HgClientCheckout.pas' {HgCheckoutDialog},
  HgClientProjectSelect in '..\hgide\HgClientProjectSelect.pas' {HgProjectSelectDialog},
  HgClientUpdate in '..\hgide\HgClientUpdate.pas' {HgUpdateDialog},
  VerInsResources in '..\verinsmisc\VerInsResources.pas',
  HgIDEIcons in '..\hgide\HgIDEIcons.pas',
  HgIDEFileStates in '..\hgide\HgIDEFileStates.pas',
  HgIDEColors in '..\hgide\HgIDEColors.pas',
  HgIDERevert in '..\hgide\HgIDERevert.pas',
  GitIDEHistory in '..\gitide\GitIDEHistory.pas',
  GitIDEClient in '..\gitide\GitIDEClient.pas',
  GitClient in '..\gitide\GitClient.pas',
  GitIDEAddInOptions in '..\gitide\GitIDEAddInOptions.pas',
  GitAddInOptionsFrame in '..\gitide\GitAddInOptionsFrame.pas' {frmGitTestsOptions: TFrame},
  GitIDEConst in '..\gitide\GitIDEConst.pas',
  GitIDEMenus in '..\gitide\GitIDEMenus.pas',
  GitIDEIcons in '..\gitide\GitIDEIcons.pas',
  GitIDEFileStates in '..\gitide\GitIDEFileStates.pas',
  GitUIConst in '..\gitide\GitUIConst.pas',
  GitIDECheckout in '..\gitide\GitIDECheckout.pas',
  GitClientCheckout in '..\gitide\GitClientCheckout.pas' {GitCheckoutDialog},
  GitClientProjectSelect in '..\gitide\GitClientProjectSelect.pas' {GitProjectSelectDialog},
  GitImages in '..\gitide\GitImages.pas' {GitImageModule: TDataModule},
  GitClientUpdate in '..\gitide\GitClientUpdate.pas' {GitUpdateDialog},
  GitClientCommitFrame in '..\gitide\GitClientCommitFrame.pas' {GitCommitFrame: TFrame},
  GitClientRecentComments in '..\gitide\GitClientRecentComments.pas' {GitRecentCommentsDialog},
  GitIDECommit in '..\gitide\GitIDECommit.pas',
  GitIDEMessageView in '..\gitide\GitIDEMessageView.pas',
  GitIDEUtils in '..\gitide\GitIDEUtils.pas',
  GitTree in '..\gitide\GitTree.pas',
  GitUITypes in '..\gitide\GitUITypes.pas',
  GitUIUtils in '..\gitide\GitUIUtils.pas',
  GitIDELog in '..\gitide\GitIDELog.pas',
  GitClientLog in '..\gitide\GitClientLog.pas' {GitLogFrame: TFrame},
  GitIDETypes in '..\gitide\GitIDETypes.pas',
  GitClientProgress in '..\gitide\GitClientProgress.pas' {GitProgressDialog},
  GitIDEColors in '..\gitide\GitIDEColors.pas',
  GitIDERevert in '..\gitide\GitIDERevert.pas',
  VerInsAddInOptionsFrame in '..\verinsmisc\VerInsAddInOptionsFrame.pas' {frmVerInsOptions: TFrame},
  VerInsIDEAddInOptions in '..\verinsmisc\VerInsIDEAddInOptions.pas',
  DiffUnit in '..\liveblame\textdiff\DiffUnit.pas',
  HashUnit in '..\liveblame\textdiff\HashUnit.pas',
  VerInsClientProgress in '..\liveblame\VerInsClientProgress.pas' {VerInsProgressDialog},
  VerInsIDETypes in '..\liveblame\VerInsIDETypes.pas',
  VerInsLiveBlame in '..\liveblame\VerInsLiveBlame.pas',
  VerInsIDEBlameAddInOptions in '..\liveblame\VerInsIDEBlameAddInOptions.pas',
  VerInsBlameOptionsFrame in '..\liveblame\VerInsBlameOptionsFrame.pas' {frmVerInsBlameOptions: TFrame},
  VerInsBlameSettings in '..\liveblame\VerInsBlameSettings.pas',
  VerInsIDEUtils in '..\verinsmisc\VerInsIDEUtils.pas',
  VerInsIDEMacros in '..\verinsmisc\VerInsIDEMacros.pas',
  VerInsLiveBlameTypes in '..\liveblame\VerInsLiveBlameTypes.pas',
  VerInsIDEDockInfo in '..\liveblame\VerInsIDEDockInfo.pas' {fmLiveBlameInfo};

{$R *.res}

var
  LiveBlameWizardIndex: Integer = -1;

procedure WizardTerminate;
begin
  if LiveBlameWizardIndex <> -1 then
  begin
    (BorlandIDEServices as IOTAWizardServices).RemoveWizard(LiveBlameWizardIndex);
    LiveBlameWizardIndex := -1;
  end;
end;

function DelphiSVNWizardInit(const BorlandIDEServices: IBorlandIDEServices;
  RegisterProc: TWizardRegisterProc;
  var Terminate: TWizardTerminateProc): Boolean; stdcall;
var
  RegIniFile: TRegIniFile;
  Key: string;
begin
  VerInsIDEAddInOptions.RegisterAddInOptions;
  Key := (BorlandIDEServices as IOTAServices).GetBaseRegistryKey + '\VersionInsight';
  RegIniFile := TRegIniFile.Create(Key);
  try
    if RegIniFile.ReadBool('Git', 'Enabled', True) then
      GitIDEClient.Register;
    if RegIniFile.ReadBool('Mercurial', 'Enabled', True) then
      HgIDEClient.Register;
    if RegIniFile.ReadBool('Subversion', 'Enabled', True) then
      SvnIDEClient.Register;
    LiveBlameWizardIndex := VerInsLiveBlame.RegisterExpert;
  finally
    RegIniFile.Free;
  end;
  Terminate := WizardTerminate;
  Result := True;
end;

exports
  DelphiSVNWizardInit name WizardEntryPoint;

begin
end.
