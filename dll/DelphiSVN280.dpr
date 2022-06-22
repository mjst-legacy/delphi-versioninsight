library DelphiSVN280;

uses
  Classes,
  ToolsAPI,
  Registry,
  DiffUnit in '..\liveblame\textdiff\DiffUnit.pas',
  HashUnit in '..\liveblame\textdiff\HashUnit.pas',
  VerInsClientProgress in '..\liveblame\VerInsClientProgress.pas' {VerInsProgressDialog},
  VerInsIDETypes in '..\liveblame\VerInsIDETypes.pas',
  VerInsLiveBlame in '..\liveblame\VerInsLiveBlame.pas',
  VerInsIDEBlameAddInOptions in '..\liveblame\VerInsIDEBlameAddInOptions.pas',
  VerInsBlameOptionsFrame in '..\liveblame\VerInsBlameOptionsFrame.pas' {frmVerInsBlameOptions: TFrame},
  VerInsBlameSettings in '..\liveblame\VerInsBlameSettings.pas',
  VerInsLiveBlameTypes in '..\liveblame\VerInsLiveBlameTypes.pas',
  VerInsIDEDockInfo in '..\liveblame\VerInsIDEDockInfo.pas' {fmLiveBlameInfo},
  VerInsIDEBlameDesignerOverlayForm in '..\liveblame\VerInsIDEBlameDesignerOverlayForm.pas' {fmLiveBlameDesignerOverlay};


var
  LiveBlameWizardIndex: Integer = -1;


procedure WizardTerminate();
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
begin
  LiveBlameWizardIndex := VerInsLiveBlame.RegisterExpert;
  Terminate := WizardTerminate;
  Result := True;
end;




exports
  DelphiSVNWizardInit name WizardEntryPoint;




begin


end.

