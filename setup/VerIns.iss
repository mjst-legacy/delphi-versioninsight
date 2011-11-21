[Setup]
; -----------------------------------------------------------------------------
; HEADER
; -----------------------------------------------------------------------------

#define BUILDNO  'Beta7_1'

OutputBaseFilename=.\VerInsPlus{#BUILDNO}
AppName=Version Insight Plus
AppVerName=Version Insight Plus
DefaultDirName={pf}\VersionInsight
DefaultGroupName=Version Insight
LicenseFile=License.rtf
Compression=lzma
SolidCompression=true
AllowRootDirectory=false
AlwaysShowDirOnReadyPage=true
ChangesAssociations=true
EnableDirDoesntExistWarning=true
AppSupportURL=http://www.sf.net/projects/radstudioverins
AppUpdatesURL=http://www.sf.net/projects/radstudioverins
AppCopyright=Released under MPL 1.1
AppID=VerIns
ShowLanguageDialog=yes

[Registry]
  Root: HKLM; Subkey: "Software\VersionInsight"; ValueType: string; ValueName: "InstallPath"; ValueData: "{app}"

[Types]
  Name: FULL;   Description: Full installation
  Name: CUSTOM; Description: Custom installation; Flags: IsCustom

[Components]
  Name: VERINS;               Description: Version Insight;             Types: FULL CUSTOM
  Name: VERINS\DSNIDEPRO;     Description: DesignIDEPro IDE Package;    Types: FULL CUSTOM

[Tasks]
  Name: "IDE_REGISTER";            Description: "Register Version Insight"; components: VERINS;             Flags: checkedonce Exclusive; check: GetBDS8_UPDetected('8.0') or GetBDS8_UPDetected('9.0')
  Name: "IDE_REGISTER\IDE";        Description: "IDE Integration";          components: VERINS;                                           check: GetBDS8_UPDetected('8.0') or GetBDS8_UPDetected('9.0')
  Name: "IDE_REGISTER\IDE\XE";     Description: "RAD Studio XE";            components: VERINS;                                           check: GetBDS8_UPDetected('8.0')
  Name: "IDE_REGISTER\IDE\XE2";    Description: "RAD Studio XE2";           components: VERINS;                                           check: GetBDS8_UPDetected('9.0')
  Name: "IDE_REGISTER\DSNIDEPRO";  Description: "Register DesignIDEPro";    components: VERINS\DSNIDEPRO;                                 check: GetBDS8_UPDetected('8.0') or GetBDS8_UPDetected('9.0')
  Name: "IDE_REGISTER\GIT";        Description: "Enable Git";               components: VERINS;                                           check: GetBDS8_UPDetected('8.0') or GetBDS8_UPDetected('9.0')
  Name: "IDE_REGISTER\HG";         Description: "Enable Mercurial";         components: VERINS;                                           check: GetBDS8_UPDetected('8.0') or GetBDS8_UPDetected('9.0')
  Name: "IDE_REGISTER\SVN";        Description: "Enable Subversion";        components: VERINS;                                           check: GetBDS8_UPDetected('8.0') or GetBDS8_UPDetected('9.0')
  Name: "IDE_REGISTER\VERINSOFF";  Description: "Deinstall Embarcadero Subversion Integration"; components: VERINS;                       check: VersionInsightOfficeExists('8.0') or VersionInsightOfficeExists('9.0')

[Files]
  Source: tortoisesvn_icon_license.txt;                       DestDir: {app};            Components: VERINS;               Flags: ignoreversion overwritereadonly 
  Source: ..\bin\DelphiSVN150.dll;                            DestDir: {app};            Components: VERINS;               Flags: ignoreversion overwritereadonly
  Source: ..\bin\DelphiSVN160.dll;                            DestDir: {app};            Components: VERINS;               Flags: ignoreversion overwritereadonly
  Source: ..\designidepro\bin\DesignIDEPro150.bpl;            DestDir: {app};            Components: VERINS\DSNIDEPRO;     Flags: ignoreversion overwritereadonly
  Source: ..\designidepro\bin\DesignIDEPro160.bpl;            DestDir: {app};            Components: VERINS\DSNIDEPRO;     Flags: ignoreversion overwritereadonly

[Run]

[Dirs]
; Name: {app}\Source

[Code]
var
  GitFilePage: TInputFileWizardPage;  
  HgFilePage: TInputFileWizardPage;
  
procedure InitializeWizard;
begin
  { Create the pages }

  GitFilePage := CreateInputFilePage(wpSelectTasks,
    'Select Git Location', 'Where is Git.exe located?',
    'Select where Git.exe is located, then click Next.');
  GitFilePage.Add('', 'Git Executable (git.exe)|git.exe', '.exe');

  HgFilePage := CreateInputFilePage(wpSelectTasks,
    'Select Mercurial Location', 'Where is Hg.exe located?',
    'Select where Hg.exe is located, then click Next.');
  HgFilePage.Add('', 'Mercurial Executable (hg.exe)|hg.exe', '.exe');
end;

function ShouldSkipPage(PageID: Integer): Boolean;
var
  sSelTasks: string;
begin
  sSelTasks := AnsiUpperCase(WizardSelectedTasks(False));
  { Skip pages that shouldn't be shown }
  if (PageID = GitFilePage.ID) and (Pos('IDE_REGISTER\GIT', sSelTasks) = 0) then
    Result := True
  else if (PageID = HgFilePage.ID) and (Pos('IDE_REGISTER\HG', sSelTasks) = 0) then
    Result := True
  else
    Result := False;
end;

function NextButtonClick(CurPageID: Integer): Boolean;
begin
  { Validate certain pages before allowing the user to proceed }
  if CurPageID = GitFilePage.ID then 
  begin
    Result := FileExists(GitFilePage.Values[0]);
    if not Result then
      MsgBox('You must enter a valid filename.', mbError, MB_OK);
  end
  else
  if CurPageID = HgFilePage.ID then 
  begin
    Result := FileExists(HgFilePage.Values[0]);
    if not Result then
      MsgBox('You must enter a valid filename.', mbError, MB_OK);
  end
  else
    Result := True;
end;

{ =============================================================================
  Purpose  : Detect installed Embarcadero RAD Studio version
  Parameter: sVersion[8.0]
}
function GetBDS8_UPDetected(const sVersion: string): Boolean;
begin
  Result := RegKeyExists(HKCU, 'Software\Embarcadero\BDS\' + sVersion);
end;

{ =============================================================================
  Purpose  : Get Embarcadero's Version Insight package name
  Parameter: sVersion[8.0]
}
function GetVersionInsightOfficePackageName(const sVersion: string): string;
var
  Names: TArrayOfString;
  I: Integer;
begin
  Result := '';
  if RegGetValueNames(HKCU,  'Software\Embarcadero\BDS\' + sVersion + '\Known Packages', Names) then
    for I := 0 to GetArrayLength(Names)-1 do
      if (Pos('SVNIDE150.BPL', AnsiUpperCase(Names[I])) > 0) or
        (Pos('SVNIDE160.BPL', AnsiUpperCase(Names[I])) > 0) then
      begin
        Result := Names[I];
        Break; 
      end;
end;

{ =============================================================================
  Purpose  : Detect if Embarcadero's Version Insight is installed
  Parameter: sVersion[8.0]
}
function VersionInsightOfficeExists(const sVersion: string): Boolean;
begin
  Result := GetVersionInsightOfficePackageName(sVersion) <> '';
end;

function GetSystemExecutableName(const sVersion, sSystem: string): string;
begin
  if (not RegQueryStringValue (HKCU, 'Software\Embarcadero\BDS\' + sVersion + '\VersionInsight\' + sSystem, 'Executable', Result)) or
    ((Result <> '') and not FileExists(Result)) then
    Result := '';
end;

function GetSystemExecutableNames(const sSystem: string; AFiles: TStringList): Integer;
var
  sSelTasks, FileName: string;
begin
  sSelTasks := AnsiUpperCase(WizardSelectedTasks(False));
  if Pos('IDE_REGISTER\IDE\XE', sSelTasks) > 0 then
  begin
    FileName := GetSystemExecutableName('8.0', sSystem);
    if (FileName <> '') and (AFiles.IndexOf(FileName) = -1) then
      AFiles.Add(FileName);
  end;
  if Pos('IDE_REGISTER\IDE\XE2', sSelTasks) > 0 then
  begin
    FileName := GetSystemExecutableName('9.0', sSystem);
    if (FileName <> '') and (AFiles.IndexOf(FileName) = -1) then
      AFiles.Add(FileName);
  end;
  Result := AFiles.Count;
end;

function SetSystemExecutableName(const sSystem: string): Boolean;
var
  Cnt: Integer;
  Files: TStringList;
begin
  Files := TStringList.Create;
  try
    if sSystem = 'Git' then
    begin
      Cnt := GetSystemExecutableNames(sSystem, Files);
      if (Cnt > 0) and (GitFilePage.Values[0] = '') then
        GitFilePage.Values[0] := Files[0];
      Result := (Cnt = 1) and (GitFilePage.Values[0] <> '');
    end
    else
    if sSystem = 'Mercurial' then
    begin
      Cnt := GetSystemExecutableNames(sSystem, Files);
      if (Cnt > 0) and (HgFilePage.Values[0] = '') then
        HgFilePage.Values[0] := Files[0];
      Result := (Cnt = 1) and (HgFilePage.Values[0] <> '');
    end
    else
      Result := False;
  finally
    Files.Free; 
  end;
end;

{ =============================================================================
  Purpose  : InnoSetup event handler which is called on change to another
             wizard page
}
procedure CurPageChanged(CurPage: Integer);
var
  sSelTasks: string;
  sIDEDll: string;
begin
  case CurPage of
  wpSelectDir:
    begin
    end;
  wpFinished:  // setup is finished, finalizing tasks
    begin
      sSelTasks := AnsiUpperCase(WizardSelectedTasks(False));
      // =========================================
      // Register Delphi (5,6,7) Plugin IDE wizard
      // =========================================
      if Pos('IDE_REGISTER\IDE\XE', sSelTasks) > 0 then
      begin
        sIDEDll := AddBackSlash(ExpandConstant('{app}')) + 'DelphiSVN150.dll';
        RegWriteStringValue (HKCU, 'Software\Embarcadero\BDS\8.0\Experts', 'VersionInsight', sIDEDll);
      end;
      if Pos('IDE_REGISTER\IDE\XE', sSelTasks) > 0 then
      begin      
      if Pos('IDE_REGISTER\DSNIDEPRO', sSelTasks) > 0 then
      begin
        sIDEDll := AddBackSlash(ExpandConstant('{app}')) + 'DesignIDEPro150.bpl';
        RegWriteStringValue (HKCU, 'Software\Embarcadero\BDS\8.0\Known IDE Packages', sIDEDll, '(Untitled)');
      end;
      if Pos('IDE_REGISTER\GIT', sSelTasks) > 0 then
      begin
        RegWriteStringValue (HKCU, 'Software\Embarcadero\BDS\8.0\VersionInsight\Git', 'Enabled', '1');
        RegWriteStringValue (HKCU, 'Software\Embarcadero\BDS\8.0\VersionInsight\Git', 'Executable', GitFilePage.Values[0]);        
      end
      else
        RegWriteStringValue (HKCU, 'Software\Embarcadero\BDS\8.0\VersionInsight\Git', 'Enabled', '0');
      if Pos('IDE_REGISTER\HG', sSelTasks) > 0 then
      begin
        RegWriteStringValue (HKCU, 'Software\Embarcadero\BDS\8.0\VersionInsight\Mercurial', 'Enabled', '1');
        RegWriteStringValue (HKCU, 'Software\Embarcadero\BDS\8.0\VersionInsight\Mercurial', 'Executable', HgFilePage.Values[0]);        
      end
      else
        RegWriteStringValue (HKCU, 'Software\Embarcadero\BDS\8.0\VersionInsight\Mercurial', 'Enabled', '0');
      if Pos('IDE_REGISTER\SVN', sSelTasks) > 0 then
        RegWriteStringValue (HKCU, 'Software\Embarcadero\BDS\8.0\VersionInsight\Subversion', 'Enabled', '1')
      else
        RegWriteStringValue (HKCU, 'Software\Embarcadero\BDS\8.0\VersionInsight\Subversion', 'Enabled', '0');
      if Pos('IDE_REGISTER\VERINSOFF', sSelTasks) > 0 then
        RegDeleteValue (HKCU, 'Software\Embarcadero\BDS\8.0\Known Packages', GetVersionInsightOfficePackageName('8.0'));
      end;
      
      if Pos('IDE_REGISTER\IDE\XE2', sSelTasks) > 0 then
      begin
        sIDEDll := AddBackSlash(ExpandConstant('{app}')) + 'DelphiSVN160.dll';
        RegWriteStringValue (HKCU, 'Software\Embarcadero\BDS\9.0\Experts', 'VersionInsight', sIDEDll);
      end;
      if Pos('IDE_REGISTER\IDE\XE2', sSelTasks) > 0 then
      begin
      if Pos('IDE_REGISTER\DSNIDEPRO', sSelTasks) > 0 then
      begin
        sIDEDll := AddBackSlash(ExpandConstant('{app}')) + 'DesignIDEPro160.bpl';
        RegWriteStringValue (HKCU, 'Software\Embarcadero\BDS\9.0\Known IDE Packages', sIDEDll, '(Untitled)');
      end;
      if Pos('IDE_REGISTER\GIT', sSelTasks) > 0 then
      begin
        RegWriteStringValue (HKCU, 'Software\Embarcadero\BDS\9.0\VersionInsight\Git', 'Enabled', '1');
        RegWriteStringValue (HKCU, 'Software\Embarcadero\BDS\9.0\VersionInsight\Git', 'Executable', GitFilePage.Values[0]);        
      end
      else
        RegWriteStringValue (HKCU, 'Software\Embarcadero\BDS\9.0\VersionInsight\Git', 'Enabled', '0');
      if Pos('IDE_REGISTER\HG', sSelTasks) > 0 then
      begin
        RegWriteStringValue (HKCU, 'Software\Embarcadero\BDS\9.0\VersionInsight\Mercurial', 'Enabled', '1');
        RegWriteStringValue (HKCU, 'Software\Embarcadero\BDS\9.0\VersionInsight\Mercurial', 'Executable', HgFilePage.Values[0]);        
      end
      else
        RegWriteStringValue (HKCU, 'Software\Embarcadero\BDS\9.0\VersionInsight\Mercurial', 'Enabled', '0');
      if Pos('IDE_REGISTER\SVN', sSelTasks) > 0 then
        RegWriteStringValue (HKCU, 'Software\Embarcadero\BDS\9.0\VersionInsight\Subversion', 'Enabled', '1')
      else
        RegWriteStringValue (HKCU, 'Software\Embarcadero\BDS\9.0\VersionInsight\Subversion', 'Enabled', '0');
      if Pos('IDE_REGISTER\VERINSOFF', sSelTasks) > 0 then
        RegDeleteValue (HKCU, 'Software\Embarcadero\BDS\9.0\Known Packages', GetVersionInsightOfficePackageName('9.0'));
      end;        
    end;  // curPage = csTerminate
  else
    if CurPage = GitFilePage.ID then
      SetSystemExecutableName('Git')
    else
    if CurPage = HgFilePage.ID then
      SetSystemExecutableName('Mercurial');
  end;  // case
end;

procedure CurUninstallStepChanged(CurUninstallStep: TUninstallStep);
var
  sIDEDll: string;
begin
  case CurUninstallStep of
    usAppMutexcheck:
      begin
      end;
    usUninstall:
      begin
        RegDeleteValue(HKEY_CURRENT_USER, 'Software\Embarcadero\BDS\8.0\Experts', 'VersionInsight');
        sIDEDll := AddBackSlash(ExpandConstant('{app}')) + 'DesignIDEPro150.bpl';
        RegDeleteValue(HKEY_CURRENT_USER, 'Software\Embarcadero\BDS\8.0\Known IDE Packages', sIDEDll);
        RegDeleteValue(HKEY_CURRENT_USER, 'Software\Embarcadero\BDS\9.0\Experts', 'VersionInsight');
        sIDEDll := AddBackSlash(ExpandConstant('{app}')) + 'DesignIDEPro160.bpl';
        RegDeleteValue(HKEY_CURRENT_USER, 'Software\Embarcadero\BDS\9.0\Known IDE Packages', sIDEDll);
      end;
    usPostUninstall:
      begin
      end;
    usDone:
      begin
      end;
  end;
end;
