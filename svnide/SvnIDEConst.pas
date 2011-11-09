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
{                                                                              }
{ This unit contains resource strings used by svnide package.                  }
{                                                                              }
{******************************************************************************}

unit SvnIDEConst;

interface

resourcestring
  sLoadError = 'Can not load DLL';
  sSubversion = 'Subversion';
  sPMMCommit = 'Commit';
  sPMMUpdate = 'Update';
  sPMMClean = 'Clean';
  sPMMSvnParent = 'Subversion';
  sPMMLog = 'Show Log';
  sPMMRootDir = 'From Repository Root';
  sPMMProjectDir = 'From Project Directory';
  sPMMExpicitFiles = 'Files in this Project';
  sPMMRepo = 'Browse Repository';
  sPMMMerge = 'Merge';
  sMenuAddToVersionControl = 'Subversion Import';
  sMenuOpenFromVersionControl = 'Open From Subversion (Checkout)';

  sCommit = 'Commit';
  sUpdated = 'Updated';
  sUpdateCompletedAtRevision = 'At Revision: %d';
  sCommited = 'Commited';
  sCommitCompleted = 'Commit completed at revision: %d';
  sCommitLoaded = 'A commit window is still open. Please close it if you wish to start a new commit.';
  sNeedToClean = 'Run Subversion Clean to correct problem.';
  sRunClean = 'Would you like to run Subversion Clean?';
  sCleaning = 'Cleaning ';
  sLog = 'Log';
  sImport = 'Import';
  sFilesUnderDir = 'All files under %s will be committed';
  sWorking = '-Working';
  sRepoBrowser = 'Repository Browser';
  sVersionControlAddInOptionArea = 'Version Control';
  sMergeDialogCaption = 'Merge revisions %s - %s of %s into %s';
  sMergeDialogCaptionRange = 'Merge revisions %s of %s into %s';
  sHead = 'HEAD';
  sRetrievingFileRevision = 'Retrieving %s revision %d';
  sSavingFileRevision = 'Saving %s revision %d';

implementation

end.
