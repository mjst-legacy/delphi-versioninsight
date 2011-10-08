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
{ This unit contains resource strings used by svnui package.                   }
{                                                                              }
{******************************************************************************}

unit SvnUIConst;

interface

resourcestring
  SLoginCaption = 'Please login to Subversion server.';
  SLoginRealmCaption = 'Please login to Subversion server:'#13#10'%s';
  SServerSSLCertCaption = 'Server returned the following SSL server certificate:';
  SServerSSLCertRealmCaption = 'Server %s returned the following SSL server certificate:';
  SServerSSLCertFailures = 'The following errors were encountered when validating the certificate:';
  SServerSSLCertNotYetValid = 'The certificate is not valid yet.';
  SServerSSLCertExpired = 'The certificate has expired.';
  SServerSSLCertHostName = 'The certificate host name does not match the remote host name.';
  SServerSSLCertAuthority = 'The certificate authority is unknown/not trusted.';
  SServerSSLCertOther = 'Other errors.';
  sModified = 'Modified';
  sAdded = 'Added';
  sDeleted = 'Deleted';
  sReplaced = 'Replaced';
  sRevertCheck = 'Are you sure you would like to revert? You will lose all local changes.';
  sRevertDirAddCheck = 'Are you sure you would like to revert? These items will no longer be added to the repository.';
  sRevertDirMoreDirectories = 'and %d additional directories';
  sRevertDirMoreFiles = 'and %d additional files';
  sRunReverseMergeRevision = 'Do you really want to revert the changes of revision %s in %s?';
  sRunReverseMergeToRevision = 'Do you really want to revert all changes in %s and go back to revision %s?';
  sRunFileReverseMergeRevision = 'Do you really want to revert the changes of revision %s in %s?';
  sDestination = 'Destination for checked out files.';
  sSaveRevisionDestination = 'Destination to save selected files of revision %s';
  sConflictsRemaining = 'There are unresolved conflicts remaining.  Do you still wish to close?';
  sLoadRepoContentError = 'Cannot load repository content. Please check the entered URL.';
  sNoFiles = 'No file modifications found.  There is nothing to commit.';
  sBugIDCaption = 'Bug-ID';
  sNoChangeList = '(no changelist)';
  sAddNewChangeList = '<new changelist>';
  sCreateChangeListCaption = 'Create Changelist';
  sCreateChangeListPrompt = 'Enter a name for the changelist:';
  sSelCountTotalCount = '%d files selected, %d files total';

implementation

end.
