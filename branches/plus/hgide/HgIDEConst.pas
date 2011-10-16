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
{ The Original Code is HgIDEConst.pas.                                         }
{                                                                              }
{ The Initial Developer of the Original Code is Uwe Schuster.                  }
{ Portions created by Uwe Schuster are Copyright © 2011 Uwe Schuster. All      }
{ Rights Reserved.                                                             }
{                                                                              }
{ Contributors:                                                                }
{ Uwe Schuster (uschuster)                                                     }
{                                                                              }
{******************************************************************************}

unit HgIDEConst;

interface

resourcestring
  sMercurial = 'Mercurial';
  sPMMHgParent = 'Mercurial';
  sPMMCommit = 'Commit';
  sPMMLog = 'Show Log';
  sPMMRootDir = 'From Repository Root';
  sPMMProjectDir = 'From Project Directory';
  sPMMExpicitFiles = 'Files in this Project';
  sMenuOpenFromVersionControl = 'Open From Mercurial (Clone)';

  sCommit = 'Commit';
  sCommitCompleted = 'Commit completed at revision: %d';
  sLog = 'Log';
  sWorking = '-Working';

  SAuthor = 'Author: ';
  STime = 'Time: ';
  SComment = 'Comment: ';

  sCommitLoaded = 'A commit window is still open. Please close it if you wish to start a new commit.';
  sRetrievingFileRevision = 'Retrieving %s revision %d';
  sSavingFileRevision = 'Saving %s revision %d';

implementation

end.
