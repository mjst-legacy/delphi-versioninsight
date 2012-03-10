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
{ The Original Code is GitIDEConst.pas.                                        }
{                                                                              }
{ The Initial Developer of the Original Code is Uwe Schuster.                  }
{ Portions created by Uwe Schuster are Copyright © 2011 Uwe Schuster. All      }
{ Rights Reserved.                                                             }
{                                                                              }
{ Contributors:                                                                }
{ Uwe Schuster (uschuster)                                                     }
{                                                                              }
{******************************************************************************}

unit GitIDEConst;

interface

resourcestring
  sGit = 'Git';
  sPMMGitParent = 'Git';
  sPMMCommit = 'Commit';
  sPMMLog = 'Show Log';
  sPMMRootDir = 'From Repository Root';
  sPMMProjectDir = 'From Project Directory';
  sPMMExpicitFiles = 'Files in this Project';
  sPMMRevert = 'Revert';
  sMenuOpenFromVersionControl = 'Open From Git (Clone)';

  sCommit = 'Commit';
  sCommitCompleted = 'Commit completed at: %s';
  sLog = 'Log';
  sWorking = '-Working';

  SAuthor = 'Author: ';
  STime = 'Time: ';
  SComment = 'Comment: ';

  sCommitLoaded = 'A commit window is still open. Please close it if you wish to start a new commit.';
  sRevertedFile = 'Reverted: %s';
  sRetrievingFileRevision = 'Retrieving %s revision %s';
  sSavingFileRevision = 'Saving %s revision %s';

implementation

end.
