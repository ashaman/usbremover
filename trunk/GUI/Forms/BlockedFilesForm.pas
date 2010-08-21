unit BlockedFilesForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs; 

type
  TBlockedFilesWnd = class(TForm)
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  BlockedFilesWnd: TBlockedFilesWnd;

implementation

initialization
  {$I BlockedFilesForm.lrs}

end.

