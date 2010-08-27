{
    BlockedFilesForm.pas
    Started: 27.08.2010
    Author: Asha'man (DarthYarius_0990@mail.ru)
    License: LGPL v3(?) /EULA

    Blocked files form declaration and implementation
}
unit BlockedFilesForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, Communication, Messages;

type

  { TBlockedFilesWnd }

  TBlockedFilesWnd = class(TForm)
    BtnTryAgain: TButton;
    BtnKillProcess: TButton;
    BtnForcedRemoval: TButton;
    BtnCancel: TButton;
    ButtonPanel: TPanel;
    ProgressPanel: TPanel;
    ProgressBar: TProgressBar;
    WarningMemo: TMemo;
    TopPanel: TPanel;
    ProcessTreeView: TTreeView;
    procedure FormCreate(Sender: TObject);
  private
    procedure HandleUSRMessage(var Message: TMessage); message WM_USBREMOVER;
  public
    { public declarations }
  end; 

var
  BlockedFilesWnd: TBlockedFilesWnd;

implementation

uses
    Mediator;

{BlockedFilesWnd}

procedure TBlockedFilesWnd.FormCreate(Sender: TObject);
begin
end;

//Handles window messages
procedure TBlockedFilesWnd.HandleUSRMessage(var Message: TMessage);
begin
    //pcontroller.HandleMessage(Message);
end; //TBlockedFilesWnd.HandleUSRMessage

initialization
  {$I BlockedFilesForm.lrs}

end.

