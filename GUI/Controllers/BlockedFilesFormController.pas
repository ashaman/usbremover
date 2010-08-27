{
    BlockedFilesFormController.pas
    Started: 27.08.2010
    Author: Asha'man (DarthYarius_0990@mail.ru)
    License: LGPL v3(?) /EULA

    Blocked files form controller declaration and implementation
}
unit BlockedFilesFormController;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BlockedFilesForm, Connector,
  Windows, Communication;

type
    TBlockedFilesFormController = class(TObject)
    private
        fPipeConnector: TPipeConnector; //pipe connector
        fView: TBlockedFilesWnd; //blocked files form

        function GetFormHandle: HWND; //gets form handle
        procedure OnSearchProgress(Sender: TObject); //progress notification
    public
        constructor Create(connector: TPipeConnector;
            view: TBlockedFilesWnd); //constructor
        destructor Destroy; override; //destructor

        property FormHandle: HWND read GetFormHandle; //form handle
        procedure HandleMessage(var Message: TMessage); //message handler
        procedure HideForm; //hides the form
        procedure ShowForm; //shows the form
    end;

implementation

{EVENT HANDLERS}

//handles the messages
procedure TBlockedFilesFormController.HandleMessage(var Message: TMessage);
var
    Code: DWORD; //message code
begin
    Code := DWORD(Message.wParam);
    case Code of
        //lockers info is ready
        WM_LOCK_INFO:
        begin
            //TODO: implement
        end;
        //search in progress
        WM_PERCENTAGE:
        begin
            Self.fView.ProgressBar.Position := BYTE(Message.lParam);
        end;
        //lock form
        WM_SHOW_LOCK_FORM:
        begin
            Self.fView.Show;
        end;
    end;
end; //TBlockedFilesFormController.HandleMessage

//handles the progress event
procedure TBlockedFilesFormController.OnSearchProgress(Sender: TObject);
var
    percentage: byte; //percentage of operation execution
begin
    percentage := Byte(Pointer(Sender)^);
    SendNotifyMessageW(Self.fView.Handle, WM_USBREMOVER,
        WPARAM(WM_PERCENTAGE), LPARAM(percentage));
end; //TBlockedFilesFormController.OnSearchProgress

{END EVENT HANDLERS}

{
    Purpose:
    Parameters:
    Return value:
}
function TBlockedFilesFormController.GetFormHandle: HWND;
begin
    Result := Self.fView.Handle;
end; //TBlockedFilesFormController.GetFormHandle

{
    Purpose:
        Hides the form
    Parameters:
        None
    Return value:
        None
}
procedure TBlockedFilesFormController.HideForm;
begin
    Self.fView.Hide;
end; //TBlockedFilesFormController.HideForm

{
    Purpose:
        Shows the form
    Parameters:
        None
    Return value:
        None
}
procedure TBlockedFilesFormController.ShowForm;
begin
    Self.fView.Show;
end; //TBlockedFilesFormController.ShowForm

{
    Purpose:
        Constructor
    Parameters:
        None
}
constructor TBlockedFilesFormController.Create(connector: TPipeConnector;
    view: TBlockedFilesWnd);
begin
    //saving the connector pointer
    Self.fPipeConnector := connector;
    //saving the view pointer
    Self.fView := view;
    //assigning the event handler
    Self.fPipeConnector.OnSearchProgress := @Self.OnSearchProgress;
end; //constructror

{
    Purpose:
        Destructor
}
destructor TBlockedFilesFormController.Destroy;
begin
    inherited Destroy;
end; //destructor

end.

