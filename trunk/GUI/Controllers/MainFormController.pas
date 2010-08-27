{
    MainFormController.pas
    Started: 22.08.2010
    Author: Asha'man (DarthYarius_0990@mail.ru)
    License: LGPL v3(?) /EULA

    Main form controller declaration and implementation
}

unit MainFormController;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Connector, Menus, ExtCtrls, MainForm, BlockedFilesForm,
  Communication,Device, ComCtrls, Forms, Windows, BlockedFilesFormController;

const
    { ToolBar state labels }
    MenuToolBarLabels: array [0..1] of string =
    (
        'Show toolbar',
        'Hide toolbar'
    );
    BalloonTitle: array [0..1] of string =
    (
        'Device ejected!',
        'Device removal failed!'
    );


    BalloonTimeout = 5;

type
    {
        Description:
            Main form controller for complex operations
    }
    TMainFormController = class(TObject)
    private
        fMainWnd: TMainWnd; //view
        fPipeConnector: TPipeConnector; //model
        fToolBarState: byte; //toolbar state index
        fEJDevice: TDevice; //device being ejected

        //event handlers
        procedure OnRefreshFinished(Sender: TObject); //refresh finished
        procedure OnRemovalFailed(Sender: TObject); //removal failed handler
        procedure OnRemovalSucceeded(Sender: TObject); //removal succeded handler

        procedure AddTreeItems(Device: TDevice; Parent: TTreeNode;
            index: integer); //adds items to the TreeView
        procedure FillView; //fills the view with the data
        procedure ShowTrayIcon(TitleIndex: integer; Hint: String;
            Flags: TBalloonFlags); //fills the tray icon data and shows it

    public
        constructor Create(connector: TPipeConnector;
            mainWnd: TMainWnd); //constructor
        destructor Destroy; override; //destructor

        procedure Refresh; //refreshes view state
        procedure RemoveDrive(Data: Pointer); //removes the device
        procedure SwitchToolBar; //switches the toolbar
        procedure ResolveMessages(var Message: TMessage); //message handler
    end;

var
    fPipeConnector: TPipeConnector;

implementation

{EVENT HANDLERS}

//handles window messages
procedure TMainFormController.ResolveMessages(var Message: TMessage);
var
    Code: DWORD;
begin
    //getting the code of message
    Code := DWORD(Message.wParam);
    case Code of
        //refresh finished
        WM_REFRESH_ANSWER:
        begin
            Self.FillView;
        end;
        //ejected successfully
        WM_EJECT_ACCEPT:
        begin
            Self.ShowTrayIcon(1, 'Device "' + Self.fEJDevice.Name +
                '" ejected successfully!', bfInfo);
        end;
        //ejection failed
        WM_EJECT_REJECT:
        begin
            Self.ShowTrayIcon(1, 'Device "' + Self.fEJDevice.Name +
                '" ejection failed!', bfError);
            BlockedFilesWnd.Show;
            //SendNotifyMessageW(blockController.FormHandle,
            //    WM_USBREMOVER, WPARAM(WM_SHOW_LOCK_FORM) , LPARAM(nil));
        end;
    end;
end; //TMainFormController.ResolveMessages

//handles the refresh finish
procedure TMainFormController.OnRefreshFinished(Sender: TObject);
begin
    //sending message to the controls
    SendNotifyMessageW(Self.fMainWnd.Handle, WM_USBREMOVER,
        WPARAM(WM_REFRESH_ANSWER), LPARAM(nil));
end; //TMainFormController.OnRefreshFinished

//handles the device removal success and shows the info in the tray
procedure TMainFormController.OnRemovalSucceeded(Sender: TObject);
begin
    //sending message to the controls
    SendNotifyMessageW(Self.fMainWnd.Handle, WM_USBREMOVER,
        WPARAM(WM_EJECT_ACCEPT), LPARAM(nil));
end; //TMainFormController.OnRemovalSucceeded

//handles the device removal failure and shows the handle search form
procedure TMainFormController.OnRemovalFailed(Sender: TObject);
begin
    //sending message to the controls
    SendNotifyMessageW(Self.fMainWnd.Handle, WM_USBREMOVER,
        WPARAM(WM_EJECT_REJECT), LPARAM(nil));
end; //TMainFormController.OnRemovalFailed

{END EVENT HANDLERS}

{
    Purpose:
        Fills the tray icon with the data
    Parameters:
        titleIndex - index of the balloon title in the array
        hint - message shown on the balloon hint
        flags - balloon flags
    Return value:
        None
}
procedure TMainFormController.ShowTrayIcon(TitleIndex: integer;
    Hint: String; Flags: TBalloonFlags);
begin
    try
        //setting balloon title
        Self.fMainWnd.TrayIcon.BalloonTitle := BalloonTitle[TitleIndex];
        //setting balloon text
        Self.fMainWnd.TrayIcon.BalloonHint := Hint;
        //setting balloon timeout
        Self.fMainWnd.TrayIcon.BalloonTimeout := BalloonTimeout;
        //setting balloon flags
        Self.fMainWnd.TrayIcon.BalloonFlags := Flags;
    finally
        //showing the balloon
        Self.fMainWnd.TrayIcon.ShowBalloonHint;
    end;
end; //TMainFormController.ShowTrayIcon

{
    Purpose:
        Fills the view with the information from the connector
    Parameters:
        None
    Return value:
        None
}
procedure TMainFormController.FillView;
var
    i: integer; //loop index
begin
    try
        //starting the device's list update process
        Self.fMainWnd.DeviceTreeView.BeginUpdate;
        Self.fMainWnd.DeviceTreeView.Items.BeginUpdate;
        Self.fMainWnd.DeviceTreeView.Items.Clear;
        //adding devices to root
        for i := 0 to Self.fPipeConnector.DeviceCount-1 do
        begin
            Self.AddTreeItems(Self.fPipeConnector.Devices[i], nil, 0);
        end;
    finally
        //finishing the update process
        Self.fMainWnd.DeviceTreeView.FullExpand;
        Self.fMainWnd.DeviceTreeView.Items.EndUpdate;
        Self.fMainWnd.DeviceTreeView.EndUpdate;
    end;
end; //TMainFormController.FillView

{
    Purpose:
        Adds all the devices to the TreeView
    Parameters:
        device - device to add
        parent - parent node to attach hierarchy to
        index - picture index
    Return value:
        None
}
procedure TMainFormController.AddTreeItems(Device: TDevice;
    Parent: TTreeNode; index: integer);
var
    i: integer; //loop index
    treeNode: TTreeNode; //new tree node
    deviceName: WideString; //full device name
begin
    //TODO: add images to nodes + add mount points if there are any
    //formatting device name
    deviceName := WideFormat('%0:S - %1:S',[Device.Name, Device.Description]);
    //adding a new tree node
    treeNode := Self.fMainWnd.DeviceTreeView.Items.AddChild(Parent,
        WideToUTF8String(deviceName));
    //attaching the data
    treeNode.Data := Device;
    //image indexes - common and selected
    treeNode.ImageIndex := index;
    treeNode.SelectedIndex := index;
    //adding mount points (if there are any)
    if Device.MountPointCount > 0
    then begin
        for i := 0 to Device.MountPointCount-1 do
        begin
            Self.fMainWnd.DeviceTreeView.Items.AddChild(treeNode,
                Device.MountPoints[i]);
        end;
    end;
    //performing the same operations on children
    for i := 0 to Device.ChildCount-1 do
    begin
        AddTreeItems(Device.Children[i], treeNode, index+1);
    end;
end; //TMainFormController.AddTreeItems

{
    Purpose:
        Sends the information about the drive removal
    Parameters:
        data - device pointer
    Return value:
        None
}
procedure TMainFormController.RemoveDrive(Data: Pointer);
begin
    //device pointer conversion
    Self.fEJDevice := TDevice(Data);
    //ejecting the device
    Self.fPipeConnector.EjectDevice(fEJDevice.Index);
end; //TMainFormController.RemoveDrive

{
    Purpose:
        Switches the toolbar's state between "Shown" and "Hidden"
    Parameters:
        None
    Return value:
        None
}
procedure TMainFormController.SwitchToolBar;
begin
    //toolbar is shown; index is greater than zero
    if (Self.fToolBarState > 0)
    then begin
        //hiding the toolbar
        Self.fMainWnd.ToolBar.Hide;
        //decreasing the index
        Dec(Self.fToolBarState);
    end
    else begin
        //showing the toolbar
        Self.fMainWnd.ToolBar.Show;
        //increasing the index
        Inc(Self.fToolBarState);
    end;
    //setting the label in the menu
    Self.fMainWnd.MIViewHideToolbar.Caption :=
        MenuToolBarLabels[Self.fToolBarState];
end; //TMainFormController.SwitchToolBar

{
    Purpose:
        Refreshes the view by getting the device information
        from the service (throug the TPipeConenctor class)
    Parameters:
        None
    Return value:
        None
}
procedure TMainFormController.Refresh;
begin
    //refreshing the device list
    fPipeConnector.Refresh;
end; //TMainFormController.Refresh

{
    Purpose:
        Initializes a new instance of controller and passes
        the main form as an argument
    Parameters:
        connector - pipe connector
        mainWnd - main window pointer
}
constructor TMainFormController.Create(connector: TPipeConnector;
    mainWnd: TMainWnd);
begin
    inherited Create;
    Self.fMainWnd := mainWnd;
    //creating the connector
    Self.fPipeConnector := TPipeConnector.Create;
    //toolbar is shown
    Self.fToolBarState := 1;
    //attaching listeners
    Self.fPipeConnector.OnRefreshFinished := @Self.OnRefreshFinished;
    Self.fPipeConnector.OnRemovalFailed := @Self.OnRemovalFailed;
    Self.fPipeConnector.OnRemovalSucceeded := @Self.OnRemovalSucceeded;
end; //constructor

{
    Purpose:
        Destructor
}
destructor TMainFormController.Destroy;
begin
    //destroying the connector object
    Self.fPipeConnector.Destroy;
    inherited Destroy;
end; //destructor

end.

