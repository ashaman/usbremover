{
    MainFormController.pas
    Started: 22.08.2010
    Author: Asha'man (DarthYarius_0990@mail.ru)
    License: LGPL v3(?) /EULA

    Main form controller declaration and implementation
}

{
    WARNING!!!
    Here VCL components are accessed from another thread.
    They are NOT thread-safe!!! Sometimes the refresh from the
    background thread works fine, sometimes it causes the access
    violation. I found no better way than to use SuspendThread-ResumeThread
    to synchronize with the view

    P.S. APC doesn't work with VCL :'( What a pity...
    Maybe it's better to send a message to the main window?
}
unit MainFormController;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Connector, Menus, ExtCtrls, MainForm, Communication,
  Device, ComCtrls, Forms;

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
            index: integer); //adds items

    public
        constructor Create(mainWnd: TMainWnd); //constructor
        destructor Destroy; override; //destructor

        procedure Refresh; //refreshes view state
        procedure RemoveDrive(Data: Pointer); //removes the device
        procedure SwitchToolBar; //switches the toolbar
    end;

implementation

{EVENT HANDLERS}

//handles the refresh finish
procedure TMainFormController.OnRefreshFinished(Sender: TObject);
var
    i: integer; //loop index
begin
    try
        //TODO: add devices to the tree + to the menus (popup)
        {
            WARNING!!!
            HERE main VCL thread is SUSPENDED! No messages
            from the system and other windows can be handled!!!
            In order to handle messages, we call ProcessMessages
        }
        SuspendThread(self.fPipeConnector.MainThreadHandle);
        Self.fMainWnd.DeviceTreeView.BeginUpdate;
        Self.fMainWnd.DeviceTreeView.Items.BeginUpdate;
        Self.fMainWnd.DeviceTreeView.Items.Clear;
        Application.ProcessMessages;
        //adding devices to root
        for i := 0 to Self.fPipeConnector.DeviceCount-1 do
        begin
            Self.AddTreeItems(Self.fPipeConnector.Devices[i], nil, 0);
            Application.ProcessMessages;
        end;
    finally
        {
            Resuming thread BEFORE any updates
        }
        ResumeThread(self.fPipeConnector.MainThreadHandle);
        Self.fMainWnd.DeviceTreeView.FullExpand;
        Self.fMainWnd.DeviceTreeView.Items.EndUpdate;
        Self.fMainWnd.DeviceTreeView.EndUpdate;
    end;
end; //TMainFormController.OnRefreshFinished

//handles the device removal success and shows the info in the tray
procedure TMainFormController.OnRemovalSucceeded(Sender: TObject);
begin
    try
        //removal succeeded - we set all the tray icon parameters
        //suspending the main thread
        SuspendThread(Self.fPipeConnector.MainThreadHandle);
        //setting balloon title - "Removal succeeded"
        Self.fMainWnd.TrayIcon.BalloonTitle := BalloonTitle[0];
        //setting message
        Self.fMainWnd.TrayIcon.BalloonHint := 'Device ' +
            Self.fEJDevice.Name + ' was ejected successfully!';
        //setting balloon timeout
        Self.fMainWnd.TrayIcon.BalloonTimeout := BalloonTimeout;
        //setting balloon flags (bfInformation - little blue "i")
        Self.fMainWnd.TrayIcon.BalloonFlags := bfInfo;
        //processing messages
        Application.ProcessMessages;
    finally
        //resuming main thread
        ResumeThread(Self.fPipeConnector.MainThreadHandle);
        //showing the hint
        Self.fMainWnd.TrayIcon.ShowBalloonHint;
    end;
end; //TMainFormController.OnRemovalSucceeded

//handles the device removal failure and shows the handle search form
procedure TMainFormController.OnRemovalFailed(Sender: TObject);
begin
    try
        //removal failed - we set all the tray icon parameters
        //suspending the main thread
        SuspendThread(Self.fPipeConnector.MainThreadHandle);
        //setting balloon header - "Removal failed!"
        Self.fMainWnd.TrayIcon.BalloonTitle := BalloonTitle[1];
        //setting balloon text
        Self.fMainWnd.TrayIcon.BalloonHint := 'Device ' +
            Self.fEJDevice.Name + ': removal failed!';
        //setting balloon timeout
        Self.fMainWnd.TrayIcon.BalloonTimeout := BalloonTimeout;
        //setting balloon flags - little "x" in a red circle
        Self.fMainWnd.TrayIcon.BalloonFlags := bfError;
        //processing messages
        Application.ProcessMessages;
        //TODO: add progress form!!!
        //It should be called from another thread... very bad...
        //maybe it's better to use messages?
    finally
        //resuming main thread
        ResumeThread(Self.fPipeConnector.MainThreadHandle);
        //showing the balloon
        Self.fMainWnd.TrayIcon.ShowBalloonHint;
    end;
end; //TMainFormController.OnRemovalFailed

{END EVENT HANDLERS}

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
    deviceName := Device.Name + ' - ' + Device.Description;
    //adding a new tree node
    treeNode := Self.fMainWnd.DeviceTreeView.Items.AddChild(Parent, deviceName);
    //attaching the data
    treeNode.Data := Device;
    //image indexes - common and selected
    treeNode.ImageIndex := index;
    treeNode.SelectedIndex := index;
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
        mainWnd - main window pointer
}
constructor TMainFormController.Create(mainWnd: TMainWnd);
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

