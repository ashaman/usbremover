unit MainFormController;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Connector, Menus, ExtCtrls, MainForm, Communication,
  Device, ComCtrls;

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
        fDeviceIndex: DEVINDEX; //ejected device index

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
        procedure RemoveDrive (Level: integer; Index: integer); //removes the device
        procedure SwitchToolBar; //switches the toolbar
    end;

implementation

{EVENT HANDLERS}

//handles the refresh finish
procedure TMainFormController.OnRefreshFinished(Sender: TObject);
var
    i: integer; //loop index
begin
    //TODO: works VERY VERY BAD
    //DOESN'T SUPPORT RUSSIAN ENCODING
    Self.fMainWnd.DeviceTreeView.BeginUpdate;
    Self.fMainWnd.DeviceTreeView.Items.BeginUpdate;
    Self.fMainWnd.DeviceTreeView.Items.Clear;
    //adding devices to root
    for i := 0 to Self.fPipeConnector.DeviceCount-1 do
    begin
        AddTreeItems(Self.fPipeConnector.Devices[i], nil, 0);
    end;
    Self.fMainWnd.DeviceTreeView.FullExpand;
    Self.fMainWnd.DeviceTreeView.Items.EndUpdate;
    Self.fMainWnd.DeviceTreeView.EndUpdate;
    //TODO: add devices to the tree + to the menus (popup)
    //TODO: perform full view clean
end; //TMainFormController.OnRefreshFinished

//handles the device removal success and shows the info in the tray
procedure TMainFormController.OnRemovalSucceeded(Sender: TObject);
begin
    //removal succeeded - we set all the tray icon parameters
    //TODO: do it better
    Self.fMainWnd.TrayIcon.BalloonTitle := BalloonTitle[0];
    Self.fMainWnd.TrayIcon.BalloonHint := 'Device removal succeeded!';
    Self.fMainWnd.TrayIcon.BalloonTimeout := BalloonTimeout;
    Self.fMainWnd.TrayIcon.BalloonFlags := bfInfo;
    Self.fMainWnd.TrayIcon.ShowBalloonHint;
    //TODO: add what device was ejected (how? - save state in the controller?)
end; //TMainFormController.OnRemovalSucceeded

//handles the device removal failure and shows the handle search form
procedure TMainFormController.OnRemovalFailed(Sender: TObject);
begin
    //removal failed - we set all the tray icon parameters
    //TODO: do it better
    Self.fMainWnd.TrayIcon.BalloonTitle := BalloonTitle[1];
    Self.fMainWnd.TrayIcon.BalloonHint := 'Device removal failed!';
    Self.fMainWnd.TrayIcon.BalloonTimeout := BalloonTimeout;
    Self.fMainWnd.TrayIcon.BalloonFlags := bfError;
    Self.fMainWnd.TrayIcon.ShowBalloonHint;
    //TODO: add progress form!!!
    //TODO: add what device was ejected (how? - save state in the controller?)
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
    deviceName: string; //full device name
begin
    //some problems with threading - WHERE?
    //POSSIBLY: double entrance to critical section
    //maybe use QueueUserAPC?
    //TODO: add images to nodes + add mount points if there are any
    //formatting device name
    deviceName := Device.Name + ' - ' + Device.Description;
    //adding a new tree node
    treeNode := Self.fMainWnd.DeviceTreeView.Items.AddChild(Parent, deviceName);
    treeNode.ImageIndex := index;
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
        level - device level
        index - device index on the level
    Return value:
        None
}
procedure TMainFormController.RemoveDrive(Level: integer; Index: integer);
begin
    //TODO: check the removal indexes
    Self.fDeviceIndex.dwDeviceLevel := level;
    Self.fDeviceIndex.dwDeviceNumber := index;
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

