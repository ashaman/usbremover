unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, CoolTrayIcon, Menus, ComCtrls, ToolWin, ImgList, Device;

type
  TMainAppForm = class(TForm)
    trayIcon: TCoolTrayIcon;
    trayRightContextMenu: TPopupMenu;
    ShowMainWindow: TMenuItem;
    ViewSettings: TMenuItem;
    AboutApp: TMenuItem;
    ExitApp: TMenuItem;
    mainMenu: TMainMenu;
    Command1: TMenuItem;
    View1: TMenuItem;
    Help1: TMenuItem;
    Index: TMenuItem;
    About: TMenuItem;
    Refresh: TMenuItem;
    Settings: TMenuItem;
    Showtoolbar: TMenuItem;
    Stop: TMenuItem;
    Properties: TMenuItem;
    Exit: TMenuItem;
    mainToolbar: TToolBar;
    deviceTreeView: TTreeView;
    StartConsole: TMenuItem;
    trayLeftContextMenu: TPopupMenu;
    ImageList: TImageList;
    tbStop: TToolButton;
    tbProperties: TToolButton;
    tbSettings: TToolButton;
    tbConsole: TToolButton;
    procedure trayIconClick(Sender: TObject);
    procedure ShowtoolbarClick(Sender: TObject);
    procedure ExitClick(Sender: TObject);
    procedure ShowMainWindowClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure tbStopClick(Sender: TObject);
  private
    procedure AddTreeItems(Device: TDevice; Parent: TTreeNode);
    procedure ClearAll;
    procedure DrivesPopupMenuItemClick(Sender: TObject);
    procedure GetMenuItem(Device: TDevice; var Info: String);
    procedure RefreshInfo(Sender: TObject);
    procedure RemovalFailed(Sender: TObject);
    procedure RemovalSucceded(Sender: TObject);
  public
    { Public declarations }
  end;

var
  MainAppForm: TMainAppForm;

implementation

{$R *.dfm}

uses
  USBManager, ProcessManager,
  Drive, Volume,
  Stopped;

{IMPLEMENTED METHODS}

procedure TMainAppForm.RemovalSucceded(Sender: TObject);
var
  deviceInfo: string;
begin
  GetMenuItem(Sender as TDevice, deviceInfo);
  trayIcon.ShowBalloonHint('Removal succeeded!', deviceInfo, bitInfo, 10);
end;

procedure TMainAppForm.RemovalFailed(Sender: TObject);
var
  deviceInfo: string;
begin
  GetMenuItem(Sender as TDevice, deviceInfo);
  trayIcon.ShowBalloonHint('Removal failed!', deviceInfo, bitError, 10);
  if not ProcessesForm.Showing
  then begin
    TUSBManager.GetManager.RemovalFailed.Detach(Self.RemovalFailed);
    trayIcon.Enabled := false;
    ProcessesForm.BlockedDevice := Sender as TDevice;
    ProcessesForm.ShowModal;
    trayIcon.Enabled := true;
    TUSBManager.GetManager.RemovalFailed.Attach(Self.RemovalFailed);
  end;
end;

//Click handler for the popup menu
procedure TMainAppForm.DrivesPopupMenuItemClick(Sender: TObject);
var
  index: integer; //needed index
begin
  index := self.trayLeftContextMenu.Items.IndexOf(Sender as TMenuItem);
  TUSBManager.GetManager.RemoveDrive(index);
end;

procedure TMainAppForm.ClearAll;
begin
  self.trayLeftContextMenu.Items.Clear;
  self.deviceTreeView.Items.Clear;
end;

procedure TMainAppForm.RefreshInfo(Sender: TObject);
var
  manager: TUSBManager; //manager
  i: integer; //loop index
  itemName: string; //name of the new menu item
  menuItem: TMenuItem; //new menu item;
  device: TDevice; //device
begin
  ClearAll;
  manager := TUSBManager.GetManager;
  for i := 0 to manager.GetDeviceCount-1 do
  begin
    device := manager.GetDeviceInfo(i);
    AddTreeItems(device, nil);
    itemName := 'Remove ';
    GetMenuItem(device, itemName);
    //inserting new popup menu item
    menuItem := TMenuItem.Create(nil);
    menuItem.Caption := itemName;
    menuItem.OnClick := self.DrivesPopupMenuItemClick;
    trayLeftContextMenu.Items.Add(menuItem);
  end; //loop
end;

procedure TMainAppForm.GetMenuItem(Device: TDevice; var Info: String);
var
  i: integer;
begin
  if Device is TDiskDrive
  then begin
    //getting volumes information
    Info := Info + Device.FriendlyName + '; volumes: ';
    for i := 0 to Device.Count-1 do
    begin
      Info := Info + Device.Children[i].MountPoints[0]+' ('+
        Device.Children[i].FriendlyName+')';
      if i <> Device.Count-1
      then begin
        Info := Info + ', ';
      end; //check if this is the last one
    end; //loop
  end //then - device is disk, getting its info
  else begin
    for i := 0 to Device.Count-1 do
    begin
      GetMenuItem(Device.Children[i], Info);
      if i <> Device.Count-1
      then begin
        Info := Info + ', ';
      end; //check if this is the last one
    end; //loop
  end; //else
end;

procedure TMainAppForm.AddTreeItems(Device: TDevice; Parent: TTreeNode);
var
  i: integer; //loop index
  treeNode: TTreeNode; //new tree node
  volumeString: String; //full name string
begin
  if Device is TVolume
  then begin
    volumeString := Device.MountPoints.Strings[0]+' ('+Device.FriendlyName+')';
    deviceTreeView.Items.AddChild(Parent, volumeString);
  end //device is a volume
  else begin
    volumeString := Device.Description + ' ' + Device.FriendlyName;
    treeNode := deviceTreeView.Items.AddChild(Parent, volumeString);
    for i := 0 to Device.Count-1 do
    begin
      AddTreeItems(Device.Children[i], treeNode);
    end; //loop
  end; //device has other type - we try to get its childrem
end;

{END IMPLEMENTED METHODS}

procedure TMainAppForm.trayIconClick(Sender: TObject);
begin
  trayLeftContextMenu.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
end;

procedure TMainAppForm.ShowtoolbarClick(Sender: TObject);
begin
  mainToolbar.Visible := not mainToolbar.Visible;
  Showtoolbar.Checked := not Showtoolbar.Checked;
end;

procedure TMainAppForm.ExitClick(Sender: TObject);
begin
  Close;
end;

procedure TMainAppForm.ShowMainWindowClick(Sender: TObject);
begin
  self.Show;
end;

procedure TMainAppForm.FormCreate(Sender: TObject);
begin
  TUSBManager.GetManager.NotifyEvent.AttachWithNotification(self.RefreshInfo);
  TUSBManager.GetManager.RemovalFailed.Attach(self.RemovalFailed);
  TUSBManager.GetManager.RemovalSucceeded.Attach(self.RemovalSucceded);
end;

procedure TMainAppForm.tbStopClick(Sender: TObject);
begin
  if not Assigned(deviceTreeView.Selected.Parent)
  then begin
    TUSBManager.GetManager.RemoveDrive(deviceTreeView.Selected.Index);
  end;
end;

end.
