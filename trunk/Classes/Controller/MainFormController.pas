{
  Main form controller.
  Written by J.L.Blackrow
}
unit MainFormController;

interface

uses
  Main, Device, ComCtrls;

type
  TMainFormController = class(TObject)
  private
    fView: TMainForm;
    fViewOldShowingState: boolean;
    procedure AddTreeItems(Device: TDevice; Parent: TTreeNode);
    procedure AttachListeners;
    procedure ClearView;
    procedure GetMenuItem(Device: TDevice; var Info: string);
    //EVENT HANDLERS
    procedure RefreshView(Sender: TObject);
    procedure RemovalFailed(Sender: TObject);
    procedure RemovalSucceded(Sender: TObject);
  public
    constructor Create(Owner: TMainForm);
    destructor Destroy; override;
    procedure Hide;
    procedure Refresh;
    procedure RemoveDrive(Index: integer);
    procedure Show;
  end;

implementation

uses
  USBManager, Drive, Volume, Menus, CoolTrayIcon, Mediator;

//Shows main form in case it was in visible state
procedure TMainFormController.Show;
begin
  if fViewOldShowingState
  then begin
    fView.Show1Click(self);
  end; //main form's old state was visible
  fView.CoolTrayIcon.Enabled := true;
end; //Show

//Hides main form
procedure TMainFormController.Hide;
begin
  if fViewOldShowingState
  then begin
    fView.Show1Click(self);
  end;
  fView.CoolTrayIcon.Enabled := false;
end; //Hide

{EVENT HANDLERS}

procedure TMainFormController.RemovalFailed(Sender: TObject);
var
  deviceInfo: string;
begin
  GetMenuItem(Sender as TDevice, deviceInfo);
  fViewOldShowingState := fView.Showing;
  fView.CoolTrayIcon.ShowBalloonHint('Removal failed!', deviceInfo,
    bitError, 20);
  TMediator.GetInstance.ShowStoppedForm(Sender as TDevice);
end;

procedure TMainFormController.RemovalSucceded(Sender: TObject);
var
  deviceInfo: string;
begin
  GetMenuItem(Sender as TDevice, deviceInfo);
  fView.CoolTrayIcon.ShowBalloonHint('Removal succeeded!', deviceInfo,
    bitInfo, 20);
  TMediator.GetInstance.CloseStoppedFormExplicitly;
end;

//Refreshes view
procedure TMainFormController.RefreshView(Sender: TObject);
begin
  Refresh;
end; //RefreshView

{END EVENT HANDLERS}

//This procedure passes the parameters to model
procedure TMainFormController.RemoveDrive(Index: integer);
begin
  TUSBManager.GetManager.RemoveDrive(Index);
end; //RemoveDrive

//This procedure attaches the listeners to the form
procedure TMainFormController.AttachListeners;
begin
  TUSBManager.GetManager.ConfigurationChanged.Attach(self.RefreshView);
  Refresh;
  TUSBManager.GetManager.RemovalFailed.Attach(self.RemovalFailed);
  TUSBManager.GetManager.RemovalSucceeded.Attach(self.RemovalSucceded);
end; //AttachListeners

//This procedure adds new nodes to the TreeView component
procedure TMainFormController.AddTreeItems(Device: TDevice; Parent: TTreeNode);
var
  i: integer; //loop index
  treeNode: TTreeNode; //new tree node
  volumeString: String; //full name string
begin
  if Device is TVolume
  then begin
    volumeString := Device.MountPoints.Strings[0]+' ('+Device.FriendlyName+')';
    fView.TreeViewDevices.Items.AddChild(Parent, volumeString);
  end //device is a volume
  else begin
    volumeString := Device.Description + ' ' + Device.FriendlyName;
    treeNode := fView.TreeViewDevices.Items.AddChild(Parent, volumeString);
    for i := 0 to Device.Count-1 do
    begin
      AddTreeItems(Device.Children[i], treeNode);
    end; //loop
  end; //device has other type - we try to get its childrem
end; //AddTreeItems

//This procedure refreshes the view object
procedure TMainFormController.Refresh;
var
  manager: TUSBManager; //manager
  i: integer; //loop index
  itemName: string; //name of the new menu item
  menuItem: TMenuItem; //new menu item;
  device: TDevice; //device
begin
  ClearView;
  manager := TUSBManager.GetManager;
  //iterating over the devices
  for i := 0 to manager.GetDeviceCount-1 do
  begin
    //getting device info
    device := manager.GetDeviceInfo(i);
    AddTreeItems(device, nil);
    itemName := 'Remove ';
    GetMenuItem(device, itemName);
    //inserting new popup menu item
    menuItem := TMenuItem.Create(nil);
    //USB Device icon
    menuItem.ImageIndex := 1;
    menuItem.Caption := itemName;
    menuItem.OnClick := fView.DevicesMenuItemClick;
    fView.PopupMenuDevices.Items.Add(menuItem);
  end; //loop
end; //Refresh

//This procedure clears all controls
procedure TMainFormController.ClearView;
begin
  fView.TreeViewDevices.Items.Clear;
  fView.PopupMenuDevices.Items.Clear;
end; //ClearView

//This procedure gets menu item in the adequate form
procedure TMainFormController.GetMenuItem(Device: TDevice; var Info: string);
var
  i: integer; //loop index
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
end; //GetMenuItem

{CONSTRUCTOR}
constructor TMainFormController.Create(Owner: TMainForm);
begin
  inherited Create;
  fView := Owner;
  fViewOldShowingState := fView.Showing;
  AttachListeners;
  TMediator.GetInstance.AttachController(self);
end; //Create

{DESTRUCTOR}
destructor TMainFormController.Destroy;
begin
  fView := nil;
  inherited Destroy;
end; //Destroy

end.
