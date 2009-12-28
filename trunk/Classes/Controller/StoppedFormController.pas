{
  UI CONTROLLER.
  DO NOT MODIFY.
}
unit StoppedFormController;

interface

uses
  Stopped, Device;

type
  TStoppedFormController = class(TObject)
  private
    fView: TStoppedForm; //form instance
    fDevice: TDevice; //blocked device
    procedure ProgressCallback(Sender: TObject); //PBar Callback
  public
    constructor Create(Owner: TStoppedForm);
    destructor Destroy; override;
    procedure Close;
    procedure ForcedClose; //closes the form explicitly
    procedure ForcedRemoval; //forces the removal
    procedure Refresh;
    procedure ShowForm;
    procedure TryEject; //tries to eject device again
    property Device: TDevice read fDevice write fDevice;
  end;

implementation

uses
  Mediator, USBManager, ProcessManager, Process, Classes,
  ComCtrls, Windows;

{EVENT HANDLERS}

//Fills the progress bar
procedure TStoppedFormController.ProgressCallback(Sender: TObject);
begin
  fView.ProgressBar.Position := Integer(Sender);
end; //ProgressCallback

{END EVENT HANDLERS}

//Forced removal of the flash drive
procedure TStoppedFormController.ForcedRemoval;
var
  pManager: TProcessManager; //temp
  i: integer; //loop index
  process: TProcess; //temp
begin
  if fDevice <> nil
  then begin
    pManager := TProcessManager.GetInstance;
    for i := 0 to pManager.BlockerProcesses.Count-1 do
    begin
      process := TProcess(pManager.BlockerProcesses.Items[i]);
      pManager.KillProcess(process.Handle);
    end; //loop
    Sleep(1000);
    TUSBManager.GetManager.RemoveDrive(fDevice);
  end;
end; //ForcedRemoval

//Tries to eject the device again
procedure TStoppedFormController.TryEject;
begin
  if Assigned(fDevice)
  then begin
    TUSBManager.GetManager.RemoveDrive(fDevice);
  end;
end; //TryEject

//Refreshes information on the form
procedure TStoppedFormController.Refresh;
var
  lockers: TList; //locker list
  process: TProcess; //temp
  i, j: integer; //loop indexes
  treeNode: TTreeNode; //temp
begin
  fView.TreeViewProcesses.Items.Clear;
  lockers := TProcessManager.GetInstance.GetLockers(fDevice.MountPoints,
    self.ProgressCallback);
  fView.ProgressBar.Position := 0;
  for i := 0 to lockers.Count-1 do
  begin
    process := TProcess(lockers.Items[i]);
    treeNode := fView.TreeViewProcesses.Items.AddChild(nil, process.Name);
    for j := 0 to process.OpenedFiles.Count-1 do
    begin
      fView.TreeViewProcesses.Items.AddChild(treeNode,
        process.OpenedFiles.Strings[j]);
    end; //loop-j
  end; //loop-i
  //lockers.Free;
end; //Refresh

//Closes the form
procedure TStoppedFormController.Close;
begin
  TMediator.GetInstance.ShowMainForm;
end; //Close

//This procedure closes the form in case the device that is blocked was detached
procedure TStoppedFormController.ForcedClose;
begin
  fView.Close;
  Close;
end; //ForcedClose

//This procedure shows form modally
procedure TStoppedFormController.ShowForm;
begin
  if not fView.Showing
  then begin
    fView.Show;
  end; //was closed
  Refresh;
end; //ShowForm

{CONSTRUCTOR}
constructor TStoppedFormController.Create(Owner: TStoppedForm);
begin
  inherited Create;
  fView := Owner;
  TMediator.GetInstance.AttachController(self);
end; //Create

{DESTRUCTOR}
destructor TStoppedFormController.Destroy;
begin
  fView := nil;
  inherited Destroy;
end; //Destroy

end.
